// [[Rcpp::plugins(cpp17)]]
// All state handlers: dispatch_state() dispatches on StateType enum.
// Returns next state name, "" for terminal, current name to stay.
#include "simulation.h"
#include "condition_eval.h"
#include <cmath>

// ── Quantity sampling (Delay, Observation range, Symptom range, VitalSign range) ──
static double sample_quantity(const QuantityDef& q, std::mt19937& rng) {
    switch (q.kind) {
    case QuantityDef::Kind::Exact:
        return q.low;
    case QuantityDef::Kind::Range: {
        if (q.low >= q.high) return q.low;
        std::uniform_real_distribution<double> d(q.low, q.high);
        return d(rng);
    }
    default: return q.low;
    }
}

// ── Transition helper ──────────────────────────────────────────────────────────
static std::string next_state(const CppState& state, PersonRecord& rec, std::mt19937& rng) {
    return resolve_transition_cpp(state.transition, rec, rng);
}

// ── FLOW STATES ───────────────────────────────────────────────────────────────

static std::string handle_initial(const CppState& state, PersonRecord& rec, std::mt19937& rng) {
    rec.visited.insert(state.visited_key);
    return next_state(state, rec, rng);
}

static std::string handle_simple(const CppState& state, PersonRecord& rec, std::mt19937& rng) {
    rec.visited.insert(state.visited_key);
    return next_state(state, rec, rng);
}

static std::string handle_terminal(const CppState&, PersonRecord&, std::mt19937&) {
    return "";  // signals terminal
}

static std::string handle_delay(const CppState& state, PersonRecord& rec, std::mt19937& rng) {
    const std::string& key = state.delay_key;
    auto it = rec.timers.find(key);

    if (it == rec.timers.end()) {
        double dur = 0.0;
        switch (state.delay.kind) {
        case CppState::DelayDef::Kind::Range: {
            double lo = state.delay.low_secs, hi = state.delay.high_secs;
            if (lo >= hi) dur = lo;
            else { std::uniform_real_distribution<double> d(lo, hi); dur = d(rng); }
            break;
        }
        case CppState::DelayDef::Kind::Gaussian: {
            double mean = state.delay.low_secs, stddev = state.delay.high_secs;
            if (stddev <= 0.0) { dur = std::max(0.0, mean); break; }
            std::normal_distribution<double> nd(mean, stddev);
            dur = std::max(0.0, nd(rng));
            break;
        }
        case CppState::DelayDef::Kind::Exponential: {
            double mean = state.delay.low_secs;
            if (mean <= 0.0) { dur = 0.0; break; }
            std::exponential_distribution<double> ed(1.0 / mean);
            dur = ed(rng);
            break;
        }
        default:
            dur = state.delay.low_secs;
        }
        rec.timers[key] = rec.t_num + dur;
        return state.name;  // stay
    }
    if (rec.t_num < it->second) return state.name;  // still waiting
    rec.timers.erase(it);
    rec.visited.insert(state.visited_key);
    return next_state(state, rec, rng);
}

static std::string handle_guard(const CppState& state, PersonRecord& rec, std::mt19937& rng) {
    if (state.guard_cond && !evaluate_condition_cpp(state.guard_cond.get(), rec))
        return state.name;  // blocked
    rec.visited.insert(state.visited_key);
    return next_state(state, rec, rng);
}

static std::string handle_death(const CppState& state, PersonRecord& rec, std::mt19937& rng) {
    rec.is_alive = false;
    rec.visited.insert(state.visited_key);
    return next_state(state, rec, rng);
}

static std::string handle_set_attribute(const CppState& state, PersonRecord& rec, std::mt19937& rng) {
    if (!state.attr_name.empty()) {
        rec.attributes[state.attr_name] = state.attr_value;
    }
    rec.visited.insert(state.visited_key);
    return next_state(state, rec, rng);
}

static std::string handle_counter(const CppState& state, PersonRecord& rec, std::mt19937& rng) {
    if (!state.attr_name.empty()) {
        // Null/missing attribute → default 0 (matches R: person@attributes[[attr]] %||% 0)
        double cur = get_attr_double(rec, state.attr_name, 0.0);
        if (attr_is_null(rec.get_attr(state.attr_name))) cur = 0.0;
        rec.attributes[state.attr_name] = state.counter_increment
            ? cur + state.counter_amount
            : cur - state.counter_amount;
    }
    rec.visited.insert(state.visited_key);
    return next_state(state, rec, rng);
}

static std::string handle_call_submodule(const CppState& state, PersonRecord& rec, std::mt19937& rng) {
    // Register submodule call — advance_module_cpp handles it after dispatch
    if (!state.submodule_name.empty())
        rec.submodule_calls[state.call_key] = state.submodule_name;
    rec.visited.insert(state.visited_key);
    return next_state(state, rec, rng);
}

// ── CLINICAL STATES ────────────────────────────────────────────────────────────

static std::string handle_encounter(const CppState& state, PersonRecord& rec, std::mt19937& rng) {
    if (state.is_wellness) {
        auto wit = rec.timers.find(state.wellness_key);
        if (wit != rec.timers.end() && wit->second >= rec.t_num)
            return state.name;  // already ran wellness this timestep
        // Schedule next wellness 1 year out (matching R: rec[[wellness_key]] <- t_num + 365.25*86400)
        rec.timers[state.wellness_key] = rec.t_num + 365.25 * 86400.0;
    }

    CppEncounter enc;
    enc.start           = rec.t_num;
    enc.end             = rec.t_num;
    enc.encounter_class = state.encounter_class;
    enc.codes           = state.codes;
    enc.is_wellness     = state.is_wellness;
    enc.id              = "enc_" + std::to_string(rec.encounters.size());
    rec.encounters.push_back(std::move(enc));
    rec.current_encounter_id = rec.encounters.back().id;

    rec.visited.insert(state.visited_key);
    return next_state(state, rec, rng);
}

static std::string handle_encounter_end(const CppState& state, PersonRecord& rec, std::mt19937& rng) {
    // Close the current encounter: set end time
    if (!rec.current_encounter_id.empty() && !rec.encounters.empty()) {
        rec.encounters.back().end = rec.t_num;
        rec.current_encounter_id.clear();
    }
    rec.visited.insert(state.visited_key);
    return next_state(state, rec, rng);
}

static std::string handle_condition_onset(const CppState& state, PersonRecord& rec, std::mt19937& rng) {
    std::string primary_code = state.codes.empty() ? "" : state.codes[0].code;
    if (!primary_code.empty() && rec.active_conditions.count(primary_code) == 0) {
        // Only create record if not already active (matches Synthea Java behavior)
        CppCondition cond;
        cond.onset        = rec.t_num;
        cond.codes        = state.codes;
        cond.encounter_id = rec.current_encounter_id;
        rec.conditions.push_back(std::move(cond));
        rec.active_conditions.insert(primary_code);
        if (!state.storage_key.empty())
            rec.module_current[state.storage_key] = primary_code;
    }
    rec.visited.insert(state.visited_key);
    return next_state(state, rec, rng);
}

static std::string handle_condition_end(const CppState& state, PersonRecord& rec, std::mt19937& rng) {
    // Find active condition by ref_key and mark abated
    // ref_key = "__condition_env__<onset_state_name>"
    // The primary code was stored under that key
    auto it = rec.module_current.find(state.ref_key);
    if (it != rec.module_current.end()) {
        const std::string& code = it->second;
        rec.active_conditions.erase(code);
        for (auto& c : rec.conditions)
            if (!c.codes.empty() && c.codes[0].code == code && c.abated == 0.0)
                c.abated = rec.t_num;
    }
    rec.visited.insert(state.visited_key);
    return next_state(state, rec, rng);
}

static std::string handle_medication_order(const CppState& state, PersonRecord& rec, std::mt19937& rng) {
    std::string primary_code = state.codes.empty() ? "" : state.codes[0].code;
    if (!primary_code.empty()) {
        CppMedication med;
        med.start        = rec.t_num;
        med.codes        = state.codes;
        med.encounter_id = rec.current_encounter_id;
        rec.medications.push_back(std::move(med));
        rec.active_medications.insert(primary_code);
        if (!state.storage_key.empty())
            rec.module_current[state.storage_key] = primary_code;
    }
    rec.visited.insert(state.visited_key);
    return next_state(state, rec, rng);
}

static std::string handle_medication_end(const CppState& state, PersonRecord& rec, std::mt19937& rng) {
    auto it = rec.module_current.find(state.ref_key);
    if (it != rec.module_current.end()) {
        const std::string& code = it->second;
        rec.active_medications.erase(code);
        for (auto& m : rec.medications)
            if (!m.codes.empty() && m.codes[0].code == code && m.active) {
                m.stop   = rec.t_num;
                m.active = false;
            }
    }
    rec.visited.insert(state.visited_key);
    return next_state(state, rec, rng);
}

static std::string handle_careplan_start(const CppState& state, PersonRecord& rec, std::mt19937& rng) {
    std::string primary_code = state.codes.empty() ? "" : state.codes[0].code;
    if (!primary_code.empty()) {
        CppCarePlan cp;
        cp.start        = rec.t_num;
        cp.codes        = state.codes;
        cp.activities   = state.activities;
        cp.encounter_id = rec.current_encounter_id;
        rec.careplans.push_back(std::move(cp));
        rec.active_careplans.insert(primary_code);
        if (!state.storage_key.empty())
            rec.module_current[state.storage_key] = primary_code;
    }
    rec.visited.insert(state.visited_key);
    return next_state(state, rec, rng);
}

static std::string handle_careplan_end(const CppState& state, PersonRecord& rec, std::mt19937& rng) {
    auto it = rec.module_current.find(state.ref_key);
    if (it != rec.module_current.end()) {
        const std::string& code = it->second;
        rec.active_careplans.erase(code);
        for (auto& cp : rec.careplans)
            if (!cp.codes.empty() && cp.codes[0].code == code && cp.active) {
                cp.stop   = rec.t_num;
                cp.active = false;
            }
    }
    rec.visited.insert(state.visited_key);
    return next_state(state, rec, rng);
}

static std::string handle_allergy_onset(const CppState& state, PersonRecord& rec, std::mt19937& rng) {
    std::string primary_code = state.codes.empty() ? "" : state.codes[0].code;
    if (!primary_code.empty()) {
        CppAllergy al;
        al.onset        = rec.t_num;
        al.codes        = state.codes;
        al.encounter_id = rec.current_encounter_id;
        rec.allergies.push_back(std::move(al));
        rec.active_conditions.insert(primary_code);
        if (!state.storage_key.empty())
            rec.module_current[state.storage_key] = primary_code;
    }
    rec.visited.insert(state.visited_key);
    return next_state(state, rec, rng);
}

static std::string handle_allergy_end(const CppState& state, PersonRecord& rec, std::mt19937& rng) {
    auto it = rec.module_current.find(state.ref_key);
    if (it != rec.module_current.end()) {
        rec.active_conditions.erase(it->second);
        for (auto& a : rec.allergies)
            if (!a.codes.empty() && a.codes[0].code == it->second && a.abated == 0.0)
                a.abated = rec.t_num;
    }
    rec.visited.insert(state.visited_key);
    return next_state(state, rec, rng);
}

static std::string handle_procedure(const CppState& state, PersonRecord& rec, std::mt19937& rng) {
    CppProcedure proc;
    proc.time        = rec.t_num;
    proc.codes       = state.codes;
    proc.encounter_id = rec.current_encounter_id;
    rec.procedures.push_back(std::move(proc));
    rec.visited.insert(state.visited_key);
    return next_state(state, rec, rng);
}

static std::string handle_observation(const CppState& state, PersonRecord& rec, std::mt19937& rng) {
    CppObservation obs;
    obs.time        = rec.t_num;
    obs.codes       = state.codes;
    obs.category    = state.obs_category;
    obs.unit        = state.obs_unit;
    obs.encounter_id = rec.current_encounter_id;

    // Determine value
    switch (state.obs_value.kind) {
    case QuantityDef::Kind::Exact:
    case QuantityDef::Kind::Range:
        obs.value = sample_quantity(state.obs_value, rng);
        break;
    case QuantityDef::Kind::Code:
        obs.value = state.obs_value.code.code;
        break;
    case QuantityDef::Kind::VitalSign: {
        auto it = rec.vital_signs.find(state.obs_value.attr_or_name);
        obs.value = (it != rec.vital_signs.end()) ? it->second.value : 0.0;
        break;
    }
    case QuantityDef::Kind::Attribute: {
        auto it = rec.attributes.find(state.obs_value.attr_or_name);
        obs.value = (it != rec.attributes.end()) ? it->second : AttrVal{};
        break;
    }
    default: break;
    }

    rec.invalidate_obs_cache();
    rec.observations.push_back(std::move(obs));
    rec.visited.insert(state.visited_key);
    return next_state(state, rec, rng);
}

static std::string handle_multi_observation(const CppState& state, PersonRecord& rec, std::mt19937& rng) {
    for (const auto& so : state.sub_obs) {
        CppObservation obs;
        obs.time        = rec.t_num;
        obs.codes       = so.codes;
        obs.category    = so.category;
        obs.unit        = so.unit;
        obs.encounter_id = rec.current_encounter_id;
        obs.value       = sample_quantity(so.value, rng);
        rec.observations.push_back(std::move(obs));
    }
    rec.invalidate_obs_cache();
    rec.visited.insert(state.visited_key);
    return next_state(state, rec, rng);
}

static std::string handle_vital_sign(const CppState& state, PersonRecord& rec, std::mt19937& rng) {
    if (!state.vs_name.empty()) {
        CppVitalSign vs;
        vs.value = sample_quantity(state.obs_value, rng);
        vs.unit  = state.obs_unit;
        vs.time  = rec.t_num;
        rec.vital_signs[state.vs_name] = vs;
    }
    rec.visited.insert(state.visited_key);
    return next_state(state, rec, rng);
}

static std::string handle_symptom(const CppState& state, PersonRecord& rec, std::mt19937& rng) {
    if (!state.sym_name.empty()) {
        // Apply probability
        if (state.sym_probability < 1.0) {
            std::uniform_real_distribution<double> d(0.0, 1.0);
            if (d(rng) > state.sym_probability) {
                rec.visited.insert(state.visited_key);
                return next_state(state, rec, rng);
            }
        }
        CppSymptom sym;
        sym.value = sample_quantity(state.obs_value, rng);
        sym.cause = state.sym_cause;
        sym.time  = rec.t_num;
        rec.symptoms[state.sym_name] = sym;
    }
    rec.visited.insert(state.visited_key);
    return next_state(state, rec, rng);
}

static std::string handle_vaccine(const CppState& state, PersonRecord& rec, std::mt19937& rng) {
    CppVaccine vac;
    vac.time        = rec.t_num;
    vac.codes       = state.codes;
    vac.encounter_id = rec.current_encounter_id;
    rec.vaccines.push_back(std::move(vac));
    rec.visited.insert(state.visited_key);
    return next_state(state, rec, rng);
}

// ImagingStudy — record as procedure (correct: R engine tracks these)
static std::string handle_imaging(const CppState& state, PersonRecord& rec, std::mt19937& rng) {
    CppProcedure proc;
    proc.time         = rec.t_num;
    proc.codes        = state.codes;
    proc.encounter_id = rec.current_encounter_id;
    rec.procedures.push_back(std::move(proc));
    rec.visited.insert(state.visited_key);
    return next_state(state, rec, rng);
}

// Device / DeviceEnd / SupplyList — mark visited and advance, no clinical record
static std::string handle_noop(const CppState& state, PersonRecord& rec, std::mt19937& rng) {
    rec.visited.insert(state.visited_key);
    return next_state(state, rec, rng);
}

// ── Central dispatch ──────────────────────────────────────────────────────────
std::string dispatch_state(const CppState& state, PersonRecord& rec,
                           std::mt19937& rng, const ModuleIndex& /*idx*/) {
    switch (state.type) {
    case StateType::Initial:          return handle_initial(state, rec, rng);
    case StateType::Simple:           return handle_simple(state, rec, rng);
    case StateType::Terminal:         return handle_terminal(state, rec, rng);
    case StateType::Delay:            return handle_delay(state, rec, rng);
    case StateType::Guard:            return handle_guard(state, rec, rng);
    case StateType::Death:            return handle_death(state, rec, rng);
    case StateType::SetAttribute:     return handle_set_attribute(state, rec, rng);
    case StateType::Counter:          return handle_counter(state, rec, rng);
    case StateType::CallSubmodule:    return handle_call_submodule(state, rec, rng);
    case StateType::Encounter:        return handle_encounter(state, rec, rng);
    case StateType::EncounterEnd:     return handle_encounter_end(state, rec, rng);
    case StateType::ConditionOnset:   return handle_condition_onset(state, rec, rng);
    case StateType::ConditionEnd:     return handle_condition_end(state, rec, rng);
    case StateType::MedicationOrder:  return handle_medication_order(state, rec, rng);
    case StateType::MedicationEnd:    return handle_medication_end(state, rec, rng);
    case StateType::CarePlanStart:    return handle_careplan_start(state, rec, rng);
    case StateType::CarePlanEnd:      return handle_careplan_end(state, rec, rng);
    case StateType::AllergyOnset:     return handle_allergy_onset(state, rec, rng);
    case StateType::AllergyEnd:       return handle_allergy_end(state, rec, rng);
    case StateType::Procedure:        return handle_procedure(state, rec, rng);
    case StateType::Observation:      return handle_observation(state, rec, rng);
    case StateType::MultiObservation: return handle_multi_observation(state, rec, rng);
    case StateType::DiagnosticReport: return handle_multi_observation(state, rec, rng);
    case StateType::VitalSign:        return handle_vital_sign(state, rec, rng);
    case StateType::Symptom:          return handle_symptom(state, rec, rng);
    case StateType::Vaccine:          return handle_vaccine(state, rec, rng);
    case StateType::ImagingStudy:     return handle_imaging(state, rec, rng);
    case StateType::Device:           return handle_noop(state, rec, rng);
    case StateType::DeviceEnd:        return handle_noop(state, rec, rng);
    case StateType::SupplyList:       return handle_noop(state, rec, rng);
    default:
        // Unknown: mark visited, advance
        rec.visited.insert(state.visited_key);
        return next_state(state, rec, rng);
    }
}
