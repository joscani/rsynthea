// [[Rcpp::plugins(cpp17)]]
#include "module.h"

// ── Safe scalar→string (codes can be integer in jsonlite-parsed JSON) ─────────
static std::string sexp_to_str(SEXP v) {
    if (Rf_isNull(v) || Rf_length(v) == 0) return "";
    if (TYPEOF(v) == STRSXP)  return Rcpp::as<std::string>(v);
    if (TYPEOF(v) == INTSXP)  return std::to_string(INTEGER(v)[0]);
    if (TYPEOF(v) == REALSXP) {
        double d = REAL(v)[0]; long long li = (long long)d;
        return ((double)li == d) ? std::to_string(li) : std::to_string(d);
    }
    return "";
}

// ── Helpers — read fields from an R environment (GMFState is new.env()) ───────
// Use Rcpp::Environment for safe, portable access across R versions.

static SEXP env_get(SEXP env_sexp, const char* key) {
    Rcpp::Environment env(env_sexp);
    if (!env.exists(key)) return R_NilValue;
    return env[key];
}

static std::string renv_str(SEXP env, const char* key, const char* def = "") {
    SEXP v = env_get(env, key);
    if (Rf_isNull(v) || Rf_length(v) == 0) return def;
    if (TYPEOF(v) == STRSXP) return Rcpp::as<std::string>(v);
    return def;
}
static double safe_dbl(SEXP v, double def = 0.0) {
    if (Rf_isNull(v) || Rf_length(v) == 0) return def;
    if (TYPEOF(v) == REALSXP) return REAL(v)[0];
    if (TYPEOF(v) == INTSXP)  return (double)INTEGER(v)[0];
    if (TYPEOF(v) == STRSXP)  { try { return std::stod(Rcpp::as<std::string>(v)); } catch(...) {} }
    return def;
}
static double renv_dbl(SEXP env, const char* key, double def = 0.0) {
    return safe_dbl(env_get(env, key), def);
}
static bool renv_bool(SEXP env, const char* key, bool def = false) {
    SEXP v = env_get(env, key);
    if (Rf_isNull(v) || Rf_length(v) == 0) return def;
    if (TYPEOF(v) == LGLSXP) return (bool)(LOGICAL(v)[0] != 0);
    return def;
}
static double rlist_dbl(Rcpp::List L, const char* key, double def = 0.0) {
    if (L.containsElementNamed(key)) return safe_dbl(L[key], def);
    return def;
}
static double unit_secs(const std::string& u) {
    if (u == "years")  return 365.25 * 86400.0;
    if (u == "months") return 30.44  * 86400.0;
    if (u == "weeks")  return 7.0    * 86400.0;
    if (u == "days")   return 86400.0;
    if (u == "hours")  return 3600.0;
    return 86400.0;
}

// Parse an R "codes" list (already processed by .parse_codes) → vector<CppCode>
static std::vector<CppCode> parse_codes(SEXP codes_sexp) {
    std::vector<CppCode> result;
    if (Rf_isNull(codes_sexp) || !Rf_isVectorList(codes_sexp)) return result;
    Rcpp::List codes(codes_sexp);
    result.reserve(codes.size());
    for (int i = 0; i < codes.size(); ++i) {
        if (!Rf_isVectorList(codes[i])) continue;
        Rcpp::List c(codes[i]);
        CppCode cc;
        if (c.containsElementNamed("code"))    cc.code    = sexp_to_str(c["code"]);
        if (c.containsElementNamed("system"))  cc.system  = sexp_to_str(c["system"]);
        if (c.containsElementNamed("display")) cc.display = sexp_to_str(c["display"]);
        result.push_back(std::move(cc));
    }
    return result;
}

// Set of state types where values are time durations (need unit_secs conversion).
// VitalSign, Observation, Symptom values are raw measurements — no conversion.
static bool is_time_duration(const std::string& state_type) {
    return state_type == "Delay" || state_type == "SetAttribute";
}

// Parse the definition sub-list from the GMFState env for quantity/range values
static QuantityDef parse_quantity(SEXP def_sexp, const std::string& state_type) {
    QuantityDef q;
    if (Rf_isNull(def_sexp) || !Rf_isVectorList(def_sexp)) return q;
    Rcpp::List def(def_sexp);

    // Unit conversion factor: only for Delay/SetAttribute (time durations).
    // VitalSign, Observation, Symptom use raw measurement values — no conversion.
    bool apply_units = is_time_duration(state_type);

    // Check for exact / range sub-keys (Delay, Observation, Symptom, VitalSign)
    if (def.containsElementNamed("exact")) {
        q.kind = QuantityDef::Kind::Exact;
        SEXP ex = def["exact"];
        if (Rf_isVectorList(ex)) {
            Rcpp::List exL(ex);
            double qty = rlist_dbl(exL, "quantity", 0.0);
            if (apply_units) {
                std::string unit = exL.containsElementNamed("unit")
                    ? sexp_to_str(exL["unit"]) : std::string("days");
                qty *= unit_secs(unit);
            }
            q.low = q.high = qty;
        } else if (Rf_length(ex) > 0) {
            q.low = q.high = safe_dbl(ex, 0.0);
        }
        return q;
    }
    if (def.containsElementNamed("range")) {
        q.kind = QuantityDef::Kind::Range;
        SEXP rng = def["range"];
        if (Rf_isVectorList(rng)) {
            Rcpp::List rL(rng);
            double lo = rlist_dbl(rL, "low",  0.0);
            double hi = rlist_dbl(rL, "high", 0.0);
            if (apply_units) {
                std::string unit = rL.containsElementNamed("unit")
                    ? sexp_to_str(rL["unit"]) : std::string("days");
                double f = unit_secs(unit);
                lo *= f; hi *= f;
            }
            q.low  = lo;
            q.high = hi;
        }
        return q;
    }
    // Distribution-style delays: "distribution": {"kind": "...", "parameters": {...}}, "unit": "..."
    // Supported kinds: GAUSSIAN, EXACT, UNIFORM, EXPONENTIAL
    if (def.containsElementNamed("distribution")) {
        SEXP dist_s = def["distribution"];
        double f = 1.0;
        if (apply_units) {
            std::string unit = def.containsElementNamed("unit") ? sexp_to_str(def["unit"]) : "days";
            f = unit_secs(unit);
        }
        if (Rf_isVectorList(dist_s)) {
            Rcpp::List dL(dist_s);
            std::string kind = dL.containsElementNamed("kind") ? sexp_to_str(dL["kind"]) : "GAUSSIAN";
            Rcpp::List params = dL.containsElementNamed("parameters")
                ? Rcpp::List(dL["parameters"]) : Rcpp::List::create();

            if (kind == "EXACT") {
                q.kind = QuantityDef::Kind::Exact;
                q.low = q.high = rlist_dbl(params, "value", 0.0) * f;
            } else if (kind == "UNIFORM") {
                q.kind = QuantityDef::Kind::Range;   // uniform_real_distribution at runtime
                q.low  = rlist_dbl(params, "low",  0.0) * f;
                q.high = rlist_dbl(params, "high", 0.0) * f;
            } else if (kind == "GAUSSIAN") {
                q.kind = QuantityDef::Kind::Gaussian; // low=mean, high=std_dev
                q.low  = rlist_dbl(params, "mean",              0.0) * f;
                q.high = rlist_dbl(params, "standardDeviation", 0.0) * f;
            } else if (kind == "EXPONENTIAL") {
                q.kind = QuantityDef::Kind::Exponential; // low=mean
                q.low  = rlist_dbl(params, "mean", 0.0) * f;
                q.high = 0.0;
            } else {
                q.kind = QuantityDef::Kind::Exact;
                double v = rlist_dbl(params, "value", rlist_dbl(params, "mean", 0.0));
                q.low = q.high = v * f;
            }
        }
        return q;
    }
    // Observation: value_code
    if (def.containsElementNamed("value_code")) {
        q.kind = QuantityDef::Kind::Code;
        SEXP vc = def["value_code"];
        if (Rf_isVectorList(vc)) {
            Rcpp::List vcL(vc);
            if (vcL.containsElementNamed("code"))    q.code.code    = Rcpp::as<std::string>(vcL["code"]);
            if (vcL.containsElementNamed("system"))  q.code.system  = Rcpp::as<std::string>(vcL["system"]);
            if (vcL.containsElementNamed("display")) q.code.display = Rcpp::as<std::string>(vcL["display"]);
        }
        return q;
    }
    // Observation: vital_sign reference
    if (def.containsElementNamed("vital_sign")) {
        q.kind = QuantityDef::Kind::VitalSign;
        q.attr_or_name = Rcpp::as<std::string>(def["vital_sign"]);
        return q;
    }
    // Observation: attribute reference
    if (def.containsElementNamed("attribute")) {
        q.kind = QuantityDef::Kind::Attribute;
        q.attr_or_name = Rcpp::as<std::string>(def["attribute"]);
        return q;
    }
    return q;
}

// Compile one GMFState (R environment) → CppState
static CppState compile_state(SEXP state_env, const std::string& module_name) {
    CppState s;
    if (!Rf_isEnvironment(state_env)) return s;

    s.name             = renv_str(state_env, "name");
    s.type             = parse_state_type(renv_str(state_env, "type"));
    s.visited_key      = renv_str(state_env, "visited_key");
    s.delay_key        = renv_str(state_env, "delay_key");
    s.wellness_key     = renv_str(state_env, "wellness_key");
    s.call_key         = renv_str(state_env, "call_key");
    s.is_wellness      = renv_bool(state_env, "is_wellness");
    s.encounter_class  = renv_str(state_env, "encounter_class", "ambulatory");

    // Codes
    SEXP codes_s = env_get(state_env, "codes");
    if (!Rf_isNull(codes_s)) s.codes = parse_codes(codes_s);

    // Activities (CarePlan)
    SEXP acts_s = env_get(state_env, "activities");
    if (!Rf_isNull(acts_s)) s.activities = parse_codes(acts_s);

    // Transition (already parsed R list)
    SEXP trans_s = env_get(state_env, "transition");
    if (!Rf_isNull(trans_s))
        s.transition = compile_transition(trans_s);

    // Definition (raw JSON list — for type-specific fields)
    SEXP def_s = env_get(state_env, "definition");

    // ── Type-specific fields ──────────────────────────────────────────────
    switch (s.type) {

    case StateType::Delay: {
        if (!Rf_isNull(def_s) && Rf_isVectorList(def_s)) {
            Rcpp::List defL(def_s);
            auto qd = parse_quantity(def_s, "Delay");
            s.delay.low_secs  = qd.low;
            s.delay.high_secs = qd.high;
            // Map QuantityDef::Kind → DelayDef::Kind
            switch (qd.kind) {
            case QuantityDef::Kind::Range:       s.delay.kind = CppState::DelayDef::Kind::Range;       break;
            case QuantityDef::Kind::Gaussian:    s.delay.kind = CppState::DelayDef::Kind::Gaussian;    break;
            case QuantityDef::Kind::Exponential: s.delay.kind = CppState::DelayDef::Kind::Exponential; break;
            default:                             s.delay.kind = CppState::DelayDef::Kind::Exact;       break;
            }
        }
        break;
    }

    case StateType::Guard: {
        SEXP allow_s = env_get(state_env, "guard_allow");
        if (!Rf_isNull(allow_s))
            s.guard_cond = compile_condition(allow_s);
        break;
    }

    case StateType::SetAttribute: {
        s.attr_name = renv_str(state_env, "attr_name");
        SEXP av = env_get(state_env, "attr_value");
        if (!Rf_isNull(av) && Rf_length(av) > 0) {
            if (TYPEOF(av) == LGLSXP)
                s.attr_value = (bool)(LOGICAL(av)[0] != 0);
            else if (TYPEOF(av) == REALSXP || TYPEOF(av) == INTSXP)
                s.attr_value = Rcpp::as<double>(av);
            else if (TYPEOF(av) == STRSXP)
                s.attr_value = Rcpp::as<std::string>(av);
        }
        // SetAttribute with distribution (no "value" field) → attr_value stays monostate (null).
        // The Counter null-safety fix handles downstream: Counter on null attr = no-op (R behavior).
        break;
    }

    case StateType::Counter: {
        s.attr_name         = renv_str(state_env, "attr_name");
        std::string action  = renv_str(state_env, "counter_action", "increment");
        s.counter_increment = (action != "decrement");
        s.counter_amount    = renv_dbl(state_env, "counter_amount", 1.0);
        break;
    }

    case StateType::Observation:
    case StateType::MultiObservation:
    case StateType::DiagnosticReport: {
        s.obs_category = renv_str(state_env, "category");
        s.obs_unit     = renv_str(state_env, "unit");
        if (!Rf_isNull(def_s)) s.obs_value = parse_quantity(def_s, "Observation");
        // Sub-observations
        SEXP sub_s = env_get(state_env, "sub_codes");
        if (!Rf_isNull(sub_s) && Rf_isVectorList(sub_s)) {
            Rcpp::List subs(sub_s);
            // sub_codes is a list of code vectors, one per observation entry
            if (!Rf_isNull(def_s) && Rf_isVectorList(def_s)) {
                Rcpp::List defL(def_s);
                SEXP obs_list_s = defL.containsElementNamed("observations")
                    ? defL["observations"] : R_NilValue;
                if (!Rf_isNull(obs_list_s) && Rf_isVectorList(obs_list_s)) {
                    Rcpp::List obs_list(obs_list_s);
                    for (int i = 0; i < obs_list.size(); ++i) {
                        SubObs so;
                        if (i < subs.size())
                            so.codes = parse_codes(subs[i]);
                        if (Rf_isVectorList(obs_list[i])) {
                            Rcpp::List oe(obs_list[i]);
                            so.value = parse_quantity(obs_list[i], "Observation");
                            if (oe.containsElementNamed("unit"))
                                so.unit = Rcpp::as<std::string>(oe["unit"]);
                            if (oe.containsElementNamed("category"))
                                so.category = Rcpp::as<std::string>(oe["category"]);
                        }
                        s.sub_obs.push_back(std::move(so));
                    }
                }
            }
        }
        break;
    }

    case StateType::VitalSign: {
        s.vs_name  = renv_str(state_env, "vs_name");
        s.obs_unit = renv_str(state_env, "unit");
        if (!Rf_isNull(def_s)) s.obs_value = parse_quantity(def_s, "VitalSign");
        break;
    }

    case StateType::Symptom: {
        s.sym_name  = renv_str(state_env, "sym_name");
        s.sym_cause = renv_str(state_env, "sym_cause");
        if (!Rf_isNull(def_s) && Rf_isVectorList(def_s)) {
            s.obs_value = parse_quantity(def_s, "Symptom");
            Rcpp::List defL(def_s);
            if (defL.containsElementNamed("probability"))
                s.sym_probability = Rcpp::as<double>(defL["probability"]);
        }
        break;
    }

    case StateType::CallSubmodule: {
        s.submodule_name = renv_str(state_env, "submodule_name");
        break;
    }

    // Onset states: storage_key for use by End states
    case StateType::ConditionOnset: {
        s.storage_key = renv_str(state_env, "cond_key");
        break;
    }
    case StateType::MedicationOrder: {
        s.storage_key = renv_str(state_env, "med_key");
        break;
    }
    case StateType::CarePlanStart: {
        s.storage_key = renv_str(state_env, "cp_key");
        break;
    }

    case StateType::ConditionEnd: {
        s.ref_key = renv_str(state_env, "cond_end_key");
        break;
    }
    case StateType::MedicationEnd: {
        s.ref_key = renv_str(state_env, "med_end_key");
        break;
    }
    case StateType::CarePlanEnd: {
        s.ref_key = renv_str(state_env, "cp_end_key");
        break;
    }
    case StateType::AllergyEnd: {
        s.ref_key = renv_str(state_env, "alg_end_key");
        break;
    }
    case StateType::DeviceEnd: {
        s.ref_key = renv_str(state_env, "device_end_key");
        break;
    }

    case StateType::AllergyOnset: {
        s.storage_key  = renv_str(state_env, "allergy_key");
        s.allergy_type = renv_str(state_env, "allergy_type");
        break;
    }

    default: break;
    }

    return s;
}

// ── Exported: compile R Module list → XPtr<vector<CppModule>> ────────────────
// Returns SEXP (external pointer) so RcppExports.cpp needs no knowledge of CppModule.
// [[Rcpp::export]]
SEXP compile_all_modules(Rcpp::List modules_r) {
    auto* vec = new std::vector<CppModule>();
    vec->reserve(modules_r.size());

    Rcpp::CharacterVector names = modules_r.names();

    for (int i = 0; i < modules_r.size(); ++i) {
        SEXP mod_sexp = modules_r[i];
        if (!Rf_isVectorList(mod_sexp)) continue;
        Rcpp::List mod_list(mod_sexp);

        CppModule cm;

        // Extract module name and metadata from R list structure
        if (mod_list.containsElementNamed("name"))
            cm.name = Rcpp::as<std::string>(mod_list["name"]);
        if (mod_list.containsElementNamed("state_key"))
            cm.state_key = Rcpp::as<std::string>(mod_list["state_key"]);
        if (mod_list.containsElementNamed("is_submodule"))
            cm.is_submodule = Rcpp::as<bool>(mod_list["is_submodule"]);

        // states is a hashed R environment
        if (mod_list.containsElementNamed("states")) {
            SEXP states_env = mod_list["states"];
            if (Rf_isEnvironment(states_env)) {
                Rcpp::Environment states_env_r(states_env);
                Rcpp::CharacterVector state_names = states_env_r.ls(false);
                for (int j = 0; j < state_names.size(); ++j) {
                    std::string sname = Rcpp::as<std::string>(state_names[j]);
                    SEXP state_env = states_env_r.exists(sname.c_str())
                        ? states_env_r[sname.c_str()] : R_NilValue;
                    if (Rf_isNull(state_env)) continue;
                    CppState cs = compile_state(state_env, cm.name);
                    if (cs.name.empty()) cs.name = sname;
                    cm.states[sname] = std::move(cs);
                }
            }
        }

        cm.module_idx = (int)vec->size();
        vec->push_back(std::move(cm));
    }

    return Rcpp::XPtr<std::vector<CppModule>>(vec, true);
}

// ── Exported: quick sanity check — return module name + state count ───────────
// [[Rcpp::export]]
Rcpp::DataFrame inspect_compiled_modules(SEXP ptr_sexp) {
    Rcpp::XPtr<std::vector<CppModule>> ptr(ptr_sexp);
    auto& vec = *ptr;
    Rcpp::CharacterVector names(vec.size());
    Rcpp::IntegerVector   n_states(vec.size());
    Rcpp::LogicalVector   is_sub(vec.size());

    for (int i = 0; i < (int)vec.size(); ++i) {
        names[i]    = vec[i].name;
        n_states[i] = (int)vec[i].states.size();
        is_sub[i]   = vec[i].is_submodule;
    }
    return Rcpp::DataFrame::create(
        Rcpp::Named("name")         = names,
        Rcpp::Named("n_states")     = n_states,
        Rcpp::Named("is_submodule") = is_sub
    );
}

// [[Rcpp::export]]
Rcpp::DataFrame inspect_delays(SEXP ptr_sexp, std::string module_name) {
    Rcpp::XPtr<std::vector<CppModule>> ptr(ptr_sexp);
    std::vector<std::string> names, kinds;
    std::vector<double> lows, highs;
    for (const auto& mod : *ptr) {
        if (!module_name.empty() && mod.name != module_name) continue;
        for (const auto& kv : mod.states) {
            if (kv.second.type != StateType::Delay) continue;
            names.push_back(mod.name + "/" + kv.first);
            kinds.push_back(kv.second.delay.kind == CppState::DelayDef::Kind::Range ? "range" : "exact");
            lows.push_back(kv.second.delay.low_secs);
            highs.push_back(kv.second.delay.high_secs);
        }
    }
    return Rcpp::DataFrame::create(
        Rcpp::Named("state") = names, Rcpp::Named("kind") = kinds,
        Rcpp::Named("low_secs") = lows, Rcpp::Named("high_secs") = highs,
        Rcpp::Named("stringsAsFactors") = false);
}
