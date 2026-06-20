#pragma once
#include "rsynthea.h"

// ── Clinical event structs ────────────────────────────────────────────────────
// CppCode is defined in rsynthea.h

struct CppEncounter {
    double start = 0.0;
    double end   = 0.0;
    std::string encounter_class;
    std::string type;
    std::vector<CppCode> codes;
    std::string id;
    bool is_wellness = false;
};

struct CppCondition {
    double onset   = 0.0;
    double abated  = 0.0;   // 0 = still active
    std::vector<CppCode> codes;
    std::string encounter_id;
};

struct CppMedication {
    double start   = 0.0;
    double stop    = 0.0;
    std::vector<CppCode> codes;
    std::string encounter_id;
    bool active = true;
};

struct CppCarePlan {
    double start  = 0.0;
    double stop   = 0.0;
    std::vector<CppCode> codes;
    std::vector<CppCode> activities;
    std::string encounter_id;
    bool active = true;
};

struct CppAllergy {
    double onset  = 0.0;
    double abated = 0.0;
    std::vector<CppCode> codes;
    std::string encounter_id;
};

struct CppObservation {
    double time = 0.0;
    std::vector<CppCode> codes;
    AttrVal value;
    std::string unit;
    std::string category;
    std::string encounter_id;
};

struct CppProcedure {
    double time = 0.0;
    std::vector<CppCode> codes;
    std::string encounter_id;
    double duration = 0.0;
};

struct CppVaccine {
    double time = 0.0;
    std::vector<CppCode> codes;
    std::string encounter_id;
};

struct CppVitalSign {
    double value = 0.0;
    std::string unit;
    double time  = 0.0;
};

struct CppSymptom {
    double value = 0.0;
    std::string cause;
    double time  = 0.0;
};

// ── PersonRecord ──────────────────────────────────────────────────────────────
// Central mutable state for one patient's simulation.
// Hot fields are plain C++ primitives; everything else uses unordered_map.
struct PersonRecord {
    // ── Hot fields (read every timestep / every state) ────────────────────
    double t_num     = 0.0;   // current time as POSIXct numeric (seconds since epoch)
    double birth_num = 0.0;   // birth date as POSIXct numeric
    bool   is_alive  = true;

    // ── Module state machine ──────────────────────────────────────────────
    // Flat vector indexed by CppModule::module_idx (avoid hash overhead in hot loop).
    // Empty string = uninitialized (→ "Initial"); TERMINAL sentinel marks done.
    std::vector<std::string> module_states_flat;  // size set in simulate_life_cpp

    // Fallback map (for submodule calls where module_idx may be unknown)
    std::unordered_map<std::string, std::string> module_current;

    // ── Delay / wellness timers ───────────────────────────────────────────
    // Keyed by state["delay_key"] or state["wellness_key"]
    std::unordered_map<std::string, double> timers;

    // ── Visited state markers (PriorState condition) ──────────────────────
    std::unordered_set<std::string> visited;

    // ── CallSubmodule pending requests ────────────────────────────────────
    // state["call_key"]  →  submodule name
    std::unordered_map<std::string, std::string> submodule_calls;

    // ── Patient attributes (demographics + simulation-set attributes) ─────
    std::unordered_map<std::string, AttrVal> attributes;

    // ── Vital signs & symptoms ────────────────────────────────────────────
    std::unordered_map<std::string, CppVitalSign> vital_signs;
    std::unordered_map<std::string, CppSymptom>   symptoms;

    // ── Active clinical collections (for O(1) "Active Condition" checks) ──
    std::unordered_set<std::string> active_conditions;   // by primary code
    std::unordered_set<std::string> active_medications;
    std::unordered_set<std::string> active_careplans;

    // ── Clinical event log (written by state handlers) ────────────────────
    std::vector<CppEncounter>   encounters;
    std::vector<CppCondition>   conditions;
    std::vector<CppMedication>  medications;
    std::vector<CppCarePlan>    careplans;
    std::vector<CppAllergy>     allergies;
    std::vector<CppObservation> observations;
    std::vector<CppProcedure>   procedures;
    std::vector<CppVaccine>     vaccines;

    // ── Latest observation cache (for "Observation" condition type) ───────
    std::unordered_map<std::string, const CppObservation*> latest_obs_cache;

    // ── Encounter context (current open encounter during state execution) ──
    std::string current_encounter_id;

    // ── Helpers ───────────────────────────────────────────────────────────
    AttrVal get_attr(const std::string& key) const {
        auto it = attributes.find(key);
        return (it != attributes.end()) ? it->second : AttrVal{};
    }

    void set_attr(const std::string& key, AttrVal val) {
        attributes[key] = std::move(val);
    }

    bool has_attr(const std::string& key) const {
        auto it = attributes.find(key);
        return (it != attributes.end()) && !attr_is_null(it->second);
    }

    const CppObservation* latest_observation(const std::string& code) {
        auto it = latest_obs_cache.find(code);
        if (it != latest_obs_cache.end()) return it->second;
        // scan from end
        for (int i = (int)observations.size() - 1; i >= 0; --i) {
            for (const auto& c : observations[i].codes) {
                if (c.code == code) {
                    latest_obs_cache[code] = &observations[i];
                    return &observations[i];
                }
            }
        }
        return nullptr;
    }

    void invalidate_obs_cache() { latest_obs_cache.clear(); }
};
