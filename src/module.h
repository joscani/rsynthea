#pragma once
#include "transitions.h"

// ── State type enum ───────────────────────────────────────────────────────────
enum class StateType : uint8_t {
    Initial, Terminal, Simple, Delay, Guard,
    Encounter, EncounterEnd,
    ConditionOnset, ConditionEnd,
    MedicationOrder, MedicationEnd,
    CarePlanStart, CarePlanEnd,
    AllergyOnset, AllergyEnd,
    Procedure,
    Observation, MultiObservation, DiagnosticReport,
    VitalSign, Symptom,
    SetAttribute, Counter, Death,
    CallSubmodule, Vaccine, ImagingStudy,
    Device, DeviceEnd, SupplyList,
    Unknown
};

inline StateType parse_state_type(const std::string& s) {
    // Ordered by frequency (from profiling across 242 modules)
    if (s == "Simple")           return StateType::Simple;
    if (s == "SetAttribute")     return StateType::SetAttribute;
    if (s == "Procedure")        return StateType::Procedure;
    if (s == "MedicationOrder")  return StateType::MedicationOrder;
    if (s == "Delay")            return StateType::Delay;
    if (s == "ConditionOnset")   return StateType::ConditionOnset;
    if (s == "EncounterEnd")     return StateType::EncounterEnd;
    if (s == "Symptom")          return StateType::Symptom;
    if (s == "Encounter")        return StateType::Encounter;
    if (s == "Observation")      return StateType::Observation;
    if (s == "CallSubmodule")    return StateType::CallSubmodule;
    if (s == "Initial")          return StateType::Initial;
    if (s == "Terminal")         return StateType::Terminal;
    if (s == "ConditionEnd")     return StateType::ConditionEnd;
    if (s == "DiagnosticReport") return StateType::DiagnosticReport;
    if (s == "MedicationEnd")    return StateType::MedicationEnd;
    if (s == "CarePlanStart")    return StateType::CarePlanStart;
    if (s == "Counter")          return StateType::Counter;
    if (s == "Death")            return StateType::Death;
    if (s == "Device")           return StateType::Device;
    if (s == "Guard")            return StateType::Guard;
    if (s == "ImagingStudy")     return StateType::ImagingStudy;
    if (s == "DeviceEnd")        return StateType::DeviceEnd;
    if (s == "MultiObservation") return StateType::MultiObservation;
    if (s == "CarePlanEnd")      return StateType::CarePlanEnd;
    if (s == "AllergyOnset")     return StateType::AllergyOnset;
    if (s == "SupplyList")       return StateType::SupplyList;
    if (s == "VitalSign")        return StateType::VitalSign;
    if (s == "Vaccine")          return StateType::Vaccine;
    if (s == "AllergyEnd")       return StateType::AllergyEnd;
    return StateType::Unknown;
}

// ── Quantity / range value used by Delay, Observation, Symptom, VitalSign ────
struct QuantityDef {
    enum class Kind : uint8_t { Exact, Range, Attribute, VitalSign, Code,
                                Gaussian, Exponential } kind = Kind::Exact;
    double low  = 0.0;   // exact value or range low (in seconds for delays)
    double high = 0.0;   // range high
    std::string attr_or_name;  // attribute name / vital_sign name / code
    CppCode code;              // for value_code observations
    double probability = 1.0;  // Symptom probability
};

// ── Sub-observation entry (MultiObservation / DiagnosticReport) ───────────────
struct SubObs {
    std::vector<CppCode> codes;
    QuantityDef          value;
    std::string          unit;
    std::string          category;
};

// ── Pre-compiled state ────────────────────────────────────────────────────────
struct CppState {
    std::string name;
    StateType   type = StateType::Unknown;
    CppTransition transition;

    // ── Precomputed string keys (avoid paste0 at runtime) ────────────────
    std::string visited_key;    // "__visited__" + name
    std::string delay_key;      // "__delay_until__" + name
    std::string wellness_key;   // "__wellness_time__" + module_name "/" + name
    std::string call_key;       // "__call_submodule__" + name
    std::string ref_key;        // "__condition_env__"/etc. for End states

    // ── Encounter fields ──────────────────────────────────────────────────
    bool        is_wellness = false;
    std::string encounter_class;   // "ambulatory", "inpatient", etc.
    std::vector<CppCode> codes;

    // ── Delay definition ──────────────────────────────────────────────────
    struct DelayDef {
        enum class Kind : uint8_t { Exact, Range, Gaussian, Exponential } kind = Kind::Exact;
        double low_secs  = 0.0;   // exact / range-low / gaussian-mean / exponential-mean
        double high_secs = 0.0;   // range-high / gaussian-std_dev / -1 for exponential
    } delay;

    // ── Storage key for onset states (ConditionOnset → "__condition_env__name") ──
    // Separate from call_key ("__call_submodule__name") to avoid confusion.
    std::string storage_key;

    // ── Guard condition ───────────────────────────────────────────────────
    std::unique_ptr<CppCond> guard_cond;

    // ── SetAttribute ──────────────────────────────────────────────────────
    std::string attr_name;
    AttrVal     attr_value;

    // ── Counter ───────────────────────────────────────────────────────────
    // attr_name reused; counter_action: true=increment, false=decrement
    bool   counter_increment = true;
    double counter_amount    = 1.0;

    // ── Observation / VitalSign / Symptom ─────────────────────────────────
    std::string  obs_category;
    std::string  obs_unit;
    QuantityDef  obs_value;
    std::string  vs_name;
    std::string  sym_name;
    std::string  sym_cause;
    double       sym_probability = 1.0;

    // ── MultiObservation / DiagnosticReport ───────────────────────────────
    std::vector<SubObs> sub_obs;

    // ── CallSubmodule ─────────────────────────────────────────────────────
    std::string submodule_name;

    // ── CarePlan activities ───────────────────────────────────────────────
    std::vector<CppCode> activities;

    // ── AllergyOnset ─────────────────────────────────────────────────────
    std::string allergy_type;
};

// ── Pre-compiled module ───────────────────────────────────────────────────────
struct CppModule {
    std::string name;
    std::string state_key;   // "__module_state__" + name
    bool        is_submodule = false;
    std::unordered_map<std::string, CppState> states;
    int         module_idx = -1;  // position in the modules vector (set by compile_all_modules)
};

// Compiler functions are defined in module_compiler.cpp and exported via Rcpp.
