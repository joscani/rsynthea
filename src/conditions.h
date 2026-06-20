#pragma once
#include "rsynthea.h"

// ── Condition types ───────────────────────────────────────────────────────────
enum class CondType : uint8_t {
    True, False,
    And, Or, Not, AtLeast,
    Gender, Age, Date, Race, SocioeconomicStatus,
    Attribute, Symptom, VitalSign, Observation,
    ActiveCondition, ActiveMedication, ActiveCarePlan, ActiveAllergy,
    PriorState,
    Unknown
};

// Comparison operator encoded as enum for fast dispatch
enum class CompOp : uint8_t { Eq, Ne, Lt, Le, Gt, Ge, IsNil, IsNotNil, Unknown };

inline CompOp parse_comp_op(const std::string& s) {
    if (s == "==" || s.empty())    return CompOp::Eq;
    if (s == "!=")                 return CompOp::Ne;
    if (s == "<")                  return CompOp::Lt;
    if (s == "<=")                 return CompOp::Le;
    if (s == ">")                  return CompOp::Gt;
    if (s == ">=")                 return CompOp::Ge;
    if (s == "is nil")             return CompOp::IsNil;
    if (s == "is not nil")         return CompOp::IsNotNil;
    return CompOp::Unknown;
}

// ── Pre-compiled condition node ───────────────────────────────────────────────
// Leaf nodes use str1/str2/num1/val fields; composite nodes use children.
struct CppCond {
    CondType type = CondType::Unknown;

    // Composite: And, Or, Not, AtLeast
    std::vector<std::unique_ptr<CppCond>> children;
    int at_least_min = 0;   // AtLeast minimum

    // String fields
    std::string str1;   // attr_name, gender, race, code, state_name, category, vs_name, sym_name
    CompOp      op  = CompOp::Eq;

    // Comparison value
    AttrVal val;        // for Attribute, Observation comparisons (bool/double/string)

    // Numeric fields
    double num1 = 0.0;  // Age quantity (converted to seconds), or pre-computed Date epoch
    std::string unit;   // Age unit for display (already converted to seconds in num1)
    bool   has_val = false;   // whether val is meaningful
};

// Compile an R condition list (raw JSON parsed by jsonlite) → CppCond tree
std::unique_ptr<CppCond> compile_condition(SEXP rcond);
