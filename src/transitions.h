#pragma once
#include "conditions.h"

// ── Transition types ──────────────────────────────────────────────────────────
enum class TransType : uint8_t {
    None, Direct, Distributed, Conditional, Complex, LookupTable
};

// Entry in a distributed transition
// weight is either a fixed double or read from person.attributes[attr_name]
struct DistEntry {
    std::string target;
    double      fixed_weight = 0.0;
    std::string attr_name;      // if non-empty, weight = attrs[attr_name] ?? def_weight
    double      def_weight  = 0.0;
    bool        is_attr = false;
};

// Entry in a conditional transition
struct CondEntry {
    std::unique_ptr<CppCond> cond;   // null → else branch
    std::string target;
};

// Entry in a complex transition (condition → distributed sub-transitions)
struct ComplexEntry {
    std::unique_ptr<CppCond> cond;   // null → else branch
    std::vector<DistEntry>   dists;
    std::string direct_target;       // if dists is empty
};

// Entry in a lookup_table transition
struct LookupEntry {
    std::string target;
    double      default_prob = 0.0;
};

// ── Pre-compiled transition ───────────────────────────────────────────────────
struct CppTransition {
    TransType type = TransType::None;

    // Direct
    std::string direct_target;

    // Distributed
    std::vector<DistEntry> dist_entries;

    // Conditional
    std::vector<CondEntry> cond_entries;

    // Complex
    std::vector<ComplexEntry> complex_entries;

    // LookupTable (table_name + per-transition defaults; row matching kept in C++ too)
    std::string lookup_table_name;
    std::vector<LookupEntry> lookup_entries;
};

// Compile R transition list (output of parse_transition()) → CppTransition
CppTransition compile_transition(SEXP rtrans);
