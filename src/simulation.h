#pragma once
#include "person_record.h"
#include "module.h"
#include <random>

// Type alias for the compiled module index (name → module pointer)
using ModuleIndex = std::unordered_map<std::string, const CppModule*>;

// Build an index from module name → const CppModule* for O(1) submodule lookup
ModuleIndex build_module_index(const std::vector<CppModule>& modules);

// Resolve a transition to the next state name given the current PersonRecord + RNG
std::string resolve_transition_cpp(const CppTransition& trans,
                                   PersonRecord& rec,
                                   std::mt19937& rng);

// Dispatch one state: execute its logic, return next state name.
// Returns "" for Terminal (or Death when already dead), returns current name to stay.
std::string dispatch_state(const CppState& state,
                           PersonRecord& rec,
                           std::mt19937& rng,
                           const ModuleIndex& idx);

// Advance one module through states until it hits a Delay/Guard/Terminal.
void advance_module_cpp(PersonRecord& rec,
                        const CppModule& mod,
                        std::mt19937& rng,
                        const ModuleIndex& idx);

// Simulate one patient's full life (pre-built index for reuse across patients).
void simulate_life_cpp(PersonRecord& rec,
                       const std::vector<CppModule>& modules,
                       double t_end,
                       std::mt19937& rng,
                       const ModuleIndex& idx);

// Helpers used by state handlers
inline double get_attr_double(const PersonRecord& rec,
                              const std::string& name,
                              double def = 0.0) {
    auto it = rec.attributes.find(name);
    if (it == rec.attributes.end()) return def;
    if (attr_is_double(it->second)) return attr_double(it->second);
    if (attr_is_bool(it->second))   return attr_bool(it->second) ? 1.0 : 0.0;
    return def;
}

inline std::string new_id(PersonRecord& rec) {
    // Simple sequential IDs within a patient
    return std::to_string(rec.encounters.size() + rec.conditions.size() +
                          rec.medications.size() + rec.procedures.size() + 1);
}
