// [[Rcpp::plugins(cpp17)]]
#include "simulation.h"
#include "condition_eval.h"

static constexpr double TIMESTEP_SECS = 7.0 * 86400.0;  // 1 week

// ── Module index ──────────────────────────────────────────────────────────────
ModuleIndex build_module_index(const std::vector<CppModule>& modules) {
    ModuleIndex idx;
    idx.reserve(modules.size());
    for (const auto& m : modules)
        idx[m.name] = &m;
    return idx;
}

// ── Transition resolver ───────────────────────────────────────────────────────
std::string resolve_transition_cpp(const CppTransition& trans,
                                   PersonRecord& rec,
                                   std::mt19937& rng) {
    switch (trans.type) {

    case TransType::Direct:
        return trans.direct_target;

    case TransType::Distributed: {
        if (trans.dist_entries.empty()) return "";
        double total = 0.0;
        for (const auto& e : trans.dist_entries)
            total += e.is_attr ? get_attr_double(rec, e.attr_name, e.def_weight)
                               : e.fixed_weight;
        if (total <= 0) return trans.dist_entries.back().target;
        std::uniform_real_distribution<double> d(0.0, total);
        double r = d(rng), cum = 0.0;
        for (const auto& e : trans.dist_entries) {
            cum += e.is_attr ? get_attr_double(rec, e.attr_name, e.def_weight)
                             : e.fixed_weight;
            if (r < cum) return e.target;
        }
        return trans.dist_entries.back().target;
    }

    case TransType::Conditional:
        for (const auto& e : trans.cond_entries)
            if (!e.cond || evaluate_condition_cpp(e.cond.get(), rec))
                return e.target;
        return "";

    case TransType::Complex:
        for (const auto& e : trans.complex_entries) {
            if (e.cond && !evaluate_condition_cpp(e.cond.get(), rec)) continue;
            if (e.dists.empty()) return e.direct_target;
            double total = 0.0;
            for (const auto& d : e.dists)
                total += d.is_attr ? get_attr_double(rec, d.attr_name, d.def_weight)
                                   : d.fixed_weight;
            if (total <= 0) return e.dists.back().target;
            std::uniform_real_distribution<double> dist(0.0, total);
            double r = dist(rng), cum = 0.0;
            for (const auto& d : e.dists) {
                cum += d.is_attr ? get_attr_double(rec, d.attr_name, d.def_weight)
                                 : d.fixed_weight;
                if (r < cum) return d.target;
            }
            return e.dists.back().target;
        }
        return "";

    case TransType::LookupTable: {
        // Simplified: use default probabilities (full row-matching in later phase)
        if (trans.lookup_entries.empty()) return "";
        double total = 0.0;
        for (const auto& e : trans.lookup_entries) total += e.default_prob;
        if (total <= 0) return trans.lookup_entries.back().target;
        std::uniform_real_distribution<double> d(0.0, total);
        double r = d(rng), cum = 0.0;
        for (const auto& e : trans.lookup_entries) {
            cum += e.default_prob;
            if (r < cum) return e.target;
        }
        return trans.lookup_entries.back().target;
    }

    default: return "";
    }
}

// ── advance_module_cpp ────────────────────────────────────────────────────────
void advance_module_cpp(PersonRecord& rec, const CppModule& mod,
                        std::mt19937& rng, const ModuleIndex& idx) {
    // Use flat vector (O(1) direct access) if index is valid; else fallback map
    int midx = mod.module_idx;
    bool use_flat = (midx >= 0 && midx < (int)rec.module_states_flat.size());

    std::string current;
    if (use_flat) {
        current = rec.module_states_flat[midx];
        if (current.empty()) current = "Initial";
    } else {
        auto it = rec.module_current.find(mod.state_key);
        current = (it != rec.module_current.end()) ? it->second : std::string("Initial");
    }

    if (current == TERMINAL) return;

    for (int iter = 0; iter < 500; ++iter) {
        auto sit = mod.states.find(current);
        if (sit == mod.states.end()) break;
        const CppState& state = sit->second;

        // Wellness bypass (same as R: skip if already had wellness this timestep)
        if (state.is_wellness) {
            auto wit = rec.timers.find(state.wellness_key);
            if (wit != rec.timers.end() && wit->second >= rec.t_num) break;
        }

        std::string next = dispatch_state(state, rec, rng, idx);

        // Handle CallSubmodule request registered by handle_call_submodule
        auto csit = rec.submodule_calls.find(state.call_key);
        if (csit != rec.submodule_calls.end()) {
            auto mit = idx.find(csit->second);
            rec.submodule_calls.erase(csit);
            if (mit != idx.end())
                advance_module_cpp(rec, *mit->second, rng, idx);
        }

        if (next.empty()) {
            if (use_flat) rec.module_states_flat[midx] = TERMINAL;
            else          rec.module_current[mod.state_key] = TERMINAL;
            break;
        }
        if (next == current) break;  // Delay/Guard: stay

        current = next;
        if (use_flat) rec.module_states_flat[midx] = current;
        else          rec.module_current[mod.state_key] = current;
        if (!rec.is_alive) break;
    }
}

// ── simulate_life_cpp — takes pre-built index ─────────────────────────────────
void simulate_life_cpp(PersonRecord& rec, const std::vector<CppModule>& modules,
                       double t_end, std::mt19937& rng,
                       const ModuleIndex& idx) {

    // Size flat state vector (one slot per module; empty = uninitialized = "Initial")
    rec.module_states_flat.assign(modules.size(), std::string{});

    // Pre-compute non-submodule module indices (avoids is_submodule branch in hot loop)
    std::vector<int> active_idxs;
    active_idxs.reserve(modules.size());
    for (int i = 0; i < (int)modules.size(); ++i)
        if (!modules[i].is_submodule) active_idxs.push_back(i);

    double t_cur = rec.birth_num;
    while (t_cur <= t_end && rec.is_alive) {
        rec.t_num = t_cur;
        for (int i : active_idxs) {
            if (rec.module_states_flat[i] == TERMINAL) continue;
            advance_module_cpp(rec, modules[i], rng, idx);
            if (!rec.is_alive) break;
        }
        t_cur += TIMESTEP_SECS;
    }
}
