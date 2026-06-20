// [[Rcpp::plugins(cpp17)]]
#include "condition_eval.h"

// ── Attr→string for mixed comparisons ────────────────────────────────────────
static std::string attr_to_str(const AttrVal& v) {
    if (attr_is_string(v)) return attr_string(v);
    if (attr_is_bool(v))   return attr_bool(v) ? "TRUE" : "FALSE";
    if (attr_is_double(v)) {
        double d = attr_double(v);
        long long li = (long long)d;
        return ((double)li == d) ? std::to_string(li) : std::to_string(d);
    }
    return "";
}

bool compare_attr(const AttrVal& left, CompOp op, const AttrVal& right) {
    // Both double
    if (attr_is_double(left) && attr_is_double(right))
        return compare_num(attr_double(left), op, attr_double(right));
    // Both bool
    if (attr_is_bool(left) && attr_is_bool(right)) {
        bool l = attr_bool(left), r = attr_bool(right);
        return (op == CompOp::Eq) ? (l == r) : (op == CompOp::Ne) ? (l != r) : false;
    }
    // Left is double, right is string numeric
    if (attr_is_double(left) && attr_is_string(right)) {
        try { return compare_num(attr_double(left), op, std::stod(attr_string(right))); }
        catch (...) {}
    }
    // Both string (or fallback)
    std::string ls = attr_to_str(left), rs = attr_to_str(right);
    // Try numeric first
    try {
        return compare_num(std::stod(ls), op, std::stod(rs));
    } catch (...) {}
    switch (op) {
    case CompOp::Eq: return ls == rs;
    case CompOp::Ne: return ls != rs;
    case CompOp::Lt: return ls <  rs;
    case CompOp::Le: return ls <= rs;
    case CompOp::Gt: return ls >  rs;
    case CompOp::Ge: return ls >= rs;
    default: return false;
    }
}

// ── Main evaluator ────────────────────────────────────────────────────────────
bool evaluate_condition_cpp(const CppCond* cond, PersonRecord& rec) {
    if (!cond) return true;

    switch (cond->type) {

    case CondType::True:  return true;
    case CondType::False: return false;
    case CondType::Unknown: return true;  // permissive fallback

    // ── Composites ───────────────────────────────────────────────────────
    case CondType::And:
        for (const auto& child : cond->children)
            if (!evaluate_condition_cpp(child.get(), rec)) return false;
        return true;

    case CondType::Or:
        for (const auto& child : cond->children)
            if (evaluate_condition_cpp(child.get(), rec)) return true;
        return false;

    case CondType::Not:
        return !cond->children.empty() &&
               !evaluate_condition_cpp(cond->children[0].get(), rec);

    case CondType::AtLeast: {
        int n = 0;
        for (const auto& child : cond->children)
            if (evaluate_condition_cpp(child.get(), rec)) ++n;
        return n >= cond->at_least_min;
    }

    // ── Demographics ─────────────────────────────────────────────────────
    case CondType::Gender: {
        auto it = rec.attributes.find("gender");
        if (it == rec.attributes.end() || !attr_is_string(it->second)) return false;
        std::string g = attr_string(it->second);
        for (auto& c : g) c = (char)toupper((unsigned char)c);
        return g == cond->str1;
    }

    case CondType::Race: {
        auto it = rec.attributes.find("race");
        if (it == rec.attributes.end() || !attr_is_string(it->second)) return false;
        std::string r = attr_string(it->second);
        for (auto& c : r) c = (char)tolower((unsigned char)c);
        return r == cond->str1;
    }

    case CondType::SocioeconomicStatus: {
        auto it = rec.attributes.find("socioeconomic_status");
        if (it == rec.attributes.end() || !attr_is_string(it->second)) return false;
        std::string s = attr_string(it->second);
        for (auto& c : s) c = (char)tolower((unsigned char)c);
        return s == cond->str1;
    }

    // ── Age (cond->num1 in years) ─────────────────────────────────────────
    case CondType::Age: {
        double age = (rec.t_num - rec.birth_num) / (365.25 * 86400.0);
        return compare_num(age, cond->op, cond->num1);
    }

    // ── Date (cond->num1 is pre-computed epoch seconds) ───────────────────
    case CondType::Date:
        return compare_num(rec.t_num, cond->op, cond->num1);

    // ── Attribute ─────────────────────────────────────────────────────────
    case CondType::Attribute: {
        if (cond->op == CompOp::IsNil) {
            auto it = rec.attributes.find(cond->str1);
            return it == rec.attributes.end() || attr_is_null(it->second);
        }
        if (cond->op == CompOp::IsNotNil) {
            auto it = rec.attributes.find(cond->str1);
            return it != rec.attributes.end() && !attr_is_null(it->second);
        }
        auto it = rec.attributes.find(cond->str1);
        if (it == rec.attributes.end() || attr_is_null(it->second))
            return cond->op == CompOp::Ne;
        if (!cond->has_val) return !attr_is_null(it->second);
        return compare_attr(it->second, cond->op, cond->val);
    }

    // ── Symptom (numeric value) ───────────────────────────────────────────
    case CondType::Symptom: {
        auto it = rec.symptoms.find(cond->str1);
        double val = (it != rec.symptoms.end()) ? it->second.value : 0.0;
        return compare_num(val, cond->op, cond->num1);
    }

    // ── Vital sign ────────────────────────────────────────────────────────
    case CondType::VitalSign: {
        auto it = rec.vital_signs.find(cond->str1);
        if (it == rec.vital_signs.end()) return false;
        return compare_num(it->second.value, cond->op, cond->num1);
    }

    // ── Observation ───────────────────────────────────────────────────────
    case CondType::Observation: {
        const CppObservation* obs = rec.latest_observation(cond->str1);
        if (!obs) return false;
        if (!cond->has_val) return true;
        return compare_attr(obs->value, cond->op, cond->val);
    }

    // ── Active collections ────────────────────────────────────────────────
    case CondType::ActiveCondition:
        return rec.active_conditions.count(cond->str1) > 0;

    case CondType::ActiveMedication:
        return rec.active_medications.count(cond->str1) > 0;

    case CondType::ActiveCarePlan:
        return rec.active_careplans.count(cond->str1) > 0;

    case CondType::ActiveAllergy:
        return rec.active_conditions.count(cond->str1) > 0;

    // ── Prior state ───────────────────────────────────────────────────────
    case CondType::PriorState:
        return rec.visited.count(cond->str1) > 0;

    default: return true;
    }
}
