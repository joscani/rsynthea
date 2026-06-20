// [[Rcpp::plugins(cpp17)]]
#include "conditions.h"

// ── Safe scalar→string: handles chr, int, real (medical codes are often integers) ──
static std::string sexp_to_str(SEXP v) {
    if (Rf_isNull(v) || Rf_length(v) == 0) return "";
    if (TYPEOF(v) == STRSXP)                return Rcpp::as<std::string>(v);
    if (TYPEOF(v) == INTSXP)               return std::to_string(INTEGER(v)[0]);
    if (TYPEOF(v) == REALSXP)              {
        double d = REAL(v)[0];
        long long li = (long long)d;
        if ((double)li == d) return std::to_string(li);
        return std::to_string(d);
    }
    if (TYPEOF(v) == LGLSXP) return LOGICAL(v)[0] ? "TRUE" : "FALSE";
    return "";
}

// ── Unit → seconds conversion (matching R's .unit_secs) ──────────────────────
static double unit_to_secs(const std::string& u) {
    if (u == "years")  return 365.25 * 86400.0;
    if (u == "months") return 30.44  * 86400.0;
    if (u == "weeks")  return 7.0    * 86400.0;
    if (u == "days")   return 86400.0;
    if (u == "hours")  return 3600.0;
    return 86400.0;  // default days
}

// Helper: extract a string field from a named R list, "" if absent
static std::string rlist_str(Rcpp::List L, const char* key, const char* def = "") {
    if (L.containsElementNamed(key)) {
        SEXP v = L[key];
        if (TYPEOF(v) == STRSXP && Rf_length(v) > 0)
            return Rcpp::as<std::string>(v);
    }
    return def;
}

static double rlist_dbl(Rcpp::List L, const char* key, double def = 0.0) {
    if (L.containsElementNamed(key)) {
        SEXP v = L[key];
        if (Rf_length(v) > 0) return Rcpp::as<double>(v);
    }
    return def;
}

// ── Condition compiler ────────────────────────────────────────────────────────
std::unique_ptr<CppCond> compile_condition(SEXP rcond) {
    if (Rf_isNull(rcond) || !Rf_isVectorList(rcond)) return nullptr;

    Rcpp::List L(rcond);
    auto c = std::make_unique<CppCond>();

    std::string ct = rlist_str(L, "condition_type");

    // ── Composite ─────────────────────────────────────────────────────────
    if (ct == "And" || ct == "Or") {
        c->type = (ct == "And") ? CondType::And : CondType::Or;
        if (L.containsElementNamed("conditions")) {
            Rcpp::List subs(L["conditions"]);
            for (int i = 0; i < subs.size(); ++i) {
                auto child = compile_condition(subs[i]);
                if (child) c->children.push_back(std::move(child));
            }
        }
        return c;
    }
    if (ct == "Not") {
        c->type = CondType::Not;
        if (L.containsElementNamed("condition")) {
            auto child = compile_condition(L["condition"]);
            if (child) c->children.push_back(std::move(child));
        }
        return c;
    }
    if (ct == "AtLeast" || ct == "At Least") {
        c->type = CondType::AtLeast;
        c->at_least_min = (int)rlist_dbl(L, "minimum", 1.0);
        if (L.containsElementNamed("conditions")) {
            Rcpp::List subs(L["conditions"]);
            for (int i = 0; i < subs.size(); ++i) {
                auto child = compile_condition(subs[i]);
                if (child) c->children.push_back(std::move(child));
            }
        }
        return c;
    }

    // ── Leaf: True / False ────────────────────────────────────────────────
    if (ct == "True")  { c->type = CondType::True;  return c; }
    if (ct == "False") { c->type = CondType::False; return c; }

    // ── Leaf: Gender ──────────────────────────────────────────────────────
    if (ct == "Gender") {
        c->type = CondType::Gender;
        // Normalize to uppercase (matching R's toupper)
        std::string g = rlist_str(L, "gender");
        for (auto& ch : g) ch = (char)toupper((unsigned char)ch);
        c->str1 = g;
        return c;
    }

    // ── Leaf: Race ────────────────────────────────────────────────────────
    if (ct == "Race") {
        c->type = CondType::Race;
        std::string r = rlist_str(L, "race");
        for (auto& ch : r) ch = (char)tolower((unsigned char)ch);
        c->str1 = r;
        return c;
    }

    // ── Leaf: Socioeconomic Status ────────────────────────────────────────
    if (ct == "Socioeconomic Status") {
        c->type = CondType::SocioeconomicStatus;
        std::string s = rlist_str(L, "category");
        for (auto& ch : s) ch = (char)tolower((unsigned char)ch);
        c->str1 = s;
        return c;
    }

    // ── Leaf: Age ─────────────────────────────────────────────────────────
    if (ct == "Age") {
        c->type = CondType::Age;
        c->op   = parse_comp_op(rlist_str(L, "operator", "=="));
        c->unit = rlist_str(L, "unit", "years");
        double qty = rlist_dbl(L, "quantity", 0.0);
        // Convert to years (stored as years for comparison with age-in-years)
        if (c->unit == "months") qty /= 12.0;
        else if (c->unit == "weeks")  qty /= 52.0;
        else if (c->unit == "days")   qty /= 365.25;
        c->num1 = qty;  // now in years
        return c;
    }

    // ── Leaf: Date ────────────────────────────────────────────────────────
    if (ct == "Date") {
        c->type = CondType::Date;
        c->op   = parse_comp_op(rlist_str(L, "operator", "=="));
        double y = rlist_dbl(L, "year",  1970.0);
        double m = rlist_dbl(L, "month", 1.0);
        double d = rlist_dbl(L, "day",   1.0);
        // Pre-compute epoch seconds (UTC) matching R's as.POSIXct("Y-M-D")
        // Simple Gregorian computation
        int Y = (int)y, M = (int)m, D = (int)d;
        // Days from Unix epoch to Y-M-D via a simple formula
        // (matches R's as.numeric(as.POSIXct(paste0(Y,"-",M,"-",D))) closely)
        // Use mktime equivalent: portable C approach
        struct tm t = {};
        t.tm_year = Y - 1900;
        t.tm_mon  = M - 1;
        t.tm_mday = D;
        t.tm_isdst = -1;
        time_t epoch = timegm(&t);  // UTC, matching R's default tz=UTC for POSIXct
        c->num1 = (double)epoch;
        return c;
    }

    // ── Leaf: Attribute ───────────────────────────────────────────────────
    if (ct == "Attribute") {
        c->type = CondType::Attribute;
        c->str1 = rlist_str(L, "attribute");
        std::string op_str = rlist_str(L, "operator", "");
        c->op = parse_comp_op(op_str);
        // value: could be bool, numeric, or string
        if (L.containsElementNamed("value")) {
            SEXP v = L["value"];
            if (!Rf_isNull(v) && Rf_length(v) > 0) {
                c->has_val = true;
                if (TYPEOF(v) == LGLSXP)
                    c->val = (bool)(LOGICAL(v)[0] != 0);
                else if (TYPEOF(v) == REALSXP || TYPEOF(v) == INTSXP)
                    c->val = Rcpp::as<double>(v);
                else
                    c->val = Rcpp::as<std::string>(v);
            }
        }
        if (L.containsElementNamed("value_code")) {
            SEXP vc = L["value_code"];
            if (Rf_isVectorList(vc) && Rf_length(vc) > 0) {
                Rcpp::List vcl(vc);
                std::string code_str = "";
                if (vcl.containsElementNamed("code"))
                    code_str = Rcpp::as<std::string>(vcl["code"]);
                c->val = code_str;
                c->has_val = true;
            }
        }
        return c;
    }

    // ── Leaf: Symptom ─────────────────────────────────────────────────────
    if (ct == "Symptom") {
        c->type = CondType::Symptom;
        c->str1 = rlist_str(L, "symptom");
        c->op   = parse_comp_op(rlist_str(L, "operator", ">="));
        c->num1 = rlist_dbl(L, "value", 0.0);
        return c;
    }

    // ── Leaf: Vital Sign ──────────────────────────────────────────────────
    if (ct == "Vital Sign") {
        c->type = CondType::VitalSign;
        c->str1 = rlist_str(L, "vital_sign");
        c->op   = parse_comp_op(rlist_str(L, "operator", ">="));
        c->num1 = rlist_dbl(L, "value", 0.0);
        return c;
    }

    // ── Leaf: Observation ─────────────────────────────────────────────────
    if (ct == "Observation") {
        c->type = CondType::Observation;
        c->op   = parse_comp_op(rlist_str(L, "operator", "=="));
        // Extract primary code
        if (L.containsElementNamed("codes")) {
            Rcpp::List codes(L["codes"]);
            if (codes.size() > 0 && Rf_isVectorList(codes[0])) {
                Rcpp::List code0(codes[0]);
                if (code0.containsElementNamed("code"))
                    c->str1 = sexp_to_str(code0["code"]);
            }
        }
        if (L.containsElementNamed("value")) {
            SEXP v = L["value"];
            if (!Rf_isNull(v) && Rf_length(v) > 0) {
                c->has_val = true;
                if (TYPEOF(v) == LGLSXP)
                    c->val = (bool)(LOGICAL(v)[0] != 0);
                else if (TYPEOF(v) == REALSXP || TYPEOF(v) == INTSXP)
                    c->val = Rcpp::as<double>(v);
                else
                    c->val = Rcpp::as<std::string>(v);
            }
        }
        return c;
    }

    // ── Leaf: Active* ─────────────────────────────────────────────────────
    auto parse_active = [&](CondType ct_val) {
        c->type = ct_val;
        if (L.containsElementNamed("codes")) {
            Rcpp::List codes(L["codes"]);
            if (codes.size() > 0 && Rf_isVectorList(codes[0])) {
                Rcpp::List code0(codes[0]);
                if (code0.containsElementNamed("code"))
                    c->str1 = sexp_to_str(code0["code"]);
            }
        }
    };
    if (ct == "Active Condition")  { parse_active(CondType::ActiveCondition);  return c; }
    if (ct == "Active Medication") { parse_active(CondType::ActiveMedication); return c; }
    if (ct == "Active CarePlan")   { parse_active(CondType::ActiveCarePlan);   return c; }
    if (ct == "Active Allergy")    { parse_active(CondType::ActiveAllergy);    return c; }

    // ── Leaf: PriorState ─────────────────────────────────────────────────
    if (ct == "PriorState") {
        c->type = CondType::PriorState;
        // rec.visited stores "__visited__<state_name>" — match that prefix
        c->str1 = std::string("__visited__") + rlist_str(L, "name");
        return c;
    }

    // Unknown → treat as True (permissive fallback)
    c->type = CondType::Unknown;
    return c;
}
