// [[Rcpp::plugins(cpp17)]]
#include "transitions.h"

static std::string rlist_str(Rcpp::List L, const char* key, const char* def = "") {
    if (L.containsElementNamed(key)) {
        SEXP v = L[key];
        if (TYPEOF(v) == STRSXP && Rf_length(v) > 0)
            return Rcpp::as<std::string>(v);
    }
    return def;
}
static double safe_dbl(SEXP v, double def = 0.0) {
    if (Rf_isNull(v) || Rf_length(v) == 0) return def;
    if (TYPEOF(v) == REALSXP) return REAL(v)[0];
    if (TYPEOF(v) == INTSXP)  return (double)INTEGER(v)[0];
    if (TYPEOF(v) == STRSXP)  {
        try { return std::stod(Rcpp::as<std::string>(v)); } catch(...) {}
    }
    return def;
}
static double rlist_dbl(Rcpp::List L, const char* key, double def = 0.0) {
    if (L.containsElementNamed(key)) return safe_dbl(L[key], def);
    return def;
}

// Parse one weight spec: either a plain numeric or {attribute, default}
static DistEntry parse_dist_entry(Rcpp::List entry) {
    DistEntry de;
    de.target = rlist_str(entry, "transition");

    SEXP dist_sexp = entry.containsElementNamed("distribution") ? entry["distribution"] : R_NilValue;
    if (Rf_isNull(dist_sexp)) {
        de.fixed_weight = rlist_dbl(entry, "distribution", 0.0);
        return de;
    }
    if (TYPEOF(dist_sexp) == REALSXP || TYPEOF(dist_sexp) == INTSXP) {
        de.fixed_weight = Rcpp::as<double>(dist_sexp);
        return de;
    }
    if (Rf_isVectorList(dist_sexp)) {
        Rcpp::List dL(dist_sexp);
        if (dL.containsElementNamed("attribute")) {
            de.is_attr    = true;
            de.attr_name  = Rcpp::as<std::string>(dL["attribute"]);
            de.def_weight = rlist_dbl(dL, "default", 0.0);
        } else {
            de.fixed_weight = rlist_dbl(dL, "default", 0.0);
        }
    }
    return de;
}

CppTransition compile_transition(SEXP rtrans) {
    CppTransition t;
    if (Rf_isNull(rtrans) || !Rf_isVectorList(rtrans)) return t;

    Rcpp::List L(rtrans);
    std::string ttype = rlist_str(L, "type");

    if (ttype == "direct") {
        t.type = TransType::Direct;
        t.direct_target = rlist_str(L, "target");
        return t;
    }

    if (ttype == "distributed") {
        t.type = TransType::Distributed;
        if (L.containsElementNamed("entries")) {
            Rcpp::List entries(L["entries"]);
            t.dist_entries.reserve(entries.size());
            for (int i = 0; i < entries.size(); ++i) {
                if (Rf_isVectorList(entries[i]))
                    t.dist_entries.push_back(parse_dist_entry(Rcpp::List(entries[i])));
            }
        }
        return t;
    }

    if (ttype == "conditional") {
        t.type = TransType::Conditional;
        if (L.containsElementNamed("entries")) {
            Rcpp::List entries(L["entries"]);
            t.cond_entries.reserve(entries.size());
            for (int i = 0; i < entries.size(); ++i) {
                if (!Rf_isVectorList(entries[i])) continue;
                Rcpp::List e(entries[i]);
                CondEntry ce;
                ce.target = rlist_str(e, "transition");
                if (e.containsElementNamed("condition"))
                    ce.cond = compile_condition(e["condition"]);
                t.cond_entries.push_back(std::move(ce));
            }
        }
        return t;
    }

    if (ttype == "complex") {
        t.type = TransType::Complex;
        if (L.containsElementNamed("entries")) {
            Rcpp::List entries(L["entries"]);
            t.complex_entries.reserve(entries.size());
            for (int i = 0; i < entries.size(); ++i) {
                if (!Rf_isVectorList(entries[i])) continue;
                Rcpp::List e(entries[i]);
                ComplexEntry ce;
                if (e.containsElementNamed("condition"))
                    ce.cond = compile_condition(e["condition"]);
                // distributions sub-entries
                if (e.containsElementNamed("distributions")) {
                    Rcpp::List dists(e["distributions"]);
                    ce.dists.reserve(dists.size());
                    for (int j = 0; j < dists.size(); ++j)
                        if (Rf_isVectorList(dists[j]))
                            ce.dists.push_back(parse_dist_entry(Rcpp::List(dists[j])));
                }
                if (ce.dists.empty())
                    ce.direct_target = rlist_str(e, "transition");
                t.complex_entries.push_back(std::move(ce));
            }
        }
        return t;
    }

    if (ttype == "lookup_table") {
        t.type = TransType::LookupTable;
        if (L.containsElementNamed("entries")) {
            Rcpp::List entries(L["entries"]);
            if (entries.size() > 0 && Rf_isVectorList(entries[0]))
                t.lookup_table_name = rlist_str(Rcpp::List(entries[0]),
                                                "lookup_table_name");
            t.lookup_entries.reserve(entries.size());
            for (int i = 0; i < entries.size(); ++i) {
                if (!Rf_isVectorList(entries[i])) continue;
                Rcpp::List e(entries[i]);
                LookupEntry le;
                le.target       = rlist_str(e, "transition");
                le.default_prob = rlist_dbl(e, "default_probability", 0.0);
                t.lookup_entries.push_back(std::move(le));
            }
        }
        return t;
    }

    return t;  // TransType::None
}
