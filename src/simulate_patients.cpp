// [[Rcpp::plugins(cpp17)]]
// R interface: simulate one patient using the C++ engine.
#include "simulation.h"

// Helper: extract AttrVal from an R SEXP
static AttrVal sexp_to_attr(SEXP v) {
    if (Rf_isNull(v) || Rf_length(v) == 0) return AttrVal{};
    switch (TYPEOF(v)) {
    case LGLSXP:  return (bool)(LOGICAL(v)[0] != 0);
    case INTSXP:  return (double)INTEGER(v)[0];
    case REALSXP: return REAL(v)[0];
    case STRSXP:  return std::string(CHAR(STRING_ELT(v, 0)));
    default:      return AttrVal{};
    }
}

// Initialise a PersonRecord from the named attribute list passed from R.
// attributes_r: named list (person@attributes)
// birth_num: as.numeric(birth_date) — already a double epoch value
static PersonRecord init_person_record(Rcpp::List attributes_r,
                                       double birth_num) {
    PersonRecord rec;
    rec.birth_num = birth_num;
    rec.t_num     = birth_num;
    rec.is_alive  = true;

    Rcpp::CharacterVector nms = attributes_r.names();
    for (int i = 0; i < attributes_r.size(); ++i) {
        std::string key = Rcpp::as<std::string>(nms[i]);
        SEXP v = attributes_r[i];
        // birth_date stored as POSIXct — already handled via birth_num
        if (key == "birth_date") continue;
        rec.attributes[key] = sexp_to_attr(v);
    }
    return rec;
}

// ── Exported: simulate one patient ───────────────────────────────────────────
// [[Rcpp::export]]
Rcpp::List simulate_patient_cpp(Rcpp::List attributes_r,
                                double     birth_num,
                                double     t_end,
                                int        seed,
                                SEXP       modules_xptr) {
    Rcpp::XPtr<std::vector<CppModule>> ptr(modules_xptr);
    const std::vector<CppModule>& modules = *ptr;

    // Init patient
    PersonRecord rec = init_person_record(attributes_r, birth_num);

    // Patient-specific RNG
    std::mt19937 rng((uint32_t)seed);

    // Run simulation (build index per-call; for single patient this is fine)
    ModuleIndex idx = build_module_index(modules);
    simulate_life_cpp(rec, modules, t_end, rng, idx);

    // Return summary (full export comes in Fase 9)
    return Rcpp::List::create(
        Rcpp::Named("is_alive")      = rec.is_alive,
        Rcpp::Named("n_encounters")  = (int)rec.encounters.size(),
        Rcpp::Named("n_conditions")  = (int)rec.conditions.size(),
        Rcpp::Named("n_medications") = (int)rec.medications.size(),
        Rcpp::Named("n_procedures")  = (int)rec.procedures.size(),
        Rcpp::Named("n_obs")         = (int)rec.observations.size(),
        Rcpp::Named("n_allergies")   = (int)rec.allergies.size()
    );
}

// ── Exported: simulate N patients (returns data.frame of summaries) ───────────
// [[Rcpp::export]]
Rcpp::DataFrame simulate_population_cpp(Rcpp::List population_attrs,
                                        Rcpp::NumericVector birth_nums,
                                        double t_end,
                                        Rcpp::IntegerVector seeds,
                                        SEXP modules_xptr) {
    Rcpp::XPtr<std::vector<CppModule>> ptr(modules_xptr);
    const std::vector<CppModule>& modules = *ptr;

    int n = population_attrs.size();
    Rcpp::LogicalVector alive(n);
    Rcpp::IntegerVector n_enc(n), n_cond(n), n_med(n), n_proc(n);

    // Build index once for all patients
    ModuleIndex idx = build_module_index(modules);

    for (int i = 0; i < n; ++i) {
        Rcpp::List attrs = Rcpp::as<Rcpp::List>(population_attrs[i]);
        PersonRecord rec = init_person_record(attrs, birth_nums[i]);
        std::mt19937 rng((uint32_t)seeds[i]);
        simulate_life_cpp(rec, modules, t_end, rng, idx);
        alive[i]  = rec.is_alive;
        n_enc[i]  = (int)rec.encounters.size();
        n_cond[i] = (int)rec.conditions.size();
        n_med[i]  = (int)rec.medications.size();
        n_proc[i] = (int)rec.procedures.size();
    }

    return Rcpp::DataFrame::create(
        Rcpp::Named("is_alive")      = alive,
        Rcpp::Named("n_encounters")  = n_enc,
        Rcpp::Named("n_conditions")  = n_cond,
        Rcpp::Named("n_medications") = n_med,
        Rcpp::Named("n_procedures")  = n_proc
    );
}
