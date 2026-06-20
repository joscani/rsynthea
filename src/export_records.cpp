// [[Rcpp::plugins(cpp17)]]
// Exports a simulated population (vector<PersonRecord>) to R vectors suitable
// for tibble construction. Returns a named list matching the schema in export.R.
#include "simulation.h"

// ── AttrVal → R SEXP ──────────────────────────────────────────────────────────
static SEXP attrval_to_sexp(const AttrVal& v) {
    if (attr_is_null(v))   return Rf_ScalarLogical(NA_LOGICAL);
    if (attr_is_bool(v))   return Rf_ScalarLogical(attr_bool(v) ? 1 : 0);
    if (attr_is_double(v)) return Rf_ScalarReal(attr_double(v));
    if (attr_is_string(v)) return Rf_mkString(attr_string(v).c_str());
    return Rf_ScalarLogical(NA_LOGICAL);
}

// ── Simulation + export: one call from R ─────────────────────────────────────
// [[Rcpp::export]]
Rcpp::List generate_and_export_cpp(
    Rcpp::List         population_attrs,   // list of named lists (demographics)
    Rcpp::NumericVector birth_nums,        // as.numeric(birth_date) per patient
    Rcpp::CharacterVector patient_ids,     // pre-generated IDs (from R's digest)
    double             t_end,
    Rcpp::IntegerVector seeds,
    SEXP               modules_xptr
) {
    Rcpp::XPtr<std::vector<CppModule>> ptr(modules_xptr);
    const std::vector<CppModule>& modules = *ptr;
    int n = population_attrs.size();

    // Build module index once
    ModuleIndex idx = build_module_index(modules);

    // ── Simulate all patients ────────────────────────────────────────────────
    std::vector<PersonRecord> records(n);
    for (int i = 0; i < n; ++i) {
        Rcpp::List attrs = Rcpp::as<Rcpp::List>(population_attrs[i]);
        // Init record
        PersonRecord& rec = records[i];
        rec.birth_num = birth_nums[i];
        rec.t_num     = birth_nums[i];
        rec.is_alive  = true;
        // Copy demographics into attributes map; vital signs prefixed ".vs."
        Rcpp::CharacterVector nms = attrs.names();
        for (int j = 0; j < attrs.size(); ++j) {
            std::string key = Rcpp::as<std::string>(nms[j]);
            if (key == "birth_date") continue;
            SEXP v = attrs[j];
            if (Rf_isNull(v) || Rf_length(v) == 0) continue;
            // Vital signs passed as ".vs.<name>" from R
            if (key.size() > 4 && key.substr(0, 4) == ".vs.") {
                std::string vs_name = key.substr(4);
                double val = (TYPEOF(v) == REALSXP) ? REAL(v)[0]
                           : (TYPEOF(v) == INTSXP)  ? (double)INTEGER(v)[0] : 0.0;
                rec.vital_signs[vs_name] = CppVitalSign{val, "%", rec.birth_num};
                continue;
            }
            switch (TYPEOF(v)) {
            case LGLSXP:  rec.attributes[key] = (bool)(LOGICAL(v)[0] != 0); break;
            case INTSXP:  rec.attributes[key] = (double)INTEGER(v)[0];       break;
            case REALSXP: rec.attributes[key] = REAL(v)[0];                  break;
            case STRSXP:  rec.attributes[key] = std::string(CHAR(STRING_ELT(v,0))); break;
            default: break;
            }
        }
        std::mt19937 rng((uint32_t)seeds[i]);
        simulate_life_cpp(rec, modules, t_end, rng, idx);
    }

    // ── Export patients table ─────────────────────────────────────────────────
    Rcpp::CharacterVector pat_id_v(n), gender_v(n), race_v(n), ethnicity_v(n),
                          first_name_v(n), last_name_v(n), state_v(n), city_v(n);
    Rcpp::NumericVector   birth_v(n), death_v(n);
    Rcpp::LogicalVector   alive_v(n);

    auto get_str = [](const PersonRecord& rec, const std::string& key,
                      const std::string& def = "") -> std::string {
        auto it = rec.attributes.find(key);
        if (it == rec.attributes.end() || !attr_is_string(it->second)) return def;
        return attr_string(it->second);
    };
    auto get_dbl = [](const PersonRecord& rec, const std::string& key) -> double {
        auto it = rec.attributes.find(key);
        if (it == rec.attributes.end()) return NA_REAL;
        if (attr_is_double(it->second)) return attr_double(it->second);
        return NA_REAL;
    };

    for (int i = 0; i < n; ++i) {
        const PersonRecord& rec = records[i];
        pat_id_v[i]    = Rcpp::as<std::string>(patient_ids[i]);
        birth_v[i]     = rec.birth_num;
        death_v[i]     = get_dbl(rec, "death_date");
        alive_v[i]     = rec.is_alive;
        gender_v[i]    = get_str(rec, "gender");
        race_v[i]      = get_str(rec, "race");
        ethnicity_v[i] = get_str(rec, "ethnicity");
        first_name_v[i]= get_str(rec, "first_name");
        last_name_v[i] = get_str(rec, "last_name");
        state_v[i]     = get_str(rec, "state");
        city_v[i]      = get_str(rec, "city");
    }
    // Tag as POSIXct
    Rcpp::CharacterVector posix_cls = Rcpp::CharacterVector::create("POSIXct","POSIXt");
    birth_v.attr("class") = posix_cls; birth_v.attr("tzone") = "UTC";
    death_v.attr("class") = posix_cls; death_v.attr("tzone") = "UTC";

    Rcpp::DataFrame patients_df = Rcpp::DataFrame::create(
        Rcpp::Named("id")         = pat_id_v,
        Rcpp::Named("birth_date") = birth_v,
        Rcpp::Named("death_date") = death_v,
        Rcpp::Named("is_alive")   = alive_v,
        Rcpp::Named("gender")     = gender_v,
        Rcpp::Named("race")       = race_v,
        Rcpp::Named("ethnicity")  = ethnicity_v,
        Rcpp::Named("first_name") = first_name_v,
        Rcpp::Named("last_name")  = last_name_v,
        Rcpp::Named("state")      = state_v,
        Rcpp::Named("city")       = city_v,
        Rcpp::Named("stringsAsFactors") = false
    );

    // ── Export clinical tables ─────────────────────────────────────────────────
    // Pre-count totals
    size_t n_enc = 0, n_cond = 0, n_med = 0, n_proc = 0,
           n_obs = 0, n_alg = 0, n_vac = 0, n_cp = 0;
    for (const auto& rec : records) {
        n_enc  += rec.encounters.size();
        n_cond += rec.conditions.size();
        n_med  += rec.medications.size();
        n_proc += rec.procedures.size();
        n_obs  += rec.observations.size();
        n_alg  += rec.allergies.size();
        n_vac  += rec.vaccines.size();
        n_cp   += rec.careplans.size();
    }

    // Helper: generate sequential IDs like "e1", "c3", etc.
    auto make_id = [](char prefix, size_t idx) -> std::string {
        return std::string(1, prefix) + std::to_string(idx);
    };

    // ── Encounters ────────────────────────────────────────────────────────────
    Rcpp::CharacterVector enc_id(n_enc), enc_pat(n_enc), enc_class(n_enc),
                          enc_code(n_enc), enc_sys(n_enc), enc_desc(n_enc);
    Rcpp::NumericVector   enc_start(n_enc), enc_end(n_enc);
    {
        size_t k = 0;
        for (int i = 0; i < n; ++i) {
            const std::string& pid = Rcpp::as<std::string>(patient_ids[i]);
            for (const auto& e : records[i].encounters) {
                enc_id[k]    = make_id('e', k+1);
                enc_pat[k]   = pid;
                enc_start[k] = e.start;
                enc_end[k]   = e.end > 0 ? e.end : e.start;
                enc_class[k] = e.encounter_class;
                enc_code[k]  = e.codes.empty() ? "" : e.codes[0].code;
                enc_sys[k]   = e.codes.empty() ? "" : e.codes[0].system;
                enc_desc[k]  = e.codes.empty() ? "" : e.codes[0].display;
                ++k;
            }
        }
    }
    enc_start.attr("class") = posix_cls; enc_start.attr("tzone") = "UTC";
    enc_end.attr("class")   = posix_cls; enc_end.attr("tzone")   = "UTC";
    Rcpp::DataFrame enc_df = Rcpp::DataFrame::create(
        Rcpp::Named("id")              = enc_id,
        Rcpp::Named("patient_id")      = enc_pat,
        Rcpp::Named("time")            = enc_start,
        Rcpp::Named("end_time")        = enc_end,
        Rcpp::Named("encounter_class") = enc_class,
        Rcpp::Named("code")            = enc_code,
        Rcpp::Named("code_system")     = enc_sys,
        Rcpp::Named("description")     = enc_desc,
        Rcpp::Named("stringsAsFactors") = false
    );

    // ── Conditions ────────────────────────────────────────────────────────────
    Rcpp::CharacterVector cond_id(n_cond), cond_pat(n_cond), cond_code(n_cond),
                          cond_sys(n_cond), cond_desc(n_cond);
    Rcpp::NumericVector   cond_onset(n_cond), cond_end(n_cond);
    Rcpp::LogicalVector   cond_active(n_cond);
    {
        size_t k = 0;
        for (int i = 0; i < n; ++i) {
            const std::string& pid = Rcpp::as<std::string>(patient_ids[i]);
            for (const auto& c : records[i].conditions) {
                cond_id[k]     = make_id('c', k+1);
                cond_pat[k]    = pid;
                cond_onset[k]  = c.onset;
                cond_end[k]    = c.abated > 0 ? c.abated : NA_REAL;
                cond_active[k] = (c.abated == 0.0);
                cond_code[k]   = c.codes.empty() ? "" : c.codes[0].code;
                cond_sys[k]    = c.codes.empty() ? "" : c.codes[0].system;
                cond_desc[k]   = c.codes.empty() ? "" : c.codes[0].display;
                ++k;
            }
        }
    }
    cond_onset.attr("class") = posix_cls; cond_onset.attr("tzone") = "UTC";
    cond_end.attr("class")   = posix_cls; cond_end.attr("tzone")   = "UTC";
    Rcpp::DataFrame cond_df = Rcpp::DataFrame::create(
        Rcpp::Named("id")          = cond_id,
        Rcpp::Named("patient_id")  = cond_pat,
        Rcpp::Named("onset_time")  = cond_onset,
        Rcpp::Named("end_time")    = cond_end,
        Rcpp::Named("is_active")   = cond_active,
        Rcpp::Named("code")        = cond_code,
        Rcpp::Named("code_system") = cond_sys,
        Rcpp::Named("description") = cond_desc,
        Rcpp::Named("stringsAsFactors") = false
    );

    // ── Medications ───────────────────────────────────────────────────────────
    Rcpp::CharacterVector med_id(n_med), med_pat(n_med), med_code(n_med),
                          med_sys(n_med), med_desc(n_med);
    Rcpp::NumericVector   med_start(n_med), med_end(n_med);
    Rcpp::LogicalVector   med_active(n_med);
    {
        size_t k = 0;
        for (int i = 0; i < n; ++i) {
            const std::string& pid = Rcpp::as<std::string>(patient_ids[i]);
            for (const auto& m : records[i].medications) {
                med_id[k]     = make_id('m', k+1);
                med_pat[k]    = pid;
                med_start[k]  = m.start;
                med_end[k]    = m.stop > 0 ? m.stop : NA_REAL;
                med_active[k] = m.active;
                med_code[k]   = m.codes.empty() ? "" : m.codes[0].code;
                med_sys[k]    = m.codes.empty() ? "" : m.codes[0].system;
                med_desc[k]   = m.codes.empty() ? "" : m.codes[0].display;
                ++k;
            }
        }
    }
    med_start.attr("class") = posix_cls; med_start.attr("tzone") = "UTC";
    med_end.attr("class")   = posix_cls; med_end.attr("tzone")   = "UTC";
    Rcpp::DataFrame med_df = Rcpp::DataFrame::create(
        Rcpp::Named("id")          = med_id,
        Rcpp::Named("patient_id")  = med_pat,
        Rcpp::Named("start_time")  = med_start,
        Rcpp::Named("end_time")    = med_end,
        Rcpp::Named("is_active")   = med_active,
        Rcpp::Named("code")        = med_code,
        Rcpp::Named("code_system") = med_sys,
        Rcpp::Named("description") = med_desc,
        Rcpp::Named("stringsAsFactors") = false
    );

    // ── Procedures ────────────────────────────────────────────────────────────
    Rcpp::CharacterVector proc_id(n_proc), proc_pat(n_proc), proc_code(n_proc),
                          proc_sys(n_proc), proc_desc(n_proc);
    Rcpp::NumericVector   proc_time(n_proc);
    {
        size_t k = 0;
        for (int i = 0; i < n; ++i) {
            const std::string& pid = Rcpp::as<std::string>(patient_ids[i]);
            for (const auto& p : records[i].procedures) {
                proc_id[k]   = make_id('p', k+1);
                proc_pat[k]  = pid;
                proc_time[k] = p.time;
                proc_code[k] = p.codes.empty() ? "" : p.codes[0].code;
                proc_sys[k]  = p.codes.empty() ? "" : p.codes[0].system;
                proc_desc[k] = p.codes.empty() ? "" : p.codes[0].display;
                ++k;
            }
        }
    }
    proc_time.attr("class") = posix_cls; proc_time.attr("tzone") = "UTC";
    Rcpp::DataFrame proc_df = Rcpp::DataFrame::create(
        Rcpp::Named("id")          = proc_id,
        Rcpp::Named("patient_id")  = proc_pat,
        Rcpp::Named("time")        = proc_time,
        Rcpp::Named("code")        = proc_code,
        Rcpp::Named("code_system") = proc_sys,
        Rcpp::Named("description") = proc_desc,
        Rcpp::Named("stringsAsFactors") = false
    );

    // ── Observations ──────────────────────────────────────────────────────────
    Rcpp::CharacterVector obs_id(n_obs), obs_pat(n_obs), obs_code(n_obs),
                          obs_sys(n_obs), obs_desc(n_obs), obs_unit(n_obs),
                          obs_cat(n_obs);
    Rcpp::NumericVector   obs_time(n_obs);
    Rcpp::List            obs_value(n_obs);  // heterogeneous
    {
        size_t k = 0;
        for (int i = 0; i < n; ++i) {
            const std::string& pid = Rcpp::as<std::string>(patient_ids[i]);
            for (const auto& o : records[i].observations) {
                obs_id[k]    = make_id('o', k+1);
                obs_pat[k]   = pid;
                obs_time[k]  = o.time;
                obs_code[k]  = o.codes.empty() ? "" : o.codes[0].code;
                obs_sys[k]   = o.codes.empty() ? "" : o.codes[0].system;
                obs_desc[k]  = o.codes.empty() ? "" : o.codes[0].display;
                obs_unit[k]  = o.unit;
                obs_cat[k]   = o.category;
                obs_value[k] = attrval_to_sexp(o.value);
                ++k;
            }
        }
    }
    obs_time.attr("class") = posix_cls; obs_time.attr("tzone") = "UTC";
    Rcpp::DataFrame obs_df = Rcpp::DataFrame::create(
        Rcpp::Named("id")          = obs_id,
        Rcpp::Named("patient_id")  = obs_pat,
        Rcpp::Named("time")        = obs_time,
        Rcpp::Named("code")        = obs_code,
        Rcpp::Named("code_system") = obs_sys,
        Rcpp::Named("description") = obs_desc,
        Rcpp::Named("unit")        = obs_unit,
        Rcpp::Named("category")    = obs_cat,
        Rcpp::Named("stringsAsFactors") = false
    );

    // ── Allergies ─────────────────────────────────────────────────────────────
    Rcpp::CharacterVector alg_id(n_alg), alg_pat(n_alg), alg_code(n_alg),
                          alg_sys(n_alg), alg_desc(n_alg);
    Rcpp::NumericVector   alg_onset(n_alg), alg_end(n_alg);
    {
        size_t k = 0;
        for (int i = 0; i < n; ++i) {
            const std::string& pid = Rcpp::as<std::string>(patient_ids[i]);
            for (const auto& a : records[i].allergies) {
                alg_id[k]    = make_id('a', k+1);
                alg_pat[k]   = pid;
                alg_onset[k] = a.onset;
                alg_end[k]   = a.abated > 0 ? a.abated : NA_REAL;
                alg_code[k]  = a.codes.empty() ? "" : a.codes[0].code;
                alg_sys[k]   = a.codes.empty() ? "" : a.codes[0].system;
                alg_desc[k]  = a.codes.empty() ? "" : a.codes[0].display;
                ++k;
            }
        }
    }
    alg_onset.attr("class") = posix_cls; alg_onset.attr("tzone") = "UTC";
    alg_end.attr("class")   = posix_cls; alg_end.attr("tzone")   = "UTC";
    Rcpp::DataFrame alg_df = Rcpp::DataFrame::create(
        Rcpp::Named("id")          = alg_id,
        Rcpp::Named("patient_id")  = alg_pat,
        Rcpp::Named("onset_time")  = alg_onset,
        Rcpp::Named("end_time")    = alg_end,
        Rcpp::Named("code")        = alg_code,
        Rcpp::Named("code_system") = alg_sys,
        Rcpp::Named("description") = alg_desc,
        Rcpp::Named("stringsAsFactors") = false
    );

    // ── Vaccines ──────────────────────────────────────────────────────────────
    Rcpp::CharacterVector vac_id(n_vac), vac_pat(n_vac), vac_code(n_vac),
                          vac_sys(n_vac), vac_desc(n_vac);
    Rcpp::NumericVector   vac_time(n_vac);
    {
        size_t k = 0;
        for (int i = 0; i < n; ++i) {
            const std::string& pid = Rcpp::as<std::string>(patient_ids[i]);
            for (const auto& v : records[i].vaccines) {
                vac_id[k]   = make_id('v', k+1);
                vac_pat[k]  = pid;
                vac_time[k] = v.time;
                vac_code[k] = v.codes.empty() ? "" : v.codes[0].code;
                vac_sys[k]  = v.codes.empty() ? "" : v.codes[0].system;
                vac_desc[k] = v.codes.empty() ? "" : v.codes[0].display;
                ++k;
            }
        }
    }
    vac_time.attr("class") = posix_cls; vac_time.attr("tzone") = "UTC";
    Rcpp::DataFrame vac_df = Rcpp::DataFrame::create(
        Rcpp::Named("id")          = vac_id,
        Rcpp::Named("patient_id")  = vac_pat,
        Rcpp::Named("time")        = vac_time,
        Rcpp::Named("code")        = vac_code,
        Rcpp::Named("code_system") = vac_sys,
        Rcpp::Named("description") = vac_desc,
        Rcpp::Named("stringsAsFactors") = false
    );

    // ── Return all tables ─────────────────────────────────────────────────────
    return Rcpp::List::create(
        Rcpp::Named("patients")      = patients_df,
        Rcpp::Named("encounters")    = enc_df,
        Rcpp::Named("conditions")    = cond_df,
        Rcpp::Named("medications")   = med_df,
        Rcpp::Named("procedures")    = proc_df,
        Rcpp::Named("observations")  = obs_df,
        Rcpp::Named("allergies")     = alg_df,
        Rcpp::Named("immunizations") = vac_df
    );
}
