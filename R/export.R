# R/export.R

export_population <- function(patients, output_dir = NULL) {
  tbls <- list(
    patients      = .patients_tbl(patients),
    encounters    = .encounters_tbl(patients),
    conditions    = .conditions_tbl(patients),
    medications   = .medications_tbl(patients),
    procedures    = .procedures_tbl(patients),
    observations  = .observations_tbl(patients),
    immunizations = .immunizations_tbl(patients),
    allergies     = .allergies_tbl(patients),
    careplans     = .careplans_tbl(patients)
  )
  if (!is.null(output_dir)) {
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
    for (nm in names(tbls)) {
      utils::write.csv(tbls[[nm]], file.path(output_dir, paste0(nm, ".csv")),
                       row.names = FALSE)
    }
  }
  tbls
}

# Helper: flatten a list of per-patient lists into one tibble
.flatten_tbl <- function(patients, fn) {
  rows <- lapply(patients, fn)
  dplyr::bind_rows(rows)
}

.first_code  <- function(codes) if (length(codes) > 0) codes[[1]]@code    else NA_character_
.first_sys   <- function(codes) if (length(codes) > 0) codes[[1]]@system  else NA_character_
.first_disp  <- function(codes) if (length(codes) > 0) codes[[1]]@display else NA_character_

.patients_tbl <- function(patients) {
  .flatten_tbl(patients, function(p) {
    a <- p@attributes
    tibble::tibble(
      id         = p@id,
      birth_date = a[["birth_date"]] %||% NA,
      death_date = a[["death_date"]] %||% NA,
      is_alive   = p@is_alive,
      gender     = a[["gender"]]     %||% NA_character_,
      race       = a[["race"]]       %||% NA_character_,
      ethnicity  = a[["ethnicity"]]  %||% NA_character_,
      first_name = a[["first_name"]] %||% NA_character_,
      last_name  = a[["last_name"]]  %||% NA_character_,
      state      = a[["state"]]      %||% NA_character_,
      city       = a[["city"]]       %||% NA_character_
    )
  })
}

.encounters_tbl <- function(patients) {
  .flatten_tbl(patients, function(p) {
    if (length(p@health_record@encounters) == 0) return(tibble::tibble())
    dplyr::bind_rows(lapply(p@health_record@encounters, function(e) {
      tibble::tibble(
        id              = e@id,
        patient_id      = p@id,
        time            = e@time,
        end_time        = e@end_time %||% NA,
        encounter_class = e@encounter_class,
        code            = .first_code(e@codes),
        code_system     = .first_sys(e@codes),
        description     = .first_disp(e@codes)
      )
    }))
  })
}

.conditions_tbl <- function(patients) {
  .flatten_tbl(patients, function(p) {
    if (length(p@health_record@conditions) == 0) return(tibble::tibble())
    dplyr::bind_rows(lapply(p@health_record@conditions, function(c) {
      tibble::tibble(
        id          = c@id,
        patient_id  = p@id,
        onset_time  = c@time,
        end_time    = c@end_time %||% NA,
        is_active   = c@is_active,
        code        = .first_code(c@codes),
        code_system = .first_sys(c@codes),
        description = .first_disp(c@codes)
      )
    }))
  })
}

.medications_tbl <- function(patients) {
  .flatten_tbl(patients, function(p) {
    if (length(p@health_record@medications) == 0) return(tibble::tibble())
    dplyr::bind_rows(lapply(p@health_record@medications, function(m) {
      tibble::tibble(
        id          = m@id,
        patient_id  = p@id,
        start_time  = m@time,
        end_time    = m@end_time %||% NA,
        is_active   = m@is_active,
        code        = .first_code(m@codes),
        code_system = .first_sys(m@codes),
        description = .first_disp(m@codes)
      )
    }))
  })
}

.procedures_tbl <- function(patients) {
  .flatten_tbl(patients, function(p) {
    if (length(p@health_record@procedures) == 0) return(tibble::tibble())
    dplyr::bind_rows(lapply(p@health_record@procedures, function(pr) {
      tibble::tibble(
        id          = pr@id,
        patient_id  = p@id,
        time        = pr@time,
        code        = .first_code(pr@codes),
        code_system = .first_sys(pr@codes),
        description = .first_disp(pr@codes)
      )
    }))
  })
}

.observations_tbl <- function(patients) {
  .flatten_tbl(patients, function(p) {
    if (length(p@health_record@observations) == 0) return(tibble::tibble())
    dplyr::bind_rows(lapply(p@health_record@observations, function(o) {
      tibble::tibble(
        id          = o@id,
        patient_id  = p@id,
        time        = o@time,
        value       = as.character(o@value %||% NA),
        unit        = o@unit     %||% NA_character_,
        category    = o@category %||% NA_character_,
        code        = .first_code(o@codes),
        code_system = .first_sys(o@codes),
        description = .first_disp(o@codes)
      )
    }))
  })
}

.immunizations_tbl <- function(patients) {
  .flatten_tbl(patients, function(p) {
    if (length(p@health_record@immunizations) == 0) return(tibble::tibble())
    dplyr::bind_rows(lapply(p@health_record@immunizations, function(i) {
      tibble::tibble(
        id          = i@id,
        patient_id  = p@id,
        time        = i@time,
        code        = .first_code(i@codes),
        description = .first_disp(i@codes)
      )
    }))
  })
}

.allergies_tbl <- function(patients) {
  .flatten_tbl(patients, function(p) {
    if (length(p@health_record@allergies) == 0) return(tibble::tibble())
    dplyr::bind_rows(lapply(p@health_record@allergies, function(a) {
      tibble::tibble(
        id          = a@id,
        patient_id  = p@id,
        onset_time  = a@time,
        end_time    = a@end_time %||% NA,
        is_active   = a@is_active,
        code        = .first_code(a@codes),
        description = .first_disp(a@codes)
      )
    }))
  })
}

.careplans_tbl <- function(patients) {
  .flatten_tbl(patients, function(p) {
    if (length(p@health_record@careplans) == 0) return(tibble::tibble())
    dplyr::bind_rows(lapply(p@health_record@careplans, function(cp) {
      tibble::tibble(
        id          = cp@id,
        patient_id  = p@id,
        start_time  = cp@time,
        end_time    = cp@end_time %||% NA,
        is_active   = cp@is_active,
        code        = .first_code(cp@codes),
        description = .first_disp(cp@codes)
      )
    }))
  })
}
