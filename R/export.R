# R/export.R

#' Export a simulated population to tidy tibbles
#'
#' Converts a list of simulated `Person` objects into a named list of tibbles
#' (one per clinical domain), optionally writing them as CSV files.
#'
#' @param patients List of `Person` objects, as returned by
#'   [generate_population()].
#' @param output_dir Character or `NULL`. If provided, each tibble is written
#'   to `<output_dir>/<domain>.csv`. The directory is created if it does not
#'   exist.
#'
#' @return A named list of tibbles:
#' \describe{
#'   \item{`patients`}{One row per patient (id, gender, birth_date, is_alive, …).}
#'   \item{`encounters`}{Clinical encounters with start/end times and codes.}
#'   \item{`conditions`}{Active and resolved conditions.}
#'   \item{`medications`}{Medication orders with start/end times.}
#'   \item{`procedures`}{Procedures performed.}
#'   \item{`observations`}{Lab and clinical observations.}
#'   \item{`immunizations`}{Vaccines administered.}
#'   \item{`allergies`}{Allergy records.}
#'   \item{`careplans`}{Care plan records.}
#'   \item{`imaging`}{Imaging studies.}
#'   \item{`devices`}{Implanted or assigned devices.}
#'   \item{`reports`}{Diagnostic reports.}
#'   \item{`report_observations`}{Observations embedded in diagnostic reports.}
#'   \item{`supplies`}{Supplies used during procedures or care episodes.}
#' }
#'
#' @examples
#' \dontrun{
#' patients <- generate_population(5, seed = 1L,
#'                                 end_date = as.POSIXct("2020-01-01"))
#' tbls <- export_population(patients)
#' tbls$encounters
#'
#' # Write CSVs
#' export_population(patients, output_dir = tempdir())
#' }
#'
#' @seealso [generate_population()]
#' @export
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
    careplans     = .careplans_tbl(patients),
    imaging       = .imaging_tbl(patients),
    devices       = .devices_tbl(patients),
    reports       = .reports_tbl(patients),
    report_observations = .report_observations_tbl(patients),
    supplies      = .supplies_tbl(patients)
  )
  if (!is.null(output_dir)) {
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
    for (nm in names(tbls)) {
      data.table::fwrite(tbls[[nm]], file.path(output_dir, paste0(nm, ".csv")))
    }
  }
  tbls
}

# --- helpers ---

.first_code  <- function(codes) if (length(codes) > 0) codes[[1L]][["code"]]    else NA_character_
.first_sys   <- function(codes) if (length(codes) > 0) codes[[1L]][["system"]]  else NA_character_
.first_disp  <- function(codes) if (length(codes) > 0) codes[[1L]][["display"]] else NA_character_

# Expand patient ids: one entry per record in domain `field`.
.pat_ids <- function(patients, field) {
  ids   <- vapply(patients, function(p) p@id, character(1L))
  counts <- vapply(patients, function(p) length(p@.record[[field]]), integer(1L))
  rep(ids, counts)
}

# Collect all records from `field` across all patients into a flat list.
.all_recs <- function(patients, field) {
  unlist(lapply(patients, function(p) p@.record[[field]]), recursive = FALSE)
}

# Extract a POSIXct vector from a list of records.
.extract_time <- function(recs, key) {
  .POSIXct(vapply(recs, function(r) {
    x <- r[[key]]
    if (is.null(x) || is.na(x)) NA_real_ else as.numeric(x)
  }, numeric(1L)), tz = "UTC")
}

# Extract a character vector from a list of records.
.extract_chr <- function(recs, key, default = NA_character_) {
  vapply(recs, function(r) r[[key]] %||% default, character(1L))
}

# Extract a logical vector from a list of records.
.extract_lgl <- function(recs, key, default = NA) {
  vapply(recs, function(r) {
    v <- r[[key]]
    if (is.null(v)) default else as.logical(v)
  }, logical(1L))
}

# --- domain tables ---

.patients_tbl <- function(patients) {
  tibble::tibble(
    id         = vapply(patients, function(p) p@id, character(1L)),
    birth_date = .POSIXct(vapply(patients, function(p) {
      x <- p@attributes[["birth_date"]]; if (is.null(x)) NA_real_ else as.numeric(x)
    }, numeric(1L)), tz = "UTC"),
    death_date = .POSIXct(vapply(patients, function(p) {
      x <- p@attributes[["death_date"]]; if (is.null(x)) NA_real_ else as.numeric(x)
    }, numeric(1L)), tz = "UTC"),
    is_alive   = vapply(patients, function(p) p@is_alive, logical(1L)),
    gender     = vapply(patients, function(p) p@attributes[["gender"]]     %||% NA_character_, character(1L)),
    race       = vapply(patients, function(p) p@attributes[["race"]]       %||% NA_character_, character(1L)),
    ethnicity  = vapply(patients, function(p) p@attributes[["ethnicity"]]  %||% NA_character_, character(1L)),
    first_name = vapply(patients, function(p) p@attributes[["first_name"]] %||% NA_character_, character(1L)),
    last_name  = vapply(patients, function(p) p@attributes[["last_name"]]  %||% NA_character_, character(1L)),
    state      = vapply(patients, function(p) p@attributes[["state"]]      %||% NA_character_, character(1L)),
    city       = vapply(patients, function(p) p@attributes[["city"]]       %||% NA_character_, character(1L))
  )
}

.encounters_tbl <- function(patients) {
  recs <- .all_recs(patients, "encounters")
  if (length(recs) == 0L) {
    return(tibble::tibble(id = character(), patient_id = character(),
      time = .POSIXct(numeric(), tz = "UTC"), end_time = .POSIXct(numeric(), tz = "UTC"),
      encounter_class = character(), code = character(), code_system = character(),
      description = character()))
  }
  tibble::tibble(
    id              = .extract_chr(recs, "id"),
    patient_id      = .pat_ids(patients, "encounters"),
    time            = .extract_time(recs, "time"),
    end_time        = .extract_time(recs, "end_time"),
    encounter_class = .extract_chr(recs, "encounter_class"),
    code            = vapply(recs, function(r) .first_code(r$codes), character(1L)),
    code_system     = vapply(recs, function(r) .first_sys(r$codes),  character(1L)),
    description     = vapply(recs, function(r) .first_disp(r$codes), character(1L))
  )
}

.conditions_tbl <- function(patients) {
  recs <- .all_recs(patients, "conditions")
  if (length(recs) == 0L) {
    return(tibble::tibble(id = character(), patient_id = character(),
      onset_time = .POSIXct(numeric(), tz = "UTC"), end_time = .POSIXct(numeric(), tz = "UTC"),
      is_active = logical(), code = character(), code_system = character(), description = character()))
  }
  tibble::tibble(
    id          = .extract_chr(recs, "id"),
    patient_id  = .pat_ids(patients, "conditions"),
    onset_time  = .extract_time(recs, "time"),
    end_time    = .extract_time(recs, "end_time"),
    is_active   = .extract_lgl(recs, "is_active", default = TRUE),
    code        = vapply(recs, function(r) .first_code(r$codes), character(1L)),
    code_system = vapply(recs, function(r) .first_sys(r$codes),  character(1L)),
    description = vapply(recs, function(r) .first_disp(r$codes), character(1L))
  )
}

.medications_tbl <- function(patients) {
  recs <- .all_recs(patients, "medications")
  if (length(recs) == 0L) {
    return(tibble::tibble(id = character(), patient_id = character(),
      start_time = .POSIXct(numeric(), tz = "UTC"), end_time = .POSIXct(numeric(), tz = "UTC"),
      is_active = logical(), code = character(), code_system = character(), description = character()))
  }
  tibble::tibble(
    id          = .extract_chr(recs, "id"),
    patient_id  = .pat_ids(patients, "medications"),
    start_time  = .extract_time(recs, "time"),
    end_time    = .extract_time(recs, "end_time"),
    is_active   = .extract_lgl(recs, "is_active", default = TRUE),
    code        = vapply(recs, function(r) .first_code(r$codes), character(1L)),
    code_system = vapply(recs, function(r) .first_sys(r$codes),  character(1L)),
    description = vapply(recs, function(r) .first_disp(r$codes), character(1L))
  )
}

.procedures_tbl <- function(patients) {
  recs <- .all_recs(patients, "procedures")
  if (length(recs) == 0L) {
    return(tibble::tibble(id = character(), patient_id = character(),
      time = .POSIXct(numeric(), tz = "UTC"), code = character(),
      code_system = character(), description = character()))
  }
  tibble::tibble(
    id          = .extract_chr(recs, "id"),
    patient_id  = .pat_ids(patients, "procedures"),
    time        = .extract_time(recs, "time"),
    code        = vapply(recs, function(r) .first_code(r$codes), character(1L)),
    code_system = vapply(recs, function(r) .first_sys(r$codes),  character(1L)),
    description = vapply(recs, function(r) .first_disp(r$codes), character(1L))
  )
}

.observations_tbl <- function(patients) {
  recs <- .all_recs(patients, "observations")
  if (length(recs) == 0L) {
    return(tibble::tibble(id = character(), patient_id = character(),
      time = .POSIXct(numeric(), tz = "UTC"), value = character(), unit = character(),
      category = character(), code = character(), code_system = character(), description = character()))
  }
  tibble::tibble(
    id          = .extract_chr(recs, "id"),
    patient_id  = .pat_ids(patients, "observations"),
    time        = .extract_time(recs, "time"),
    value       = vapply(recs, function(r) as.character(r$value %||% NA), character(1L)),
    unit        = .extract_chr(recs, "unit"),
    category    = .extract_chr(recs, "category"),
    code        = vapply(recs, function(r) .first_code(r$codes), character(1L)),
    code_system = vapply(recs, function(r) .first_sys(r$codes),  character(1L)),
    description = vapply(recs, function(r) .first_disp(r$codes), character(1L))
  )
}

.immunizations_tbl <- function(patients) {
  recs <- .all_recs(patients, "immunizations")
  if (length(recs) == 0L) {
    return(tibble::tibble(id = character(), patient_id = character(),
      time = .POSIXct(numeric(), tz = "UTC"), code = character(), description = character()))
  }
  tibble::tibble(
    id          = .extract_chr(recs, "id"),
    patient_id  = .pat_ids(patients, "immunizations"),
    time        = .extract_time(recs, "time"),
    code        = vapply(recs, function(r) .first_code(r$codes), character(1L)),
    description = vapply(recs, function(r) .first_disp(r$codes), character(1L))
  )
}

.allergies_tbl <- function(patients) {
  recs <- .all_recs(patients, "allergies")
  if (length(recs) == 0L) {
    return(tibble::tibble(id = character(), patient_id = character(),
      onset_time = .POSIXct(numeric(), tz = "UTC"), end_time = .POSIXct(numeric(), tz = "UTC"),
      is_active = logical(), code = character(), description = character()))
  }
  tibble::tibble(
    id          = .extract_chr(recs, "id"),
    patient_id  = .pat_ids(patients, "allergies"),
    onset_time  = .extract_time(recs, "time"),
    end_time    = .extract_time(recs, "end_time"),
    is_active   = .extract_lgl(recs, "is_active", default = TRUE),
    code        = vapply(recs, function(r) .first_code(r$codes), character(1L)),
    description = vapply(recs, function(r) .first_disp(r$codes), character(1L))
  )
}

.careplans_tbl <- function(patients) {
  recs <- .all_recs(patients, "careplans")
  if (length(recs) == 0L) {
    return(tibble::tibble(id = character(), patient_id = character(),
      start_time = .POSIXct(numeric(), tz = "UTC"), end_time = .POSIXct(numeric(), tz = "UTC"),
      is_active = logical(), code = character(), description = character()))
  }
  tibble::tibble(
    id          = .extract_chr(recs, "id"),
    patient_id  = .pat_ids(patients, "careplans"),
    start_time  = .extract_time(recs, "time"),
    end_time    = .extract_time(recs, "end_time"),
    is_active   = .extract_lgl(recs, "is_active", default = TRUE),
    code        = vapply(recs, function(r) .first_code(r$codes), character(1L)),
    description = vapply(recs, function(r) .first_disp(r$codes), character(1L))
  )
}

.imaging_tbl <- function(patients) {
  recs <- .all_recs(patients, "imaging")
  if (length(recs) == 0L) {
    return(tibble::tibble(id = character(), patient_id = character(),
      time = .POSIXct(numeric(), tz = "UTC"), code = character(),
      code_system = character(), description = character(), series_count = integer()))
  }
  tibble::tibble(
    id           = .extract_chr(recs, "id"),
    patient_id   = .pat_ids(patients, "imaging"),
    time         = .extract_time(recs, "time"),
    code         = vapply(recs, function(r) .first_code(r$codes), character(1L)),
    code_system  = vapply(recs, function(r) .first_sys(r$codes),  character(1L)),
    description  = vapply(recs, function(r) .first_disp(r$codes), character(1L)),
    series_count = vapply(recs, function(r) length(r$series %||% list()), integer(1L))
  )
}

.devices_tbl <- function(patients) {
  recs <- .all_recs(patients, "devices")
  if (length(recs) == 0L) {
    return(tibble::tibble(id = character(), patient_id = character(),
      start_time = .POSIXct(numeric(), tz = "UTC"), end_time = .POSIXct(numeric(), tz = "UTC"),
      is_active = logical(), code = character(), code_system = character(), description = character()))
  }
  tibble::tibble(
    id          = .extract_chr(recs, "id"),
    patient_id  = .pat_ids(patients, "devices"),
    start_time  = .extract_time(recs, "time"),
    end_time    = .extract_time(recs, "end_time"),
    is_active   = .extract_lgl(recs, "is_active", default = TRUE),
    code        = vapply(recs, function(r) .first_code(r$codes), character(1L)),
    code_system = vapply(recs, function(r) .first_sys(r$codes),  character(1L)),
    description = vapply(recs, function(r) .first_disp(r$codes), character(1L))
  )
}

.reports_tbl <- function(patients) {
  recs <- .all_recs(patients, "reports")
  if (length(recs) == 0L) {
    return(tibble::tibble(id = character(), patient_id = character(),
      time = .POSIXct(numeric(), tz = "UTC"), code = character(),
      code_system = character(), description = character(), observation_count = integer()))
  }
  tibble::tibble(
    id                = .extract_chr(recs, "id"),
    patient_id        = .pat_ids(patients, "reports"),
    time              = .extract_time(recs, "time"),
    code              = vapply(recs, function(r) .first_code(r$codes), character(1L)),
    code_system       = vapply(recs, function(r) .first_sys(r$codes),  character(1L)),
    description       = vapply(recs, function(r) .first_disp(r$codes), character(1L)),
    observation_count = vapply(recs, function(r) length(r$observations %||% list()), integer(1L))
  )
}

.report_observations_tbl <- function(patients) {
  all_reports <- .all_recs(patients, "reports")
  if (length(all_reports) == 0L) {
    return(tibble::tibble(id = character(), report_id = character(), patient_id = character(),
      time = .POSIXct(numeric(), tz = "UTC"), value = character(), unit = character(),
      code = character(), code_system = character(), description = character()))
  }
  pat_id_per_report <- .pat_ids(patients, "reports")
  obs_lists <- lapply(all_reports, function(r) r$observations %||% list())
  obs_counts <- lengths(obs_lists)
  all_obs <- unlist(obs_lists, recursive = FALSE)
  if (length(all_obs) == 0L) {
    return(tibble::tibble(id = character(), report_id = character(), patient_id = character(),
      time = .POSIXct(numeric(), tz = "UTC"), value = character(), unit = character(),
      code = character(), code_system = character(), description = character()))
  }
  tibble::tibble(
    id          = .extract_chr(all_obs, "id"),
    report_id   = rep(.extract_chr(all_reports, "id"), obs_counts),
    patient_id  = rep(pat_id_per_report, obs_counts),
    time        = .extract_time(all_obs, "time"),
    value       = vapply(all_obs, function(o) as.character(o$value %||% NA), character(1L)),
    unit        = .extract_chr(all_obs, "unit"),
    code        = vapply(all_obs, function(o) .first_code(o$codes), character(1L)),
    code_system = vapply(all_obs, function(o) .first_sys(o$codes),  character(1L)),
    description = vapply(all_obs, function(o) .first_disp(o$codes), character(1L))
  )
}

.supplies_tbl <- function(patients) {
  recs <- .all_recs(patients, "supplies")
  if (length(recs) == 0L) {
    return(tibble::tibble(id = character(), patient_id = character(),
      time = .POSIXct(numeric(), tz = "UTC"), quantity = character(),
      code = character(), code_system = character(), description = character()))
  }
  tibble::tibble(
    id          = .extract_chr(recs, "id"),
    patient_id  = .pat_ids(patients, "supplies"),
    time        = .extract_time(recs, "time"),
    quantity    = vapply(recs, function(r) as.character(r$quantity %||% NA), character(1L)),
    code        = vapply(recs, function(r) .first_code(r$codes), character(1L)),
    code_system = vapply(recs, function(r) .first_sys(r$codes),  character(1L)),
    description = vapply(recs, function(r) .first_disp(r$codes), character(1L))
  )
}
