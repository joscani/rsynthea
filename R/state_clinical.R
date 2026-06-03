# R/state_clinical.R
# Real implementations of clinical states — overrides stubs in state_flow.R

.parse_codes <- function(raw_codes) {
  lapply(raw_codes %||% list(), function(c) {
    Code(
      system  = as.character(c[["system"]]  %||% ""),
      code    = as.character(c[["code"]]    %||% ""),
      display = as.character(c[["display"]] %||% "")
    )
  })
}

.new_id <- function() {
  paste0(format(Sys.time(), "%Y%m%d%H%M%OS3"), "-", sample.int(99999L, 1L))
}

# --- Encounter ---

.state_encounter <- function(state, person, time) {
  def    <- state@definition
  enc_id <- .new_id()
  enc <- Encounter(
    id              = enc_id,
    time            = time,
    codes           = .parse_codes(def[["codes"]]),
    encounter_class = def[["encounter_class"]] %||% "ambulatory"
  )
  person@health_record@encounters <- c(person@health_record@encounters, list(enc))
  person@attributes[["__current_encounter__"]] <- enc_id
  .next(state, person, time)
}

.state_encounter_end <- function(state, person, time) {
  enc_id <- person@attributes[["__current_encounter__"]]
  if (!is.null(enc_id)) {
    person@health_record@encounters <- lapply(
      person@health_record@encounters,
      function(e) { if (e@id == enc_id) { e@end_time <- time; e } else e }
    )
    person@attributes[["__current_encounter__"]] <- NULL
  }
  .next(state, person, time)
}

# --- Condition ---

.state_condition_onset <- function(state, person, time) {
  def     <- state@definition
  cond_id <- .new_id()
  cond <- Condition(
    id    = cond_id,
    time  = time,
    codes = .parse_codes(def[["codes"]])
  )
  person@health_record@conditions <- c(person@health_record@conditions, list(cond))
  person@attributes[[paste0("__condition_ref__", state@name)]] <- cond_id
  .next(state, person, time)
}

.state_condition_end <- function(state, person, time) {
  onset_name <- state@definition[["condition_onset"]] %||% ""
  cond_id    <- person@attributes[[paste0("__condition_ref__", onset_name)]]
  person@health_record@conditions <- lapply(
    person@health_record@conditions,
    function(c) {
      if (!is.null(cond_id) && c@id == cond_id) {
        c@is_active <- FALSE; c@end_time <- time; c
      } else c
    }
  )
  .next(state, person, time)
}

# --- Medication ---

.state_medication_order <- function(state, person, time) {
  def    <- state@definition
  med_id <- .new_id()
  med <- Medication(
    id    = med_id,
    time  = time,
    codes = .parse_codes(def[["codes"]])
  )
  person@health_record@medications <- c(person@health_record@medications, list(med))
  person@attributes[[paste0("__medication_ref__", state@name)]] <- med_id
  .next(state, person, time)
}

.state_medication_end <- function(state, person, time) {
  med_name <- state@definition[["medication_order"]] %||% ""
  med_id   <- person@attributes[[paste0("__medication_ref__", med_name)]]
  person@health_record@medications <- lapply(
    person@health_record@medications,
    function(m) {
      if (!is.null(med_id) && m@id == med_id) {
        m@is_active <- FALSE; m@end_time <- time; m
      } else m
    }
  )
  .next(state, person, time)
}

# --- CarePlan ---

.state_careplan_start <- function(state, person, time) {
  def   <- state@definition
  cp_id <- .new_id()
  cp <- CarePlan(
    id         = cp_id,
    time       = time,
    codes      = .parse_codes(def[["codes"]]),
    activities = .parse_codes(def[["activities"]])
  )
  person@health_record@careplans <- c(person@health_record@careplans, list(cp))
  person@attributes[[paste0("__careplan_ref__", state@name)]] <- cp_id
  .next(state, person, time)
}

.state_careplan_end <- function(state, person, time) {
  cp_name <- state@definition[["careplan"]] %||% ""
  cp_id   <- person@attributes[[paste0("__careplan_ref__", cp_name)]]
  person@health_record@careplans <- lapply(
    person@health_record@careplans,
    function(cp) {
      if (!is.null(cp_id) && cp@id == cp_id) {
        cp@is_active <- FALSE; cp@end_time <- time; cp
      } else cp
    }
  )
  .next(state, person, time)
}

# --- Allergy ---

.state_allergy_onset <- function(state, person, time) {
  def <- state@definition
  al <- AllergyIntolerance(
    id           = .new_id(),
    time         = time,
    codes        = .parse_codes(def[["codes"]]),
    allergy_type = def[["allergy_type"]] %||% NULL,
    category     = def[["category"]] %||% NULL
  )
  person@health_record@allergies <- c(person@health_record@allergies, list(al))
  .next(state, person, time)
}

.state_allergy_end <- function(state, person, time) {
  person@health_record@allergies <- lapply(
    person@health_record@allergies,
    function(a) { if (a@is_active) { a@is_active <- FALSE; a@end_time <- time; a } else a }
  )
  .next(state, person, time)
}

# --- Procedure ---

.state_procedure <- function(state, person, time) {
  def  <- state@definition
  proc <- Procedure(
    id    = .new_id(),
    time  = time,
    codes = .parse_codes(def[["codes"]])
  )
  person@health_record@procedures <- c(person@health_record@procedures, list(proc))
  .next(state, person, time)
}

# --- Vaccine ---

.state_vaccine <- function(state, person, time) {
  def <- state@definition
  imm <- Immunization(
    id    = .new_id(),
    time  = time,
    codes = .parse_codes(def[["codes"]])
  )
  person@health_record@immunizations <- c(person@health_record@immunizations, list(imm))
  .next(state, person, time)
}
