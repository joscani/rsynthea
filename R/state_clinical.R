# R/state_clinical.R
# Clinical state implementations using mutable environments for O(1) updates.
# All environment writes use `rec <- .REC$e; rec[[key]] <- val` to avoid
# triggering S7's @<- operator (which would copy the Person object).

.rec_append <- function(person, field, item) {
  rec <- .REC$e
  rec[[field]][[length(rec[[field]]) + 1L]] <- item
  person
}

.parse_codes <- function(raw_codes) {
  lapply(raw_codes %||% list(), function(c) {
    list(
      system  = as.character(c[["system"]]  %||% ""),
      code    = as.character(c[["code"]]    %||% ""),
      display = as.character(c[["display"]] %||% "")
    )
  })
}

.id_counter <- local({ n <- 0L; environment() })

.new_id <- function() {
  .id_counter$n <- .id_counter$n + 1L
  paste0(.REC$e$.patient_id %||% "0", "-", .id_counter$n)
}

.new_item_env <- function(...) {
  e <- new.env(parent = emptyenv())
  args <- list(...)
  for (nm in names(args)) e[[nm]] <- args[[nm]]
  e
}

# --- Encounter ---

.state_encounter <- function(state, person, time) {
  rec <- .REC$e

  if (state[["is_wellness"]]) {
    wellness_key <- state[["wellness_key"]]
    t_num <- rec$.t_num %||% as.numeric(time)
    wt    <- rec[[wellness_key]]
    if (!is.null(wt) && wt >= t_num) {
      return(list(person = person, next_state = state[["name"]]))
    }
    rec[[wellness_key]] <- t_num
  }

  enc_env <- new.env(parent = emptyenv(), hash = FALSE)
  enc_env$id              <- .new_id()
  enc_env$time            <- time
  enc_env$end_time        <- NULL
  enc_env$codes           <- state[["codes"]]
  enc_env$encounter_class <- state[["encounter_class"]]
  rec[["__current_encounter_env__"]] <- enc_env
  rec$encounters[[length(rec$encounters) + 1L]] <- enc_env
  .next(state, person, time)
}

.state_encounter_end <- function(state, person, time) {
  rec     <- .REC$e
  enc_env <- rec[["__current_encounter_env__"]]
  if (!is.null(enc_env)) {
    enc_env$end_time <- time
    rec[["__current_encounter_env__"]] <- NULL
  }
  .next(state, person, time)
}

# --- Condition ---

.state_condition_onset <- function(state, person, time) {
  rec      <- .REC$e
  cond_env <- new.env(parent = emptyenv(), hash = FALSE)
  cond_env$id        <- .new_id()
  cond_env$time      <- time
  cond_env$codes     <- state[["codes"]]
  cond_env$is_active <- TRUE
  cond_env$end_time  <- NULL
  rec$conditions[[length(rec$conditions) + 1L]] <- cond_env
  rec[[state[["cond_key"]]]] <- cond_env
  primary_code <- cond_env$codes[[1L]][["code"]]
  if (!is.null(primary_code)) rec$.active_conditions[[primary_code]] <- cond_env
  .next(state, person, time)
}

.state_condition_end <- function(state, person, time) {
  rec      <- .REC$e
  cond_env <- rec[[state[["cond_end_key"]]]]
  if (!is.null(cond_env)) {
    primary_code <- cond_env$codes[[1L]][["code"]]
    if (!is.null(primary_code) && exists(primary_code, envir = rec$.active_conditions, inherits = FALSE))
      rm(list = primary_code, envir = rec$.active_conditions)
    cond_env$is_active <- FALSE
    cond_env$end_time  <- time
  }
  .next(state, person, time)
}

# --- Medication ---

.state_medication_order <- function(state, person, time) {
  rec     <- .REC$e
  med_env <- new.env(parent = emptyenv(), hash = FALSE)
  med_env$id        <- .new_id()
  med_env$time      <- time
  med_env$codes     <- state[["codes"]]
  med_env$is_active <- TRUE
  med_env$end_time  <- NULL
  rec$medications[[length(rec$medications) + 1L]] <- med_env
  rec[[state[["med_key"]]]] <- med_env
  primary_code <- med_env$codes[[1L]][["code"]]
  if (!is.null(primary_code)) rec$.active_medications[[primary_code]] <- med_env
  .next(state, person, time)
}

.state_medication_end <- function(state, person, time) {
  rec     <- .REC$e
  med_env <- rec[[state[["med_end_key"]]]]
  if (!is.null(med_env)) {
    primary_code <- med_env$codes[[1L]][["code"]]
    if (!is.null(primary_code) && exists(primary_code, envir = rec$.active_medications, inherits = FALSE))
      rm(list = primary_code, envir = rec$.active_medications)
    med_env$is_active <- FALSE
    med_env$end_time  <- time
  }
  .next(state, person, time)
}

# --- CarePlan ---

.state_careplan_start <- function(state, person, time) {
  rec    <- .REC$e
  cp_env <- new.env(parent = emptyenv(), hash = FALSE)
  cp_env$id         <- .new_id()
  cp_env$time       <- time
  cp_env$codes      <- state[["codes"]]
  cp_env$activities <- state[["activities"]]
  cp_env$is_active  <- TRUE
  cp_env$end_time   <- NULL
  rec$careplans[[length(rec$careplans) + 1L]] <- cp_env
  rec[[state[["cp_key"]]]] <- cp_env
  primary_code <- cp_env$codes[[1L]][["code"]]
  if (!is.null(primary_code)) rec$.active_careplans[[primary_code]] <- cp_env
  .next(state, person, time)
}

.state_careplan_end <- function(state, person, time) {
  rec    <- .REC$e
  cp_env <- rec[[state[["cp_end_key"]]]]
  if (!is.null(cp_env)) {
    primary_code <- cp_env$codes[[1L]][["code"]]
    if (!is.null(primary_code) && exists(primary_code, envir = rec$.active_careplans, inherits = FALSE))
      rm(list = primary_code, envir = rec$.active_careplans)
    cp_env$is_active <- FALSE
    cp_env$end_time  <- time
  }
  .next(state, person, time)
}

# --- Allergy ---

.state_allergy_onset <- function(state, person, time) {
  rec     <- .REC$e
  alg_env <- new.env(parent = emptyenv(), hash = FALSE)
  alg_env$id           <- .new_id()
  alg_env$time         <- time
  alg_env$codes        <- state[["codes"]]
  alg_env$is_active    <- TRUE
  alg_env$end_time     <- NULL
  alg_env$allergy_type <- state[["allergy_type"]]
  alg_env$category     <- state[["category"]]
  rec$allergies[[length(rec$allergies) + 1L]] <- alg_env
  rec[[state[["allergy_key"]]]] <- alg_env
  .next(state, person, time)
}

.state_allergy_end <- function(state, person, time) {
  rec     <- .REC$e
  alg_env <- rec[[state[["alg_end_key"]]]]
  if (!is.null(alg_env)) {
    alg_env$is_active <- FALSE
    alg_env$end_time  <- time
  }
  .next(state, person, time)
}

# --- Procedure ---

.state_procedure <- function(state, person, time) {
  rec <- .REC$e
  n   <- length(rec$procedures) + 1L
  rec$procedures[[n]] <- list(
    id    = .new_id(),
    time  = time,
    codes = state[["codes"]]
  )
  .next(state, person, time)
}

# --- Vaccine ---

.state_vaccine <- function(state, person, time) {
  rec <- .REC$e
  n   <- length(rec$immunizations) + 1L
  rec$immunizations[[n]] <- list(
    id    = .new_id(),
    time  = time,
    codes = state[["codes"]]
  )
  .next(state, person, time)
}
