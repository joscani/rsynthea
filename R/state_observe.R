# R/state_observe.R

.resolve_obs_value <- function(def, person) {
  if (!is.null(def[["exact"]])) {
    return(as.numeric(def[["exact"]][["quantity"]]))
  }
  if (!is.null(def[["range"]])) {
    low  <- as.numeric(def[["range"]][["low"]])
    high <- as.numeric(def[["range"]][["high"]])
    return(runif(1, low, high))
  }
  if (!is.null(def[["attribute"]])) {
    return(person@attributes[[def[["attribute"]]]])
  }
  if (!is.null(def[["value"]])) {
    return(def[["value"]])
  }
  NA_real_
}

.state_observation <- function(state, person, time) {
  def <- state@definition
  obs <- Observation(
    id       = .new_id(),
    time     = time,
    codes    = .parse_codes(def[["codes"]]),
    value    = .resolve_obs_value(def, person),
    unit     = def[["unit"]] %||% NULL,
    category = def[["category"]] %||% NULL
  )
  person@health_record@observations <- c(person@health_record@observations, list(obs))
  .next(state, person, time)
}

.state_multi_observation <- function(state, person, time) {
  def      <- state@definition
  obs_list <- def[["observations"]] %||% list()
  for (obs_def in obs_list) {
    obs <- Observation(
      id       = .new_id(),
      time     = time,
      codes    = .parse_codes(obs_def[["codes"]]),
      value    = .resolve_obs_value(obs_def, person),
      unit     = obs_def[["unit"]] %||% NULL,
      category = def[["category"]] %||% NULL
    )
    person@health_record@observations <- c(person@health_record@observations, list(obs))
  }
  .next(state, person, time)
}

.state_diagnostic_report <- function(state, person, time) {
  def       <- state@definition
  obs_defs  <- def[["observations"]] %||% list()
  obs_entries <- lapply(obs_defs, function(o) {
    Observation(
      id    = .new_id(),
      time  = time,
      codes = .parse_codes(o[["codes"]]),
      value = .resolve_obs_value(o, person),
      unit  = o[["unit"]] %||% NULL
    )
  })
  report <- DiagnosticReport(
    id           = .new_id(),
    time         = time,
    codes        = .parse_codes(def[["codes"]]),
    observations = obs_entries
  )
  person@health_record@reports <- c(person@health_record@reports, list(report))
  .next(state, person, time)
}

.state_vital_sign <- function(state, person, time) {
  def     <- state@definition
  vs_name <- def[["vital_sign"]] %||% ""
  value   <- .resolve_obs_value(def, person)
  person@vital_signs[[vs_name]] <- list(
    value = value,
    unit  = def[["unit"]] %||% NULL,
    time  = time
  )
  .next(state, person, time)
}

.state_symptom <- function(state, person, time) {
  def      <- state@definition
  sym_name <- def[["symptom"]] %||% ""
  value    <- as.numeric(.resolve_obs_value(def, person) %||% 0)
  value    <- max(0, min(100, value))
  person@symptoms[[sym_name]] <- list(
    value = value,
    cause = def[["cause"]] %||% NULL,
    time  = time
  )
  .next(state, person, time)
}

.state_imaging_study <- function(state, person, time) {
  def <- state@definition
  img <- ImagingStudy(
    id     = .new_id(),
    time   = time,
    codes  = .parse_codes(def[["codes"]]),
    series = def[["series"]] %||% list()
  )
  person@health_record@imaging <- c(person@health_record@imaging, list(img))
  .next(state, person, time)
}

.state_device <- function(state, person, time) {
  def    <- state@definition
  dev_id <- .new_id()
  dev <- Device(
    id    = dev_id,
    time  = time,
    codes = .parse_codes(def[["codes"]])
  )
  person@health_record@devices <- c(person@health_record@devices, list(dev))
  person@attributes[[paste0("__device_ref__", state@name)]] <- dev_id
  .next(state, person, time)
}

.state_device_end <- function(state, person, time) {
  dev_name <- state@definition[["device"]] %||% ""
  dev_id   <- person@attributes[[paste0("__device_ref__", dev_name)]]
  person@health_record@devices <- lapply(
    person@health_record@devices,
    function(d) {
      if (!is.null(dev_id) && d@id == dev_id) {
        d@is_active <- FALSE; d@end_time <- time; d
      } else d
    }
  )
  .next(state, person, time)
}

.state_supply_list <- function(state, person, time) {
  .next(state, person, time)
}

.state_call_submodule <- function(state, person, time) {
  sub_name <- state@definition[["submodule"]] %||% ""
  person@attributes[[paste0("__call_submodule__", state@name)]] <- sub_name
  .next(state, person, time)
}
