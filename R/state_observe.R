# R/state_observe.R

.resolve_obs_value <- function(def, person) {
  if (!is.null(def[["exact"]]))     return(.coerce_quantity(def[["exact"]][["quantity"]]))
  if (!is.null(def[["range"]]))     return(stats::runif(1, as.numeric(def[["range"]][["low"]]),
                                                        as.numeric(def[["range"]][["high"]])))
  if (!is.null(def[["attribute"]])) return(person@attributes[[def[["attribute"]]]])
  if (!is.null(def[["value"]]))     return(def[["value"]])
  NA_real_
}

.coerce_quantity <- function(quantity) {
  if (is.numeric(quantity) || is.logical(quantity) || is.null(quantity)) return(quantity)
  numeric_value <- suppressWarnings(as.numeric(quantity))
  if (!is.na(numeric_value)) numeric_value else quantity
}

.append_observation <- function(person, observation) {
  person <- .rec_append(person, "observations", observation)
  .index_observation(observation)
  person
}

.index_observation <- function(observation) {
  rec <- .REC$e
  for (code in observation$codes %||% list()) {
    code_value <- code[["code"]]
    if (is.null(code_value) || !nzchar(code_value)) next
    rec$.latest_observations[[code_value]] <- observation
    by_code <- rec$.observations_by_code[[code_value]]
    if (is.null(by_code)) by_code <- list()
    by_code[[length(by_code) + 1L]] <- observation
    rec$.observations_by_code[[code_value]] <- by_code
  }
}

.state_observation <- function(state, person, time) {
  person <- .append_observation(person, list(
    id       = .new_id(),
    time     = time,
    codes    = state[["codes"]],
    value    = .resolve_obs_value(state[["definition"]], person),
    unit     = state[["unit"]],
    category = state[["category"]]
  ))
  .next(state, person, time)
}

.state_multi_observation <- function(state, person, time) {
  def       <- state[["definition"]]
  sub_obs   <- def[["observations"]] %||% list()
  sub_codes <- state[["sub_codes"]]
  category  <- state[["category"]]
  for (i in seq_along(sub_obs)) {
    obs_def <- sub_obs[[i]]
    person <- .append_observation(person, list(
      id       = .new_id(),
      time     = time,
      codes    = sub_codes[[i]],
      value    = .resolve_obs_value(obs_def, person),
      unit     = obs_def[["unit"]] %||% NULL,
      category = category
    ))
  }
  .next(state, person, time)
}

.state_diagnostic_report <- function(state, person, time) {
  def       <- state[["definition"]]
  sub_obs   <- def[["observations"]] %||% list()
  sub_codes <- state[["sub_codes"]]
  obs_entries <- vector("list", length(sub_obs))
  for (i in seq_along(sub_obs)) {
    o <- sub_obs[[i]]
    obs_entries[[i]] <- list(
      id    = .new_id(),
      time  = time,
      codes = sub_codes[[i]],
      value = .resolve_obs_value(o, person),
      unit  = o[["unit"]] %||% NULL
    )
  }
  person <- .rec_append(person, "reports", list(
    id           = .new_id(),
    time         = time,
    codes        = state[["codes"]],
    observations = obs_entries
  ))
  .next(state, person, time)
}

.state_vital_sign <- function(state, person, time) {
  person@vital_signs[[state[["vs_name"]]]] <- list(
    value = .resolve_obs_value(state[["definition"]], person),
    unit  = state[["unit"]],
    time  = time
  )
  .next(state, person, time)
}

.state_symptom <- function(state, person, time) {
  value <- max(0, min(100, as.numeric(.resolve_obs_value(state[["definition"]], person) %||% 0)))
  person@symptoms[[state[["sym_name"]]]] <- list(value = value, cause = state[["sym_cause"]], time = time)
  .next(state, person, time)
}

.state_imaging_study <- function(state, person, time) {
  person <- .rec_append(person, "imaging", list(
    id     = .new_id(),
    time   = time,
    codes  = state[["codes"]],
    series = state[["series"]]
  ))
  .next(state, person, time)
}

.state_device <- function(state, person, time) {
  dev_id <- .new_id()
  person <- .rec_append(person, "devices", list(
    id        = dev_id,
    time      = time,
    codes     = state[["codes"]],
    is_active = TRUE,
    end_time  = NULL
  ))
  rec2 <- .REC$e; rec2[[state[["device_key"]]]] <- dev_id
  .next(state, person, time)
}

.state_device_end <- function(state, person, time) {
  dev_id <- .REC$e[[state[["device_end_key"]]]]
  rec    <- .REC$e
  rec$devices <- lapply(rec$devices, function(d) {
    if (!is.null(dev_id) && d$id == dev_id) {
      d$is_active <- FALSE; d$end_time <- time; d
    } else d
  })
  .next(state, person, time)
}

.state_supply_list <- function(state, person, time) {
  supplies <- state[["definition"]][["supplies"]] %||% list()
  for (supply in supplies) {
    person <- .rec_append(person, "supplies", list(
      id       = .new_id(),
      time     = time,
      quantity = .coerce_quantity(supply[["quantity"]]),
      codes    = .parse_codes(if (!is.null(supply[["code"]])) list(supply[["code"]]) else list())
    ))
  }
  .next(state, person, time)
}

.state_call_submodule <- function(state, person, time) {
  rec <- .REC$e; rec[[state[["call_key"]]]] <- state[["submodule_name"]]
  .next(state, person, time)
}
