# R/state_flow.R
# process_state dispatches on state[["type"]] (plain list field, no S7 dispatch).

process_state <- function(state, person, time) {
  force(person)
  if (is.null(.REC$e)) .REC$e <- person@.record
  switch(state[["type"]],
    "Delay"            = .state_delay(state, person, time),
    "Guard"            = .state_guard(state, person, time),
    "Simple"           = .state_simple(state, person, time),
    "Encounter"        = .state_encounter(state, person, time),
    "EncounterEnd"     = .state_encounter_end(state, person, time),
    "Initial"          = .state_initial(state, person, time),
    "ConditionOnset"   = .state_condition_onset(state, person, time),
    "Terminal"         = list(person = person, next_state = NULL),
    "Procedure"        = .state_procedure(state, person, time),
    "ConditionEnd"     = .state_condition_end(state, person, time),
    "SetAttribute"     = .state_set_attribute(state, person, time),
    "Counter"          = .state_counter(state, person, time),
    "Death"            = .state_death(state, person, time),
    "MedicationOrder"  = .state_medication_order(state, person, time),
    "MedicationEnd"    = .state_medication_end(state, person, time),
    "CarePlanStart"    = .state_careplan_start(state, person, time),
    "CarePlanEnd"      = .state_careplan_end(state, person, time),
    "AllergyOnset"     = .state_allergy_onset(state, person, time),
    "AllergyEnd"       = .state_allergy_end(state, person, time),
    "Observation"      = .state_observation(state, person, time),
    "MultiObservation" = .state_multi_observation(state, person, time),
    "DiagnosticReport" = .state_diagnostic_report(state, person, time),
    "VitalSign"        = .state_vital_sign(state, person, time),
    "Symptom"          = .state_symptom(state, person, time),
    "CallSubmodule"    = .state_call_submodule(state, person, time),
    "Vaccine"          = .state_vaccine(state, person, time),
    "ImagingStudy"     = .state_imaging_study(state, person, time),
    "Device"           = .state_device(state, person, time),
    "DeviceEnd"        = .state_device_end(state, person, time),
    "SupplyList"       = .state_supply_list(state, person, time),
    {
      warning("Unknown state type: ", state[["type"]])
      .next(state, person, time)
    }
  )
}

.next <- function(state, person, time) {
  rec <- .REC$e
  rec[[state[["visited_key"]]]] <- TRUE
  list(person = person, next_state = resolve_transition(state[["transition"]], person, time))
}

.state_initial <- function(state, person, time) .next(state, person, time)
.state_simple  <- function(state, person, time) .next(state, person, time)

.state_delay <- function(state, person, time) {
  key <- state[["delay_key"]]
  rec <- .REC$e
  delay_until <- rec[[key]]
  t_num <- rec$.t_num %||% as.numeric(time)

  if (is.null(delay_until)) {
    rec[[key]] <- t_num + .resolve_duration(state[["definition"]])
    return(list(person = person, next_state = state[["name"]]))
  }
  if (t_num < delay_until) {
    return(list(person = person, next_state = state[["name"]]))
  }
  rec[[key]] <- NULL
  .next(state, person, time)
}

.unit_secs <- c(years = 365.25 * 86400, months = 30.44 * 86400,
                weeks = 7 * 86400, days = 86400, hours = 3600)

.unit_lookup <- function(unit) {
  v <- .unit_secs[unit]
  if (is.na(v)) 86400 else v
}

.sample_distribution <- function(dist) {
  kind   <- toupper(dist[["kind"]] %||% "EXACT")
  params <- dist[["parameters"]] %||% list()
  switch(kind,
    "EXACT"       = as.numeric(params[["value"]] %||% 0),
    "UNIFORM"     = stats::runif(1, as.numeric(params[["low"]] %||% 0),
                                    as.numeric(params[["high"]] %||% 0)),
    "GAUSSIAN"    = stats::rnorm(1, mean = as.numeric(params[["mean"]] %||% 0),
                                    sd   = as.numeric(params[["standardDeviation"]] %||% 1)),
    "EXPONENTIAL" = stats::rexp(1, rate = 1 / max(as.numeric(params[["mean"]] %||% 1), 1e-9)),
    0
  )
}

.resolve_duration <- function(def) {
  if (!is.null(def[["exact"]])) {
    qty  <- as.numeric(def[["exact"]][["quantity"]])
    unit <- def[["exact"]][["unit"]] %||% "days"
    return(qty * .unit_lookup(unit))
  }
  if (!is.null(def[["range"]])) {
    low  <- as.numeric(def[["range"]][["low"]])
    high <- as.numeric(def[["range"]][["high"]])
    unit <- def[["range"]][["unit"]] %||% "days"
    return(stats::runif(1, low, high) * .unit_lookup(unit))
  }
  # v2 format: distribution.kind + separate unit field
  dist <- def[["distribution"]]
  if (is.list(dist) && !is.null(dist[["kind"]])) {
    unit <- def[["unit"]] %||% "days"
    return(.sample_distribution(dist) * .unit_lookup(unit))
  }
  0
}

.state_guard <- function(state, person, time) {
  allow <- state[["guard_allow"]]
  if (!is.null(allow) && !evaluate_condition(allow, person, time)) {
    return(list(person = person, next_state = state[["name"]]))
  }
  .next(state, person, time)
}

.state_set_attribute <- function(state, person, time) {
  attr_name <- state[["attr_name"]]
  if (!is.null(attr_name)) {
    val  <- state[["attr_value"]]
    dist <- state[["definition"]][["distribution"]]
    if (is.null(val) && is.list(dist) && !is.null(dist[["kind"]])) {
      val <- .sample_distribution(dist)
    }
    person@attributes[[attr_name]] <- val
  }
  .next(state, person, time)
}

.state_counter <- function(state, person, time) {
  attr_name <- state[["attr_name"]]
  current   <- as.numeric(person@attributes[[attr_name]] %||% 0)
  person@attributes[[attr_name]] <- if (state[["counter_action"]] == "decrement")
                                      current - state[["counter_amount"]]
                                    else
                                      current + state[["counter_amount"]]
  .next(state, person, time)
}

.state_death <- function(state, person, time) {
  .REC$e$.is_alive <- FALSE
  person@is_alive <- FALSE
  person@attributes[["death_date"]] <- time
  list(person = person, next_state = NULL)
}
