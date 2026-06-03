# R/state_flow.R

# process_state generic — dispatches by GMFState type string
process_state <- new_generic("process_state", "state")

method(process_state, GMFState) <- function(state, person, time) {
  switch(state@type,
    "Initial"          = .state_initial(state, person, time),
    "Simple"           = .state_simple(state, person, time),
    "Terminal"         = list(person = person, next_state = NULL),
    "Delay"            = .state_delay(state, person, time),
    "Guard"            = .state_guard(state, person, time),
    "SetAttribute"     = .state_set_attribute(state, person, time),
    "Counter"          = .state_counter(state, person, time),
    "Death"            = .state_death(state, person, time),
    "Encounter"        = .state_encounter(state, person, time),
    "EncounterEnd"     = .state_encounter_end(state, person, time),
    "ConditionOnset"   = .state_condition_onset(state, person, time),
    "ConditionEnd"     = .state_condition_end(state, person, time),
    "MedicationOrder"  = .state_medication_order(state, person, time),
    "MedicationEnd"    = .state_medication_end(state, person, time),
    "CarePlanStart"    = .state_careplan_start(state, person, time),
    "CarePlanEnd"      = .state_careplan_end(state, person, time),
    "AllergyOnset"     = .state_allergy_onset(state, person, time),
    "AllergyEnd"       = .state_allergy_end(state, person, time),
    "Procedure"        = .state_procedure(state, person, time),
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
      warning("Unknown state type: ", state@type)
      .next(state, person, time)
    }
  )
}

# Mark state visited and resolve transition
.next <- function(state, person, time) {
  key <- paste0("__visited__", state@name)
  person@attributes[[key]] <- TRUE
  list(person = person, next_state = resolve_transition(state@transition, person, time))
}

.state_initial <- function(state, person, time) .next(state, person, time)
.state_simple  <- function(state, person, time) .next(state, person, time)

.state_delay <- function(state, person, time) {
  def <- state@definition
  key <- paste0("__delay_until__", state@name)
  delay_until <- person@attributes[[key]]

  if (is.null(delay_until)) {
    duration_secs <- .resolve_duration(def)
    person@attributes[[key]] <- time + duration_secs
    return(list(person = person, next_state = state@name))
  }
  if (time < delay_until) {
    return(list(person = person, next_state = state@name))
  }
  person@attributes[[key]] <- NULL
  .next(state, person, time)
}

.resolve_duration <- function(def) {
  unit_secs <- c(years = 365.25 * 86400, months = 30.44 * 86400,
                 weeks = 7 * 86400, days = 86400, hours = 3600)
  .unit_lookup <- function(unit) {
    v <- unit_secs[unit]
    if (is.na(v)) 86400 else v
  }
  if (!is.null(def[["exact"]])) {
    qty  <- as.numeric(def[["exact"]][["quantity"]])
    unit <- def[["exact"]][["unit"]] %||% "days"
    return(qty * .unit_lookup(unit))
  }
  if (!is.null(def[["range"]])) {
    low  <- as.numeric(def[["range"]][["low"]])
    high <- as.numeric(def[["range"]][["high"]])
    unit <- def[["range"]][["unit"]] %||% "days"
    return(runif(1, low, high) * .unit_lookup(unit))
  }
  0
}

.state_guard <- function(state, person, time) {
  allow <- state@definition[["allow"]]
  if (!is.null(allow) && !evaluate_condition(allow, person, time)) {
    return(list(person = person, next_state = state@name))
  }
  .next(state, person, time)
}

.state_set_attribute <- function(state, person, time) {
  def <- state@definition
  attr_name <- def[["attribute"]]
  if (!is.null(attr_name)) {
    val <- def[["value"]]
    person@attributes[[attr_name]] <- val
  }
  .next(state, person, time)
}

.state_counter <- function(state, person, time) {
  def       <- state@definition
  attr_name <- def[["attribute"]]
  action    <- def[["action"]] %||% "increment"
  amount    <- as.numeric(def[["amount"]] %||% 1)
  current   <- as.numeric(person@attributes[[attr_name]] %||% 0)
  person@attributes[[attr_name]] <- if (action == "decrement") current - amount
                                    else current + amount
  .next(state, person, time)
}

.state_death <- function(state, person, time) {
  person@is_alive <- FALSE
  person@attributes[["death_date"]] <- time
  list(person = person, next_state = NULL)
}

