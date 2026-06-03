# R/logic.R

`%||%` <- function(a, b) if (!is.null(a)) a else b

evaluate_condition <- function(cond, person, time) {
  if (is.null(cond) || length(cond) == 0) return(TRUE)
  ct <- cond[["condition_type"]]

  switch(ct,
    "And"    = all(vapply(cond$conditions, evaluate_condition, logical(1),
                          person = person, time = time)),
    "Or"     = any(vapply(cond$conditions, evaluate_condition, logical(1),
                          person = person, time = time)),
    "Not"    = !evaluate_condition(cond$condition, person, time),
    "Gender" = .cond_gender(cond, person),
    "Age"    = .cond_age(cond, person, time),
    "Date"   = .cond_date(cond, time),
    "Race"   = .cond_race(cond, person),
    "Socioeconomic Status" = identical(
      tolower(person@attributes[["socioeconomic_status"]] %||% ""),
      tolower(cond$category %||% "")
    ),
    "Attribute"         = .cond_attribute(cond, person),
    "Symptom"           = .cond_symptom(cond, person),
    "Vital Sign"        = .cond_vital_sign(cond, person),
    "Observation"       = .cond_observation(cond, person),
    "Active Condition"  = .cond_active_condition(cond, person),
    "Active Medication" = .cond_active_medication(cond, person),
    "Active CarePlan"   = .cond_active_careplan(cond, person),
    "PriorState"        = .cond_prior_state(cond, person),
    "True"              = TRUE,
    "False"             = FALSE,
    FALSE
  )
}

.compare <- function(left, op, right) {
  tryCatch(
    switch(op,
      "<"  = left < right,
      "<=" = left <= right,
      ">"  = left > right,
      ">=" = left >= right,
      "==" = left == right,
      "!=" = left != right,
      FALSE
    ),
    error = function(e) FALSE
  )
}

.cond_gender <- function(cond, person) {
  toupper(person@attributes[["gender"]] %||% "") ==
    toupper(cond[["gender"]] %||% "")
}

.cond_age <- function(cond, person, time) {
  age <- age_at(person, time)
  qty <- as.numeric(cond[["quantity"]] %||% 0)
  qty <- switch(cond[["unit"]] %||% "years",
    "months" = qty / 12,
    "weeks"  = qty / 52,
    "days"   = qty / 365.25,
    qty
  )
  .compare(age, cond[["operator"]] %||% "==", qty)
}

.cond_date <- function(cond, time) {
  y <- cond[["year"]]
  if (is.null(y)) return(FALSE)
  m <- cond[["month"]] %||% 1
  d <- cond[["day"]] %||% 1
  target <- as.POSIXct(paste(y, m, d, sep = "-"))
  .compare(as.numeric(time), cond[["operator"]] %||% "==", as.numeric(target))
}

.cond_race <- function(cond, person) {
  tolower(person@attributes[["race"]] %||% "") == tolower(cond[["race"]] %||% "")
}

.cond_attribute <- function(cond, person) {
  attr_val <- person@attributes[[cond[["attribute"]]]]
  if (is.null(cond[["operator"]]) && is.null(cond[["value"]])) return(!is.null(attr_val))
  if (is.null(attr_val)) return(identical(cond[["operator"]], "!="))
  target <- cond[["value"]] %||% cond[["value_code"]]
  .compare(attr_val, cond[["operator"]] %||% "==", target)
}

.cond_symptom <- function(cond, person) {
  val <- (person@symptoms[[cond[["symptom"]]]] %||% list(value = 0))[["value"]]
  .compare(val, cond[["operator"]] %||% ">=", cond[["value"]] %||% 0)
}

.cond_vital_sign <- function(cond, person) {
  vs <- person@vital_signs[[cond[["vital_sign"]]]]
  if (is.null(vs)) return(FALSE)
  .compare(vs[["value"]], cond[["operator"]] %||% ">=", cond[["value"]] %||% 0)
}

.cond_observation <- function(cond, person) {
  target_code <- (cond[["codes"]] %||% list())[[1]]
  if (is.null(target_code)) return(FALSE)
  obs_list <- person@health_record@observations
  matching <- Filter(function(o) {
    any(vapply(o@codes, function(c) c@code == target_code[["code"]], logical(1)))
  }, obs_list)
  if (length(matching) == 0) return(FALSE)
  latest <- matching[[length(matching)]]
  if (!is.null(cond[["value"]])) {
    .compare(latest@value, cond[["operator"]] %||% "==", cond[["value"]])
  } else TRUE
}

.cond_active_condition <- function(cond, person) {
  target_code <- (cond[["codes"]] %||% list())[[1]]
  if (is.null(target_code)) return(FALSE)
  any(vapply(person@health_record@conditions, function(c) {
    c@is_active && any(vapply(c@codes, function(cd) cd@code == target_code[["code"]], logical(1)))
  }, logical(1)))
}

.cond_active_medication <- function(cond, person) {
  target_code <- (cond[["codes"]] %||% list())[[1]]
  if (is.null(target_code)) return(FALSE)
  any(vapply(person@health_record@medications, function(m) {
    m@is_active && any(vapply(m@codes, function(cd) cd@code == target_code[["code"]], logical(1)))
  }, logical(1)))
}

.cond_active_careplan <- function(cond, person) {
  target_code <- (cond[["codes"]] %||% list())[[1]]
  if (is.null(target_code)) return(FALSE)
  any(vapply(person@health_record@careplans, function(cp) {
    cp@is_active && any(vapply(cp@codes, function(cd) cd@code == target_code[["code"]], logical(1)))
  }, logical(1)))
}

.cond_prior_state <- function(cond, person) {
  state_name <- cond[["name"]] %||% ""
  key <- paste0("__visited__", state_name)
  isTRUE(person@attributes[[key]])
}
