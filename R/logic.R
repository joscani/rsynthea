# R/logic.R

`%||%` <- function(a, b) if (!is.null(a)) a else b

# Package-level cache: person@.record is always the same env for a given patient.
# Setting this once per simulate_life avoids repeated S7 @-dispatch in hot paths.
.REC <- local({ e <- NULL; environment() })

.DATE_CACHE <- new.env(parent = emptyenv(), hash = TRUE)

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
  isTRUE(tryCatch(
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
  ))
}

.cond_gender <- function(cond, person) {
  toupper(person@attributes[["gender"]] %||% "") ==
    toupper(cond[["gender"]] %||% "")
}

.cond_age <- function(cond, person, time) {
  rec <- .REC$e
  birth_num <- rec$.birth_num
  if (is.null(birth_num)) {
    birth <- person@attributes[["birth_date"]]
    if (is.null(birth)) return(FALSE)
    birth_num <- as.numeric(birth)
    rec$.birth_num <- birth_num
  }
  t_num <- rec$.t_num %||% as.numeric(time)
  age   <- (t_num - birth_num) / (365.25 * 86400)
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
  key <- paste0(y, "-", m, "-", d)
  target <- .DATE_CACHE[[key]]
  if (is.null(target)) {
    target <- as.numeric(as.POSIXct(key))
    .DATE_CACHE[[key]] <- target
  }
  .compare(.REC$e$.t_num %||% as.numeric(time), cond[["operator"]] %||% "==", target)
}

.cond_race <- function(cond, person) {
  tolower(person@attributes[["race"]] %||% "") == tolower(cond[["race"]] %||% "")
}

.cond_attribute <- function(cond, person) {
  attr_val <- person@attributes[[cond[["attribute"]]]]
  op <- cond[["operator"]]
  if (identical(op, "is not nil")) return(!is.null(attr_val))
  if (identical(op, "is nil"))     return(is.null(attr_val))
  if (is.null(op) && is.null(cond[["value"]])) return(!is.null(attr_val))
  if (is.null(attr_val)) return(identical(op, "!="))
  target <- cond[["value"]] %||% cond[["value_code"]]
  .compare(attr_val, op %||% "==", target)
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
  target_code_value <- target_code[["code"]]
  latest <- .REC$e$.latest_observations[[target_code_value]]
  if (is.null(latest)) {
    obs_list <- .REC$e$observations
    matching <- Filter(function(o) {
      any(vapply(o$codes, function(c) c[["code"]] == target_code_value, logical(1)))
    }, obs_list)
    if (length(matching) == 0) return(FALSE)
    latest <- matching[[length(matching)]]
  }
  if (!is.null(cond[["value"]])) {
    .compare(latest$value, cond[["operator"]] %||% "==", cond[["value"]])
  } else TRUE
}

.cond_active_condition <- function(cond, person) {
  target_code <- (cond[["codes"]] %||% list())[[1L]][["code"]]
  if (!is.character(target_code) || length(target_code) != 1L) return(FALSE)
  !is.null(.REC$e$.active_conditions[[target_code]])
}

.cond_active_medication <- function(cond, person) {
  target_code <- (cond[["codes"]] %||% list())[[1L]][["code"]]
  if (!is.character(target_code) || length(target_code) != 1L) return(FALSE)
  !is.null(.REC$e$.active_medications[[target_code]])
}

.cond_active_careplan <- function(cond, person) {
  target_code <- (cond[["codes"]] %||% list())[[1L]][["code"]]
  if (!is.character(target_code) || length(target_code) != 1L) return(FALSE)
  !is.null(.REC$e$.active_careplans[[target_code]])
}

.cond_prior_state <- function(cond, person) {
  state_name <- cond[["name"]] %||% ""
  isTRUE(.REC$e[[paste0("__visited__", state_name)]])
}
