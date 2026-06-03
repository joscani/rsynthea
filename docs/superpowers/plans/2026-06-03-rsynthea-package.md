# rsynthea Package Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Implement an R package `rsynthea` that ports py-synthea (Python) to R, generating synthetic longitudinal EHR data using Synthea's Generic Module Framework (GMF) JSON modules.

**Architecture:** S7 classes with value (functional) semantics — every simulation function takes and returns modified objects. The GMF JSON modules from py-synthea are reused verbatim. Output is a named list of tibbles (patients, encounters, conditions, medications, procedures, observations, immunizations).

**Tech Stack:** R, S7, jsonlite, tibble, dplyr, yaml, testthat

---

## File Map

```
R/
  classes.R          # All S7 class definitions (Code, Entry hierarchy, HealthRecord, Person)
  logic.R            # evaluate_condition() — 15 condition types
  transition.R       # parse_transition() + resolve_transition()
  module.R           # load_module(), load_all_modules(), Module/State S7 classes
  state_flow.R       # Initial, Simple, Terminal, Delay, Guard, SetAttribute, Counter, Death
  state_clinical.R   # Encounter/End, Condition/End, Medication/End, Procedure, Allergy, CarePlan
  state_observe.R    # Observation, MultiObs, DiagnosticReport, VitalSign, Symptom, Vaccine, Imaging
  state_advanced.R   # CallSubmodule, Device/End, SupplyList
  demographics.R     # sample_demographics()
  simulation.R       # advance_module(), simulate_life()
  generator.R        # generate_population()
  export.R           # export_population() -> list of tibbles

inst/extdata/
  modules/           # 231 GMF JSON modules (copied from py-synthea repo)
  resources/         # biometrics.yml, names.yml, immunization_schedule.json

tests/testthat/
  test-classes.R
  test-logic.R
  test-transition.R
  test-module.R
  test-state-flow.R
  test-state-clinical.R
  test-simulation.R
  test-generator.R
  test-export.R

DESCRIPTION
NAMESPACE
```

---

## Task 1: Package scaffold

**Files:**
- Create: `DESCRIPTION`
- Create: `NAMESPACE`
- Create: `R/` (empty, will populate later)
- Create: `tests/testthat/` structure

- [ ] **Step 1: Initialize package structure**

```bash
cd /Users/jlcanadas/proyectos/proyectos_personales/r-synthea
Rscript -e '
  usethis::create_package(".", open = FALSE, rstudio = FALSE)
  usethis::use_testthat()
  usethis::use_package("S7")
  usethis::use_package("jsonlite")
  usethis::use_package("tibble")
  usethis::use_package("dplyr")
  usethis::use_package("yaml")
  usethis::use_package("testthat", type = "Suggests")
'
```

- [ ] **Step 2: Edit DESCRIPTION to set correct metadata**

Replace the generated DESCRIPTION with:

```
Package: rsynthea
Title: Synthetic Patient Population Simulator
Version: 0.1.0
Authors@R: person("Given", "Family", role = c("aut", "cre"), email = "jlcanadas@idealista.com")
Description: Generates synthetic longitudinal electronic health records using
    Synthea's Generic Module Framework (GMF). Port of py-synthea to R.
License: Apache License (>= 2)
Encoding: UTF-8
Roxygen: list(markdown = TRUE)
RoxygenNote: 7.0.0
Imports:
    S7,
    jsonlite,
    tibble,
    dplyr,
    yaml
Suggests:
    testthat (>= 3.0.0)
Config/testthat/edition: 3
```

- [ ] **Step 3: Initialize renv**

```bash
Rscript -e 'renv::init()'
```

- [ ] **Step 4: Verify package loads**

```bash
Rscript -e 'devtools::load_all(); message("OK")'
```

Expected: `OK` with no errors.

- [ ] **Step 5: Commit**

```bash
git init
git add DESCRIPTION NAMESPACE R/ tests/ renv.lock .Rprofile
git commit -m "chore: initialize rsynthea package scaffold"
```

---

## Task 2: Core S7 data classes

**Files:**
- Create: `R/classes.R`
- Create: `tests/testthat/test-classes.R`

- [ ] **Step 1: Write failing tests**

```r
# tests/testthat/test-classes.R
library(testthat)
library(S7)

test_that("Code stores system, code, display", {
  c1 <- Code(system = "SNOMED-CT", code = "44054006", display = "Diabetes mellitus type 2")
  expect_equal(c1@system, "SNOMED-CT")
  expect_equal(c1@code, "44054006")
  expect_equal(c1@display, "Diabetes mellitus type 2")
})

test_that("Encounter inherits Entry and has encounter_class", {
  enc <- Encounter(
    id        = "enc-001",
    time      = as.POSIXct("2020-01-01"),
    codes     = list(Code(system = "CPT", code = "99213", display = "Office visit")),
    encounter_class = "ambulatory"
  )
  expect_equal(enc@encounter_class, "ambulatory")
  expect_s3_class(enc, "rsynthea_Encounter")
})

test_that("Condition stores onset_time and is_active = TRUE by default", {
  cond <- Condition(
    id = "cond-001",
    time = as.POSIXct("2020-03-01"),
    codes = list(Code(system = "SNOMED-CT", code = "44054006", display = "T2DM"))
  )
  expect_true(cond@is_active)
  expect_null(cond@end_time)
})

test_that("HealthRecord starts with empty lists", {
  hr <- HealthRecord()
  expect_equal(length(hr@encounters),    0L)
  expect_equal(length(hr@conditions),    0L)
  expect_equal(length(hr@medications),   0L)
  expect_equal(length(hr@procedures),    0L)
  expect_equal(length(hr@observations),  0L)
  expect_equal(length(hr@immunizations), 0L)
  expect_equal(length(hr@allergies),     0L)
  expect_equal(length(hr@careplans),     0L)
})
```

- [ ] **Step 2: Run tests to confirm they fail**

```bash
Rscript -e 'testthat::test_file("tests/testthat/test-classes.R")'
```

Expected: errors — `Code` not found.

- [ ] **Step 3: Implement classes**

```r
# R/classes.R
library(S7)

Code <- new_class("Code",
  package = "rsynthea",
  properties = list(
    system  = class_character,
    code    = class_character,
    display = class_character
  )
)

Entry <- new_class("Entry",
  package = "rsynthea",
  properties = list(
    id    = class_character,
    time  = class_any,   # POSIXct
    codes = class_list,  # list<Code>
    name  = new_property(class = class_any, default = NULL)
  )
)

Encounter <- new_class("Encounter",
  package    = "rsynthea",
  parent     = Entry,
  properties = list(
    encounter_class      = class_character,
    provider_id          = new_property(class = class_any, default = NULL),
    reason_code          = new_property(class = class_any, default = NULL),
    end_time             = new_property(class = class_any, default = NULL),
    conditions           = new_property(class = class_list, default = list()),
    procedures           = new_property(class = class_list, default = list()),
    medications          = new_property(class = class_list, default = list()),
    observations         = new_property(class = class_list, default = list()),
    careplans            = new_property(class = class_list, default = list()),
    immunizations        = new_property(class = class_list, default = list()),
    imaging_studies      = new_property(class = class_list, default = list()),
    devices              = new_property(class = class_list, default = list()),
    supplies             = new_property(class = class_list, default = list()),
    diagnostic_reports   = new_property(class = class_list, default = list())
  )
)

Condition <- new_class("Condition",
  package    = "rsynthea",
  parent     = Entry,
  properties = list(
    is_active  = new_property(class = class_logical, default = TRUE),
    end_time   = new_property(class = class_any, default = NULL),
    cause      = new_property(class = class_any, default = NULL)
  )
)

Medication <- new_class("Medication",
  package    = "rsynthea",
  parent     = Entry,
  properties = list(
    is_active    = new_property(class = class_logical, default = TRUE),
    end_time     = new_property(class = class_any, default = NULL),
    reasons      = new_property(class = class_list, default = list()),
    dosage       = new_property(class = class_any, default = NULL),
    duration     = new_property(class = class_any, default = NULL),
    prescription = new_property(class = class_any, default = NULL)
  )
)

Procedure <- new_class("Procedure",
  package    = "rsynthea",
  parent     = Entry,
  properties = list(
    reasons   = new_property(class = class_list, default = list()),
    duration  = new_property(class = class_any, default = NULL)
  )
)

Observation <- new_class("Observation",
  package    = "rsynthea",
  parent     = Entry,
  properties = list(
    value      = new_property(class = class_any, default = NULL),
    unit       = new_property(class = class_any, default = NULL),
    category   = new_property(class = class_any, default = NULL),
    obs_type   = new_property(class = class_any, default = NULL)
  )
)

DiagnosticReport <- new_class("DiagnosticReport",
  package    = "rsynthea",
  parent     = Entry,
  properties = list(
    observations = new_property(class = class_list, default = list())
  )
)

Immunization <- new_class("Immunization",
  package    = "rsynthea",
  parent     = Entry,
  properties = list()
)

AllergyIntolerance <- new_class("AllergyIntolerance",
  package    = "rsynthea",
  parent     = Entry,
  properties = list(
    is_active    = new_property(class = class_logical, default = TRUE),
    end_time     = new_property(class = class_any, default = NULL),
    allergy_type = new_property(class = class_any, default = NULL),
    category     = new_property(class = class_any, default = NULL)
  )
)

CarePlan <- new_class("CarePlan",
  package    = "rsynthea",
  parent     = Entry,
  properties = list(
    is_active    = new_property(class = class_logical, default = TRUE),
    end_time     = new_property(class = class_any, default = NULL),
    reasons      = new_property(class = class_list, default = list()),
    activities   = new_property(class = class_list, default = list())
  )
)

ImagingStudy <- new_class("ImagingStudy",
  package    = "rsynthea",
  parent     = Entry,
  properties = list(
    series = new_property(class = class_list, default = list())
  )
)

Device <- new_class("Device",
  package    = "rsynthea",
  parent     = Entry,
  properties = list(
    is_active = new_property(class = class_logical, default = TRUE),
    end_time  = new_property(class = class_any, default = NULL),
    udi       = new_property(class = class_any, default = NULL)
  )
)

HealthRecord <- new_class("HealthRecord",
  package    = "rsynthea",
  properties = list(
    encounters    = new_property(class = class_list, default = list()),
    conditions    = new_property(class = class_list, default = list()),
    medications   = new_property(class = class_list, default = list()),
    procedures    = new_property(class = class_list, default = list()),
    observations  = new_property(class = class_list, default = list()),
    immunizations = new_property(class = class_list, default = list()),
    allergies     = new_property(class = class_list, default = list()),
    careplans     = new_property(class = class_list, default = list()),
    imaging       = new_property(class = class_list, default = list()),
    devices       = new_property(class = class_list, default = list()),
    reports       = new_property(class = class_list, default = list()),
    supplies      = new_property(class = class_list, default = list())
  )
)
```

- [ ] **Step 4: Run tests to confirm they pass**

```bash
Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-classes.R")'
```

Expected: 4 tests, 0 failures.

- [ ] **Step 5: Commit**

```bash
git add R/classes.R tests/testthat/test-classes.R
git commit -m "feat: add core S7 data classes (Code, Entry hierarchy, HealthRecord)"
```

---

## Task 3: Person class

**Files:**
- Create: `R/person.R` (add to `R/classes.R` or separate — keep in `classes.R`)
- Modify: `R/classes.R`
- Create: `tests/testthat/test-person.R`

- [ ] **Step 1: Write failing tests**

```r
# tests/testthat/test-person.R
library(testthat)

test_that("Person initializes with seed and empty record", {
  p <- Person(seed = 42L)
  expect_equal(p@seed, 42L)
  expect_true(is.character(p@id) && nchar(p@id) > 0)
  expect_equal(length(p@attributes), 0L)
  expect_s3_class(p@health_record, "rsynthea_HealthRecord")
})

test_that("age_at returns correct age", {
  p <- Person(seed = 1L)
  p@attributes[["birth_date"]] <- as.POSIXct("1990-01-01")
  expect_equal(floor(age_at(p, as.POSIXct("2020-01-01"))), 30)
})

test_that("is_alive is TRUE by default, FALSE after death", {
  p <- Person(seed = 1L)
  expect_true(p@is_alive)
  p@is_alive <- FALSE
  expect_false(p@is_alive)
})

test_that("module_history starts empty", {
  p <- Person(seed = 1L)
  expect_equal(length(p@module_history), 0L)
})
```

- [ ] **Step 2: Run tests to confirm they fail**

```bash
Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-person.R")'
```

- [ ] **Step 3: Add Person to `R/classes.R`**

Append to `R/classes.R`:

```r
Person <- new_class("Person",
  package    = "rsynthea",
  properties = list(
    seed           = class_integer,
    id             = class_character,
    is_alive       = new_property(class = class_logical, default = TRUE),
    attributes     = new_property(class = class_list, default = list()),
    vital_signs    = new_property(class = class_list, default = list()),
    symptoms       = new_property(class = class_list, default = list()),
    module_history = new_property(class = class_list, default = list()),
    health_record  = new_property(
      class   = class_any,
      default = NULL,
      getter  = function(self) {
        if (is.null(self@.record)) HealthRecord() else self@.record
      },
      setter  = function(self, value) {
        self@.record <- value
        self
      }
    ),
    .record = new_property(class = class_any, default = NULL)
  ),
  constructor = function(seed = NULL) {
    seed <- if (is.null(seed)) sample.int(.Machine$integer.max, 1L) else as.integer(seed)
    id   <- substr(digest::digest(seed, algo = "md5"), 1, 16)
    new_object(S7_object(), seed = seed, id = id, .record = HealthRecord())
  }
)

# Generic: age_at(person, time) -> numeric years
age_at <- new_generic("age_at", "person")
method(age_at, Person) <- function(person, time) {
  birth <- person@attributes[["birth_date"]]
  if (is.null(birth)) return(0)
  as.numeric(difftime(time, birth, units = "days")) / 365.25
}
```

> Note: `digest` is needed for ID generation — add to DESCRIPTION Imports.

- [ ] **Step 3b: Add digest to dependencies**

```bash
Rscript -e 'usethis::use_package("digest")'
```

- [ ] **Step 4: Run tests**

```bash
Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-person.R")'
```

Expected: 4 tests, 0 failures.

- [ ] **Step 5: Commit**

```bash
git add R/classes.R tests/testthat/test-person.R DESCRIPTION
git commit -m "feat: add Person S7 class with age_at generic"
```

---

## Task 4: Logic / condition evaluation

**Files:**
- Create: `R/logic.R`
- Create: `tests/testthat/test-logic.R`

- [ ] **Step 1: Write failing tests**

```r
# tests/testthat/test-logic.R
library(testthat)

make_person <- function(gender = "M", birth_year = 1980, race = "white",
                        attributes = list()) {
  p <- Person(seed = 1L)
  p@attributes <- c(
    list(
      gender     = gender,
      birth_date = as.POSIXct(paste0(birth_year, "-01-01")),
      race       = race
    ),
    attributes
  )
  p
}

test_that("Gender condition matches correctly", {
  p <- make_person(gender = "M")
  expect_true(evaluate_condition(list(condition_type = "Gender", gender = "M"), p, Sys.time()))
  expect_false(evaluate_condition(list(condition_type = "Gender", gender = "F"), p, Sys.time()))
})

test_that("Age condition uses operator", {
  p <- make_person(birth_year = 1980)
  time <- as.POSIXct("2020-01-01")
  expect_true(evaluate_condition(
    list(condition_type = "Age", operator = ">=", quantity = 30, unit = "years"), p, time))
  expect_false(evaluate_condition(
    list(condition_type = "Age", operator = ">", quantity = 50, unit = "years"), p, time))
})

test_that("And condition requires all sub-conditions", {
  p <- make_person(gender = "M", birth_year = 1980)
  time <- as.POSIXct("2020-01-01")
  cond <- list(
    condition_type = "And",
    conditions = list(
      list(condition_type = "Gender", gender = "M"),
      list(condition_type = "Age", operator = ">=", quantity = 30, unit = "years")
    )
  )
  expect_true(evaluate_condition(cond, p, time))
})

test_that("Or condition requires at least one", {
  p <- make_person(gender = "F")
  time <- Sys.time()
  cond <- list(
    condition_type = "Or",
    conditions = list(
      list(condition_type = "Gender", gender = "M"),
      list(condition_type = "Gender", gender = "F")
    )
  )
  expect_true(evaluate_condition(cond, p, time))
})

test_that("Not inverts", {
  p <- make_person(gender = "M")
  cond <- list(
    condition_type = "Not",
    condition = list(condition_type = "Gender", gender = "F")
  )
  expect_true(evaluate_condition(cond, p, Sys.time()))
})

test_that("Attribute condition with == operator", {
  p <- make_person(attributes = list(diabetes = TRUE))
  expect_true(evaluate_condition(
    list(condition_type = "Attribute", attribute = "diabetes", operator = "==", value = TRUE),
    p, Sys.time()
  ))
})

test_that("Active Condition checks health record", {
  p <- make_person()
  dm_code <- Code(system = "SNOMED-CT", code = "44054006", display = "T2DM")
  cond_entry <- Condition(id = "c1", time = Sys.time(), codes = list(dm_code))
  p@health_record@conditions <- list(cond_entry)

  expect_true(evaluate_condition(
    list(condition_type = "Active Condition",
         codes = list(list(system = "SNOMED-CT", code = "44054006", display = "T2DM"))),
    p, Sys.time()
  ))
})

test_that("Unknown condition_type returns FALSE", {
  p <- make_person()
  expect_false(evaluate_condition(list(condition_type = "WhatIsThis"), p, Sys.time()))
})
```

- [ ] **Step 2: Run to confirm failures**

```bash
Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-logic.R")'
```

- [ ] **Step 3: Implement `R/logic.R`**

```r
# R/logic.R

evaluate_condition <- function(cond, person, time) {
  if (is.null(cond) || length(cond) == 0) return(TRUE)
  ct <- cond[["condition_type"]]

  switch(ct,
    "And"               = all(vapply(cond$conditions, evaluate_condition, logical(1),
                                    person = person, time = time)),
    "Or"                = any(vapply(cond$conditions, evaluate_condition, logical(1),
                                    person = person, time = time)),
    "Not"               = !evaluate_condition(cond$condition, person, time),
    "Gender"            = .cond_gender(cond, person),
    "Age"               = .cond_age(cond, person, time),
    "Date"              = .cond_date(cond, time),
    "Race"              = .cond_race(cond, person),
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
    FALSE  # unknown
  )
}

# Null-coalescing operator
`%||%` <- function(a, b) if (!is.null(a)) a else b

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
    qty  # years
  )
  .compare(age, cond[["operator"]] %||% "==", qty)
}

.cond_date <- function(cond, time) {
  y <- cond[["year"]]; m <- cond[["month"]] %||% 1; d <- cond[["day"]] %||% 1
  if (is.null(y)) return(FALSE)
  target <- as.POSIXct(paste(y, m, d, sep = "-"))
  .compare(as.numeric(time), cond[["operator"]] %||% "==", as.numeric(target))
}

.cond_race <- function(cond, person) {
  tolower(person@attributes[["race"]] %||% "") == tolower(cond[["race"]] %||% "")
}

.cond_attribute <- function(cond, person) {
  attr_val <- person@attributes[[cond[["attribute"]]]]
  if (is.null(cond[["operator"]]) && is.null(cond[["value"]])) return(!is.null(attr_val))
  if (is.null(attr_val)) return(cond[["operator"]] == "!=")
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
```

- [ ] **Step 4: Run tests**

```bash
Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-logic.R")'
```

Expected: 8 tests, 0 failures.

- [ ] **Step 5: Commit**

```bash
git add R/logic.R tests/testthat/test-logic.R
git commit -m "feat: implement evaluate_condition for all 15 GMF condition types"
```

---

## Task 5: Transition system

**Files:**
- Create: `R/transition.R`
- Create: `tests/testthat/test-transition.R`

- [ ] **Step 1: Write failing tests**

```r
# tests/testthat/test-transition.R
library(testthat)

test_that("parse_transition returns 'direct' type for direct_transition key", {
  t <- parse_transition(list(direct_transition = "Next_State"))
  expect_equal(t$type, "direct")
  expect_equal(t$target, "Next_State")
})

test_that("resolve_transition on direct always returns same state", {
  t <- parse_transition(list(direct_transition = "Next_State"))
  p <- Person(seed = 1L)
  results <- replicate(10, resolve_transition(t, p, Sys.time()))
  expect_true(all(results == "Next_State"))
})

test_that("resolve_transition on distributed respects weights", {
  set.seed(42)
  t <- parse_transition(list(distributed_transition = list(
    list(distribution = 0.9, transition = "Likely"),
    list(distribution = 0.1, transition = "Unlikely")
  )))
  p <- Person(seed = 1L)
  results <- replicate(1000, resolve_transition(t, p, Sys.time()))
  expect_gt(mean(results == "Likely"), 0.8)
})

test_that("resolve_transition on conditional picks first matching", {
  t <- parse_transition(list(conditional_transition = list(
    list(
      condition  = list(condition_type = "Gender", gender = "F"),
      transition = "Female_Branch"
    ),
    list(transition = "Default_Branch")
  )))
  p <- Person(seed = 1L)
  p@attributes[["gender"]] <- "F"
  expect_equal(resolve_transition(t, p, Sys.time()), "Female_Branch")

  p2 <- Person(seed = 2L)
  p2@attributes[["gender"]] <- "M"
  expect_equal(resolve_transition(t, p2, Sys.time()), "Default_Branch")
})

test_that("NULL transition returned when no transition key present", {
  t <- parse_transition(list())
  expect_null(t)
})
```

- [ ] **Step 2: Run to confirm failures**

```bash
Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-transition.R")'
```

- [ ] **Step 3: Implement `R/transition.R`**

```r
# R/transition.R

parse_transition <- function(state_def) {
  if ("direct_transition" %in% names(state_def)) {
    list(type = "direct", target = state_def[["direct_transition"]])
  } else if ("transition" %in% names(state_def)) {
    list(type = "direct", target = state_def[["transition"]])
  } else if ("distributed_transition" %in% names(state_def)) {
    list(type = "distributed", entries = state_def[["distributed_transition"]])
  } else if ("conditional_transition" %in% names(state_def)) {
    list(type = "conditional", entries = state_def[["conditional_transition"]])
  } else if ("complex_transition" %in% names(state_def)) {
    list(type = "complex", entries = state_def[["complex_transition"]])
  } else if ("lookup_table_transition" %in% names(state_def)) {
    list(type = "lookup_table", entries = state_def[["lookup_table_transition"]])
  } else {
    NULL
  }
}

resolve_transition <- function(transition, person, time) {
  if (is.null(transition)) return(NULL)
  switch(transition$type,
    "direct"       = transition$target,
    "distributed"  = .resolve_distributed(transition$entries, person),
    "conditional"  = .resolve_conditional(transition$entries, person, time),
    "complex"      = .resolve_complex(transition$entries, person, time),
    "lookup_table" = .resolve_lookup(transition$entries, person, time),
    NULL
  )
}

.resolve_weight <- function(dist_val, person) {
  if (is.list(dist_val)) {
    attr_name <- dist_val[["attribute"]]
    default   <- as.numeric(dist_val[["default"]] %||% 0)
    if (!is.null(attr_name)) as.numeric(person@attributes[[attr_name]] %||% default)
    else default
  } else {
    as.numeric(dist_val %||% 0)
  }
}

.resolve_distributed <- function(entries, person) {
  weights <- vapply(entries, function(e) .resolve_weight(e[["distribution"]], person), numeric(1))
  total <- sum(weights)
  if (total <= 0) return(entries[[length(entries)]][["transition"]])
  r <- runif(1) * total
  cumw <- 0
  for (i in seq_along(entries)) {
    cumw <- cumw + weights[[i]]
    if (r < cumw) return(entries[[i]][["transition"]])
  }
  entries[[length(entries)]][["transition"]]
}

.resolve_conditional <- function(entries, person, time) {
  for (e in entries) {
    if (is.null(e[["condition"]]) || evaluate_condition(e[["condition"]], person, time)) {
      return(e[["transition"]])
    }
  }
  NULL
}

.resolve_complex <- function(entries, person, time) {
  matching <- Filter(function(e) {
    is.null(e[["condition"]]) || evaluate_condition(e[["condition"]], person, time)
  }, entries)
  if (length(matching) == 0) return(NULL)
  first <- matching[[1]]
  if (!is.null(first[["distributions"]])) {
    .resolve_distributed(first[["distributions"]], person)
  } else {
    first[["transition"]]
  }
}

.resolve_lookup <- function(entries, person, time) {
  weights <- vapply(entries, function(e) as.numeric(e[["default_probability"]] %||% 0), numeric(1))
  total <- sum(weights)
  if (total <= 0) return(entries[[length(entries)]][["transition"]])
  r <- runif(1) * total
  cumw <- 0
  for (i in seq_along(entries)) {
    cumw <- cumw + weights[[i]]
    if (r < cumw) return(entries[[i]][["transition"]])
  }
  entries[[length(entries)]][["transition"]]
}
```

- [ ] **Step 4: Run tests**

```bash
Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-transition.R")'
```

Expected: 5 tests, 0 failures.

- [ ] **Step 5: Commit**

```bash
git add R/transition.R tests/testthat/test-transition.R
git commit -m "feat: implement GMF transition system (direct, distributed, conditional, complex)"
```

---

## Task 6: Module loading

**Files:**
- Create: `R/module.R`
- Create: `tests/testthat/test-module.R`
- Create: `inst/extdata/modules/test_cold.json` (minimal test module)

- [ ] **Step 1: Create a minimal test module JSON**

```json
{
  "name": "test_cold",
  "states": {
    "Initial": {
      "type": "Initial",
      "distributed_transition": [
        {"distribution": 0.3, "transition": "Cold_Onset"},
        {"distribution": 0.7, "transition": "Terminal"}
      ]
    },
    "Cold_Onset": {
      "type": "ConditionOnset",
      "codes": [{"system": "SNOMED-CT", "code": "82272006", "display": "Common cold"}],
      "direct_transition": "Cold_Duration"
    },
    "Cold_Duration": {
      "type": "Delay",
      "range": {"low": 7, "high": 14, "unit": "days"},
      "direct_transition": "Cold_Resolves"
    },
    "Cold_Resolves": {
      "type": "ConditionEnd",
      "condition_onset": "Cold_Onset",
      "direct_transition": "Terminal"
    },
    "Terminal": {"type": "Terminal"}
  }
}
```

Save to: `inst/extdata/modules/test_cold.json`

- [ ] **Step 2: Write failing tests**

```r
# tests/testthat/test-module.R
library(testthat)

test_module_path <- system.file("extdata/modules/test_cold.json", package = "rsynthea")

test_that("load_module returns a Module with correct name", {
  skip_if_not(file.exists(test_module_path))
  m <- load_module(test_module_path)
  expect_s3_class(m, "rsynthea_Module")
  expect_equal(m@name, "test_cold")
})

test_that("load_module parses all states", {
  skip_if_not(file.exists(test_module_path))
  m <- load_module(test_module_path)
  expect_setequal(names(m@states),
    c("Initial", "Cold_Onset", "Cold_Duration", "Cold_Resolves", "Terminal"))
})

test_that("state types parsed correctly", {
  skip_if_not(file.exists(test_module_path))
  m <- load_module(test_module_path)
  expect_equal(m@states[["Initial"]]@type, "Initial")
  expect_equal(m@states[["Cold_Duration"]]@type, "Delay")
  expect_equal(m@states[["Terminal"]]@type, "Terminal")
})

test_that("state transition parsed for Initial state", {
  skip_if_not(file.exists(test_module_path))
  m <- load_module(test_module_path)
  t <- m@states[["Initial"]]@transition
  expect_equal(t$type, "distributed")
})
```

- [ ] **Step 3: Run to confirm failures**

```bash
Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-module.R")'
```

- [ ] **Step 4: Implement `R/module.R`**

```r
# R/module.R

GMFState <- new_class("GMFState",
  package    = "rsynthea",
  properties = list(
    name       = class_character,
    type       = class_character,
    definition = class_list,
    transition = new_property(class = class_any, default = NULL)
  )
)

Module <- new_class("Module",
  package    = "rsynthea",
  properties = list(
    name       = class_character,
    states     = class_list,  # named list<GMFState>
    submodules = new_property(class = class_list, default = list())
  )
)

load_module <- function(path) {
  raw <- jsonlite::read_json(path, simplifyVector = FALSE)
  name <- raw[["name"]] %||% tools::file_path_sans_ext(basename(path))
  states_raw <- raw[["states"]] %||% list()
  states <- lapply(states_raw, function(s) {
    GMFState(
      name       = s[["name"]] %||% "",
      type       = s[["type"]] %||% "Simple",
      definition = s,
      transition = parse_transition(s)
    )
  })
  # Populate name field from the list key (JSON object keys = state names)
  states <- Map(function(s, nm) {
    s@name <- nm
    s
  }, states, names(states_raw))
  Module(name = name, states = states)
}

load_all_modules <- function(modules_dir = NULL) {
  if (is.null(modules_dir)) {
    modules_dir <- system.file("extdata/modules", package = "rsynthea")
  }
  json_files <- list.files(modules_dir, pattern = "\\.json$", full.names = TRUE, recursive = TRUE)
  modules <- lapply(json_files, function(f) {
    tryCatch(load_module(f), error = function(e) {
      warning("Failed to load module: ", f, " — ", conditionMessage(e))
      NULL
    })
  })
  modules <- Filter(Negate(is.null), modules)
  stats::setNames(modules, vapply(modules, function(m) m@name, character(1)))
}
```

- [ ] **Step 5: Run tests**

```bash
Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-module.R")'
```

Expected: 4 tests, 0 failures.

- [ ] **Step 6: Commit**

```bash
git add R/module.R tests/testthat/test-module.R inst/extdata/modules/test_cold.json
git commit -m "feat: implement GMF module loader — load_module() and load_all_modules()"
```

---

## Task 7: State machine — flow control states

**Files:**
- Create: `R/state_flow.R`
- Create: `tests/testthat/test-state-flow.R`

These are the states that control simulation flow without adding clinical entries:
`Initial`, `Simple`, `Terminal`, `Delay`, `Guard`, `SetAttribute`, `Counter`, `Death`

- [ ] **Step 1: Write failing tests**

```r
# tests/testthat/test-state-flow.R
library(testthat)

make_state <- function(type, extra = list()) {
  GMFState(name = "Test", type = type,
           definition = c(list(type = type), extra),
           transition = parse_transition(c(list(type = type, direct_transition = "Next"), extra)))
}

make_person_simple <- function() {
  p <- Person(seed = 1L)
  p@attributes[["birth_date"]] <- as.POSIXct("1990-01-01")
  p@attributes[["gender"]] <- "M"
  p
}

test_that("process_state on Initial returns person unchanged, transitions out", {
  state <- make_state("Initial")
  p <- make_person_simple()
  result <- process_state(state, p, Sys.time())
  expect_equal(result$next_state, "Next")
  expect_true(result$person@is_alive)
})

test_that("process_state on Terminal marks module done", {
  state <- GMFState(name = "Terminal", type = "Terminal",
                    definition = list(type = "Terminal"), transition = NULL)
  p <- make_person_simple()
  result <- process_state(state, p, Sys.time())
  expect_null(result$next_state)
})

test_that("process_state on Delay blocks until delay elapsed", {
  state <- GMFState(
    name = "WaitState", type = "Delay",
    definition = list(type = "Delay",
                      exact = list(quantity = 30, unit = "days"),
                      direct_transition = "AfterDelay"),
    transition = parse_transition(list(direct_transition = "AfterDelay"))
  )
  p <- make_person_simple()
  now <- as.POSIXct("2020-01-01")

  # First call: delay starts, should NOT transition yet
  result1 <- process_state(state, p, now)
  expect_equal(result1$next_state, "WaitState")  # stays in Delay

  # After 30 days: should transition
  p2 <- result1$person
  result2 <- process_state(state, p2, now + 31 * 86400)
  expect_equal(result2$next_state, "AfterDelay")
})

test_that("process_state on Guard blocks when condition false, transitions when true", {
  state <- GMFState(
    name = "GuardTest", type = "Guard",
    definition = list(
      type = "Guard",
      allow = list(condition_type = "Gender", gender = "M"),
      direct_transition = "Allowed"
    ),
    transition = parse_transition(list(direct_transition = "Allowed"))
  )
  p_male <- make_person_simple()
  result_male <- process_state(state, p_male, Sys.time())
  expect_equal(result_male$next_state, "Allowed")

  p_female <- make_person_simple()
  p_female@attributes[["gender"]] <- "F"
  result_female <- process_state(state, p_female, Sys.time())
  expect_equal(result_female$next_state, "GuardTest")  # stays
})

test_that("process_state on SetAttribute sets attribute on person", {
  state <- GMFState(
    name = "SetDiabetes", type = "SetAttribute",
    definition = list(type = "SetAttribute", attribute = "diabetes", value = TRUE,
                      direct_transition = "Next"),
    transition = parse_transition(list(direct_transition = "Next"))
  )
  p <- make_person_simple()
  result <- process_state(state, p, Sys.time())
  expect_true(result$person@attributes[["diabetes"]])
  expect_equal(result$next_state, "Next")
})

test_that("process_state on Death marks person not alive", {
  state <- GMFState(
    name = "Die", type = "Death",
    definition = list(type = "Death"), transition = NULL
  )
  p <- make_person_simple()
  result <- process_state(state, p, as.POSIXct("2020-06-01"))
  expect_false(result$person@is_alive)
  expect_false(is.null(result$person@attributes[["death_date"]]))
})

test_that("process_state on Counter increments attribute", {
  state <- GMFState(
    name = "CountIt", type = "Counter",
    definition = list(type = "Counter", attribute = "visit_count",
                      action = "increment", direct_transition = "Next"),
    transition = parse_transition(list(direct_transition = "Next"))
  )
  p <- make_person_simple()
  result <- process_state(state, p, Sys.time())
  expect_equal(result$person@attributes[["visit_count"]], 1)
  result2 <- process_state(state, result$person, Sys.time())
  expect_equal(result2$person@attributes[["visit_count"]], 2)
})
```

- [ ] **Step 2: Run to confirm failures**

```bash
Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-state-flow.R")'
```

- [ ] **Step 3: Implement `R/state_flow.R`**

```r
# R/state_flow.R

# Generic: process_state(state, person, time) -> list(person, next_state)
# next_state is the name of the next state, or NULL if Terminal
process_state <- new_generic("process_state", "state")

# Dispatch by type string (S7 can't dispatch on character, so we wrap)
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
    "Procedure"        = .state_procedure(state, person, time),
    "Observation"      = .state_observation(state, person, time),
    "MultiObservation" = .state_multi_observation(state, person, time),
    "DiagnosticReport" = .state_diagnostic_report(state, person, time),
    "VitalSign"        = .state_vital_sign(state, person, time),
    "Symptom"          = .state_symptom(state, person, time),
    "AllergyOnset"     = .state_allergy_onset(state, person, time),
    "AllergyEnd"       = .state_allergy_end(state, person, time),
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

# Helper: mark state visited and resolve transition
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
    # First entry: compute delay end time
    duration_secs <- .resolve_duration(def, person)
    delay_until <- time + duration_secs
    person@attributes[[key]] <- delay_until
    return(list(person = person, next_state = state@name))  # stay
  }
  if (time < delay_until) {
    return(list(person = person, next_state = state@name))  # still waiting
  }
  # Clear delay key and transition
  person@attributes[[key]] <- NULL
  .next(state, person, time)
}

.resolve_duration <- function(def, person) {
  unit_to_secs <- c(years = 365.25 * 86400, months = 30.44 * 86400,
                    weeks = 7 * 86400, days = 86400, hours = 3600)
  if (!is.null(def[["exact"]])) {
    qty  <- as.numeric(def[["exact"]][["quantity"]])
    unit <- def[["exact"]][["unit"]] %||% "days"
    return(qty * (unit_to_secs[[unit]] %||% 86400))
  }
  if (!is.null(def[["range"]])) {
    low  <- as.numeric(def[["range"]][["low"]])
    high <- as.numeric(def[["range"]][["high"]])
    unit <- def[["range"]][["unit"]] %||% "days"
    qty  <- runif(1, low, high)
    return(qty * (unit_to_secs[[unit]] %||% 86400))
  }
  0
}

.state_guard <- function(state, person, time) {
  allow <- state@definition[["allow"]]
  if (!is.null(allow) && !evaluate_condition(allow, person, time)) {
    return(list(person = person, next_state = state@name))  # blocked
  }
  .next(state, person, time)
}

.state_set_attribute <- function(state, person, time) {
  def <- state@definition
  attr_name <- def[["attribute"]]
  if (!is.null(attr_name)) {
    val <- if (!is.null(def[["value"]])) def[["value"]] else NULL
    # Attribute may reference another attribute
    if (is.character(val) && val %in% names(person@attributes)) {
      val <- person@attributes[[val]]
    }
    person@attributes[[attr_name]] <- val
  }
  .next(state, person, time)
}

.state_counter <- function(state, person, time) {
  def <- state@definition
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
  person@attributes[["cause_of_death"]] <- state@definition[["codes"]]
  list(person = person, next_state = NULL)
}
```

- [ ] **Step 4: Run tests**

```bash
Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-state-flow.R")'
```

Expected: 7 tests, 0 failures.

- [ ] **Step 5: Commit**

```bash
git add R/state_flow.R tests/testthat/test-state-flow.R
git commit -m "feat: implement flow control states (Initial, Delay, Guard, SetAttribute, Counter, Death)"
```

---

## Task 8: State machine — clinical states

**Files:**
- Create: `R/state_clinical.R`
- Modify: `R/state_flow.R` (the dispatch switch already routes to these functions)
- Create: `tests/testthat/test-state-clinical.R`

- [ ] **Step 1: Write failing tests**

```r
# tests/testthat/test-state-clinical.R
library(testthat)

make_person_clinical <- function() {
  p <- Person(seed = 1L)
  p@attributes[["birth_date"]] <- as.POSIXct("1980-01-01")
  p@attributes[["gender"]] <- "M"
  p
}

test_that("ConditionOnset adds active condition to health record", {
  state <- GMFState(
    name = "DiabetesOnset", type = "ConditionOnset",
    definition = list(
      type = "ConditionOnset",
      codes = list(list(system = "SNOMED-CT", code = "44054006", display = "T2DM")),
      direct_transition = "Next"
    ),
    transition = parse_transition(list(direct_transition = "Next"))
  )
  p <- make_person_clinical()
  result <- process_state(state, p, as.POSIXct("2020-01-01"))
  conds <- result$person@health_record@conditions
  expect_equal(length(conds), 1L)
  expect_true(conds[[1]]@is_active)
  expect_equal(conds[[1]]@codes[[1]]@code, "44054006")
})

test_that("ConditionEnd deactivates the matching condition", {
  p <- make_person_clinical()
  dm_code <- Code(system = "SNOMED-CT", code = "44054006", display = "T2DM")
  active_cond <- Condition(id = "c1", time = as.POSIXct("2019-01-01"), codes = list(dm_code))
  p@health_record@conditions <- list(active_cond)
  p@attributes[["__condition_ref__DiabetesOnset"]] <- "c1"

  state <- GMFState(
    name = "DiabetesEnd", type = "ConditionEnd",
    definition = list(type = "ConditionEnd", condition_onset = "DiabetesOnset",
                      direct_transition = "Next"),
    transition = parse_transition(list(direct_transition = "Next"))
  )
  result <- process_state(state, p, as.POSIXct("2022-01-01"))
  conds <- result$person@health_record@conditions
  expect_false(conds[[1]]@is_active)
  expect_false(is.null(conds[[1]]@end_time))
})

test_that("MedicationOrder adds active medication", {
  state <- GMFState(
    name = "PrescribeMetformin", type = "MedicationOrder",
    definition = list(
      type = "MedicationOrder",
      codes = list(list(system = "RxNorm", code = "860975", display = "Metformin")),
      direct_transition = "Next"
    ),
    transition = parse_transition(list(direct_transition = "Next"))
  )
  p <- make_person_clinical()
  result <- process_state(state, p, as.POSIXct("2020-03-01"))
  meds <- result$person@health_record@medications
  expect_equal(length(meds), 1L)
  expect_true(meds[[1]]@is_active)
})

test_that("Encounter opens an encounter and sets current_encounter", {
  state <- GMFState(
    name = "AnnualVisit", type = "Encounter",
    definition = list(
      type = "Encounter",
      encounter_class = "ambulatory",
      codes = list(list(system = "SNOMED-CT", code = "185349003", display = "Encounter")),
      direct_transition = "Next"
    ),
    transition = parse_transition(list(direct_transition = "Next"))
  )
  p <- make_person_clinical()
  result <- process_state(state, p, as.POSIXct("2020-06-01"))
  encs <- result$person@health_record@encounters
  expect_equal(length(encs), 1L)
  expect_equal(encs[[1]]@encounter_class, "ambulatory")
  expect_false(is.null(result$person@attributes[["__current_encounter__"]]))
})
```

- [ ] **Step 2: Run to confirm failures**

```bash
Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-state-clinical.R")'
```

- [ ] **Step 3: Implement `R/state_clinical.R`**

```r
# R/state_clinical.R

.parse_codes <- function(raw_codes) {
  lapply(raw_codes %||% list(), function(c) {
    Code(system = c[["system"]] %||% "", code = c[["code"]] %||% "", display = c[["display"]] %||% "")
  })
}

.new_id <- function() {
  paste0(format(Sys.time(), "%Y%m%d%H%M%S"), "-", sample.int(99999, 1))
}

.state_encounter <- function(state, person, time) {
  def <- state@definition
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
    person@health_record@encounters <- lapply(person@health_record@encounters, function(e) {
      if (e@id == enc_id) { e@end_time <- time; e } else e
    })
    person@attributes[["__current_encounter__"]] <- NULL
  }
  .next(state, person, time)
}

.state_condition_onset <- function(state, person, time) {
  def    <- state@definition
  cond_id <- .new_id()
  cond <- Condition(
    id    = cond_id,
    time  = time,
    codes = .parse_codes(def[["codes"]])
  )
  person@health_record@conditions <- c(person@health_record@conditions, list(cond))
  # Store reference so ConditionEnd can find it
  person@attributes[[paste0("__condition_ref__", state@name)]] <- cond_id
  .next(state, person, time)
}

.state_condition_end <- function(state, person, time) {
  def         <- state@definition
  onset_name  <- def[["condition_onset"]] %||% ""
  cond_id     <- person@attributes[[paste0("__condition_ref__", onset_name)]]
  person@health_record@conditions <- lapply(person@health_record@conditions, function(c) {
    if (!is.null(cond_id) && c@id == cond_id) {
      c@is_active <- FALSE
      c@end_time  <- time
      c
    } else c
  })
  .next(state, person, time)
}

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
  def      <- state@definition
  med_name <- def[["medication_order"]] %||% ""
  med_id   <- person@attributes[[paste0("__medication_ref__", med_name)]]
  person@health_record@medications <- lapply(person@health_record@medications, function(m) {
    if (!is.null(med_id) && m@id == med_id) {
      m@is_active <- FALSE; m@end_time <- time; m
    } else m
  })
  .next(state, person, time)
}

.state_careplan_start <- function(state, person, time) {
  def <- state@definition
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
  def     <- state@definition
  cp_name <- def[["careplan"]] %||% ""
  cp_id   <- person@attributes[[paste0("__careplan_ref__", cp_name)]]
  person@health_record@careplans <- lapply(person@health_record@careplans, function(cp) {
    if (!is.null(cp_id) && cp@id == cp_id) {
      cp@is_active <- FALSE; cp@end_time <- time; cp
    } else cp
  })
  .next(state, person, time)
}

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
  person@health_record@allergies <- lapply(person@health_record@allergies, function(a) {
    if (a@is_active) { a@is_active <- FALSE; a@end_time <- time; a } else a
  })
  .next(state, person, time)
}

.state_procedure <- function(state, person, time) {
  def <- state@definition
  proc <- Procedure(
    id    = .new_id(),
    time  = time,
    codes = .parse_codes(def[["codes"]])
  )
  person@health_record@procedures <- c(person@health_record@procedures, list(proc))
  .next(state, person, time)
}

.state_vaccine <- function(state, person, time) {
  def  <- state@definition
  imm  <- Immunization(id = .new_id(), time = time, codes = .parse_codes(def[["codes"]]))
  person@health_record@immunizations <- c(person@health_record@immunizations, list(imm))
  .next(state, person, time)
}
```

- [ ] **Step 4: Run tests**

```bash
Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-state-clinical.R")'
```

Expected: 4 tests, 0 failures.

- [ ] **Step 5: Commit**

```bash
git add R/state_clinical.R tests/testthat/test-state-clinical.R
git commit -m "feat: implement clinical states (Encounter, ConditionOnset/End, Medication, Procedure, Vaccine, Allergy, CarePlan)"
```

---

## Task 9: State machine — observation and advanced states

**Files:**
- Create: `R/state_observe.R`
- No new tests file needed — add to `tests/testthat/test-state-clinical.R`

- [ ] **Step 1: Add tests for observation and advanced states**

Append to `tests/testthat/test-state-clinical.R`:

```r
test_that("Observation adds observation with value and unit", {
  state <- GMFState(
    name = "RecordA1c", type = "Observation",
    definition = list(
      type = "Observation",
      category = "laboratory",
      unit = "%",
      codes = list(list(system = "LOINC", code = "4548-4", display = "HbA1c")),
      range = list(low = 6.5, high = 8.0),
      direct_transition = "Next"
    ),
    transition = parse_transition(list(direct_transition = "Next"))
  )
  p <- make_person_clinical()
  result <- process_state(state, p, as.POSIXct("2020-01-15"))
  obs <- result$person@health_record@observations
  expect_equal(length(obs), 1L)
  expect_gte(obs[[1]]@value, 6.5)
  expect_lte(obs[[1]]@value, 8.0)
})

test_that("VitalSign updates vital_signs on person", {
  state <- GMFState(
    name = "RecordBP", type = "VitalSign",
    definition = list(
      type = "VitalSign",
      vital_sign = "Systolic Blood Pressure",
      unit = "mmHg",
      exact = list(quantity = 120),
      direct_transition = "Next"
    ),
    transition = parse_transition(list(direct_transition = "Next"))
  )
  p <- make_person_clinical()
  result <- process_state(state, p, Sys.time())
  expect_equal(result$person@vital_signs[["Systolic Blood Pressure"]][["value"]], 120)
})

test_that("Symptom updates symptom map on person", {
  state <- GMFState(
    name = "SetPain", type = "Symptom",
    definition = list(
      type = "Symptom",
      symptom = "Pain",
      cause = "Cold",
      exact = list(quantity = 30),
      direct_transition = "Next"
    ),
    transition = parse_transition(list(direct_transition = "Next"))
  )
  p <- make_person_clinical()
  result <- process_state(state, p, Sys.time())
  expect_equal(result$person@symptoms[["Pain"]][["value"]], 30)
})
```

- [ ] **Step 2: Run to confirm new tests fail**

```bash
Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-state-clinical.R")'
```

- [ ] **Step 3: Implement `R/state_observe.R`**

```r
# R/state_observe.R

.state_observation <- function(state, person, time) {
  def <- state@definition
  value <- .resolve_obs_value(def, person)
  obs <- Observation(
    id       = .new_id(),
    time     = time,
    codes    = .parse_codes(def[["codes"]]),
    value    = value,
    unit     = def[["unit"]] %||% NULL,
    category = def[["category"]] %||% NULL
  )
  person@health_record@observations <- c(person@health_record@observations, list(obs))
  .next(state, person, time)
}

.resolve_obs_value <- function(def, person) {
  if (!is.null(def[["exact"]])) return(as.numeric(def[["exact"]][["quantity"]]))
  if (!is.null(def[["range"]])) {
    return(runif(1, as.numeric(def[["range"]][["low"]]),
                    as.numeric(def[["range"]][["high"]])))
  }
  if (!is.null(def[["attribute"]])) return(person@attributes[[def[["attribute"]]]])
  if (!is.null(def[["value"]])) return(def[["value"]])
  NA
}

.state_multi_observation <- function(state, person, time) {
  def       <- state@definition
  obs_list  <- def[["observations"]] %||% list()
  for (obs_def in obs_list) {
    value <- .resolve_obs_value(obs_def, person)
    obs <- Observation(
      id       = .new_id(),
      time     = time,
      codes    = .parse_codes(obs_def[["codes"]]),
      value    = value,
      unit     = obs_def[["unit"]] %||% NULL,
      category = def[["category"]] %||% NULL
    )
    person@health_record@observations <- c(person@health_record@observations, list(obs))
  }
  .next(state, person, time)
}

.state_diagnostic_report <- function(state, person, time) {
  def      <- state@definition
  obs_list <- def[["observations"]] %||% list()
  obs_entries <- lapply(obs_list, function(o) {
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
  value    <- .resolve_obs_value(def, person)
  value    <- max(0, min(100, as.numeric(value %||% 0)))
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
  dev <- Device(id = dev_id, time = time, codes = .parse_codes(def[["codes"]]))
  person@health_record@devices <- c(person@health_record@devices, list(dev))
  person@attributes[[paste0("__device_ref__", state@name)]] <- dev_id
  .next(state, person, time)
}

.state_device_end <- function(state, person, time) {
  dev_name <- state@definition[["device"]] %||% ""
  dev_id   <- person@attributes[[paste0("__device_ref__", dev_name)]]
  person@health_record@devices <- lapply(person@health_record@devices, function(d) {
    if (!is.null(dev_id) && d@id == dev_id) { d@is_active <- FALSE; d@end_time <- time; d }
    else d
  })
  .next(state, person, time)
}

.state_supply_list <- function(state, person, time) {
  # Supplies tracked on current encounter; minimal implementation
  .next(state, person, time)
}

.state_call_submodule <- function(state, person, time) {
  # Submodule execution handled by the simulation loop via module lookup
  # Here we just store the call intent and transition
  sub_name <- state@definition[["submodule"]] %||% ""
  person@attributes[[paste0("__call_submodule__", state@name)]] <- sub_name
  .next(state, person, time)
}
```

- [ ] **Step 4: Run all clinical tests**

```bash
Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-state-clinical.R")'
```

Expected: 7 tests, 0 failures.

- [ ] **Step 5: Commit**

```bash
git add R/state_observe.R tests/testthat/test-state-clinical.R
git commit -m "feat: implement observation states (Observation, VitalSign, Symptom, DiagnosticReport, ImagingStudy, Device)"
```

---

## Task 10: Demographics sampling

**Files:**
- Create: `R/demographics.R`
- No test file (pure sampling — tested via integration)

- [ ] **Step 1: Implement `R/demographics.R`**

```r
# R/demographics.R

# Samples demographics for a Person and returns the modified Person.
# Uses US national distributions by default; no geographic data in v1.
sample_demographics <- function(person, state = NULL, city = NULL,
                                gender = NULL, min_age = 0, max_age = 140,
                                end_date = Sys.time()) {
  # Gender
  person@attributes[["gender"]] <- gender %||%
    sample(c("M", "F"), 1, prob = c(0.49, 0.51))

  # Race
  person@attributes[["race"]] <- sample(
    c("white", "black", "asian", "native", "other"),
    1, prob = c(0.723, 0.127, 0.06, 0.02, 0.07)
  )

  # Ethnicity (correlated with race)
  p_hisp <- if (person@attributes[["race"]] %in% c("other", "native")) 0.4 else 0.15
  person@attributes[["ethnicity"]] <- sample(
    c("hispanic", "non_hispanic"), 1, prob = c(p_hisp, 1 - p_hisp)
  )

  # Socioeconomic status
  person@attributes[["socioeconomic_status"]] <- sample(
    c("low", "middle", "high"), 1, prob = c(0.3, 0.5, 0.2)
  )

  # Age → birth date
  age <- if (min_age == max_age) min_age
         else sample(seq(min_age, max_age), 1,
                     prob = .age_weights(min_age, max_age))
  person@attributes[["birth_date"]] <- end_date - age * 365.25 * 86400

  # Name (simple placeholders; real name lists bundled in Task 15)
  person@attributes[["first_name"]] <- .sample_name(person@attributes[["gender"]])
  person@attributes[["last_name"]]  <- .sample_surname()

  # Location
  person@attributes[["state"]] <- state %||% "Massachusetts"
  person@attributes[["city"]]  <- city  %||% "Boston"

  person
}

.age_weights <- function(min_age, max_age) {
  ages <- seq(min_age, max_age)
  # Approximate US age distribution: decreasing after 40
  w <- ifelse(ages <= 40, 1.0, exp(-0.02 * (ages - 40)))
  w / sum(w)
}

.sample_name <- function(gender) {
  male_names   <- c("James", "John", "Robert", "Michael", "William",
                    "David", "Joseph", "Charles", "Thomas", "Daniel")
  female_names <- c("Mary", "Patricia", "Jennifer", "Linda", "Barbara",
                    "Elizabeth", "Susan", "Jessica", "Sarah", "Karen")
  sample(if (gender == "M") male_names else female_names, 1)
}

.sample_surname <- function() {
  surnames <- c("Smith", "Johnson", "Williams", "Brown", "Jones",
                "Garcia", "Miller", "Davis", "Wilson", "Martinez")
  sample(surnames, 1)
}
```

- [ ] **Step 2: Verify it runs without error**

```bash
Rscript -e '
devtools::load_all()
p <- Person(seed = 42L)
p <- sample_demographics(p)
cat("gender:", p@attributes$gender, "\n")
cat("race:",   p@attributes$race,   "\n")
cat("age:",    round(age_at(p, Sys.time())), "\n")
'
```

Expected: prints gender, race, age values without errors.

- [ ] **Step 3: Commit**

```bash
git add R/demographics.R
git commit -m "feat: add sample_demographics() with US national distributions"
```

---

## Task 11: Simulation loop

**Files:**
- Create: `R/simulation.R`
- Create: `tests/testthat/test-simulation.R`

- [ ] **Step 1: Write failing tests**

```r
# tests/testthat/test-simulation.R
library(testthat)

test_that("advance_module on test_cold eventually reaches Terminal", {
  set.seed(42)
  m <- load_module(system.file("extdata/modules/test_cold.json", package = "rsynthea"))
  p <- Person(seed = 42L)
  p@attributes[["birth_date"]] <- as.POSIXct("1980-01-01")
  p@attributes[["gender"]] <- "M"
  modules <- list(test_cold = m)

  end_date <- as.POSIXct("1980-12-31")
  p_result <- simulate_life(p, modules, end_date)
  # Person should still be alive (cold doesn't kill)
  expect_true(p_result@is_alive)
})

test_that("simulate_life respects end_date", {
  modules <- list()
  p <- Person(seed = 1L)
  p@attributes[["birth_date"]] <- as.POSIXct("2000-01-01")
  end_date <- as.POSIXct("2001-01-01")
  p_result <- simulate_life(p, modules, end_date)
  # No modules = no events, person still alive
  expect_true(p_result@is_alive)
})

test_that("simulate_life stops when person dies", {
  death_module_json <- '{
    "name": "instant_death",
    "states": {
      "Initial": {"type": "Initial", "direct_transition": "Die"},
      "Die": {"type": "Death"}
    }
  }'
  tmp <- tempfile(fileext = ".json")
  writeLines(death_module_json, tmp)
  m <- load_module(tmp)
  unlink(tmp)

  p <- Person(seed = 5L)
  p@attributes[["birth_date"]] <- as.POSIXct("1990-01-01")
  end_date <- as.POSIXct("2050-01-01")
  p_result <- simulate_life(p, list(instant_death = m), end_date)
  expect_false(p_result@is_alive)
})
```

- [ ] **Step 2: Run to confirm failures**

```bash
Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-simulation.R")'
```

- [ ] **Step 3: Implement `R/simulation.R`**

```r
# R/simulation.R

TIMESTEP_DAYS <- 7L  # 1 week

simulate_life <- function(person, modules, end_date) {
  current_time <- person@attributes[["birth_date"]] %||% end_date
  timestep_secs <- TIMESTEP_DAYS * 86400

  while (current_time <= end_date && person@is_alive) {
    for (module in modules) {
      person <- advance_module(person, module, current_time, modules)
    }
    current_time <- current_time + timestep_secs
  }
  person
}

advance_module <- function(person, module, time, all_modules = list()) {
  state_key <- paste0("__module_state__", module@name)
  current_name <- person@attributes[[state_key]] %||% "Initial"

  # If Terminal, skip this module this timestep
  if (identical(current_name, "__terminal__")) return(person)

  max_iter <- 500L  # guard against infinite loops
  iter <- 0L

  while (iter < max_iter) {
    iter <- iter + 1L
    state <- module@states[[current_name]]
    if (is.null(state)) break

    result <- process_state(state, person, time)
    person <- result$person
    next_name <- result$next_state

    # Terminal state
    if (is.null(next_name)) {
      person@attributes[[state_key]] <- "__terminal__"
      break
    }

    # CallSubmodule: run submodule inline
    call_key <- paste0("__call_submodule__", state@name)
    sub_name <- person@attributes[[call_key]]
    if (!is.null(sub_name) && !is.null(all_modules[[sub_name]])) {
      person@attributes[[call_key]] <- NULL
      person <- advance_module(person, all_modules[[sub_name]], time, all_modules)
    }

    # Stay (Delay / Guard returns same state name)
    if (next_name == current_name) break

    current_name <- next_name
    person@attributes[[state_key]] <- current_name

    # If new state is Terminal, mark and stop
    next_state <- module@states[[current_name]]
    if (!is.null(next_state) && next_state@type == "Terminal") {
      process_state(next_state, person, time)  # mark visited
      person@attributes[[state_key]] <- "__terminal__"
      break
    }
  }

  person
}
```

- [ ] **Step 4: Run tests**

```bash
Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-simulation.R")'
```

Expected: 3 tests, 0 failures.

- [ ] **Step 5: Commit**

```bash
git add R/simulation.R tests/testthat/test-simulation.R
git commit -m "feat: implement simulate_life() and advance_module() simulation loop"
```

---

## Task 12: Generator

**Files:**
- Create: `R/generator.R`
- Create: `tests/testthat/test-generator.R`

- [ ] **Step 1: Write failing tests**

```r
# tests/testthat/test-generator.R
library(testthat)

test_that("generate_population returns a list of Person objects", {
  set.seed(42)
  result <- generate_population(
    n        = 3,
    seed     = 42L,
    modules  = list(),
    end_date = as.POSIXct("2000-12-31")
  )
  expect_equal(length(result), 3L)
  expect_true(all(vapply(result, function(p) inherits(p, "rsynthea_Person"), logical(1))))
})

test_that("generate_population respects gender filter", {
  result <- generate_population(
    n = 20, seed = 1L, gender = "F",
    modules = list(), end_date = as.POSIXct("2000-12-31")
  )
  genders <- vapply(result, function(p) p@attributes[["gender"]], character(1))
  expect_true(all(genders == "F"))
})

test_that("generate_population seed is reproducible", {
  r1 <- generate_population(n = 2, seed = 99L, modules = list(),
                             end_date = as.POSIXct("2000-12-31"))
  r2 <- generate_population(n = 2, seed = 99L, modules = list(),
                             end_date = as.POSIXct("2000-12-31"))
  expect_equal(r1[[1]]@attributes[["gender"]], r2[[1]]@attributes[["gender"]])
})
```

- [ ] **Step 2: Run to confirm failures**

```bash
Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-generator.R")'
```

- [ ] **Step 3: Implement `R/generator.R`**

```r
# R/generator.R

generate_population <- function(
  n        = 1L,
  seed     = NULL,
  state    = NULL,
  city     = NULL,
  gender   = NULL,
  min_age  = 0L,
  max_age  = 140L,
  modules  = NULL,
  end_date = Sys.time()
) {
  if (is.null(modules)) {
    modules <- load_all_modules()
  }

  patients <- vector("list", n)
  for (i in seq_len(n)) {
    person_seed <- if (!is.null(seed)) seed + i - 1L else sample.int(.Machine$integer.max, 1L)
    set.seed(person_seed)
    p <- Person(seed = as.integer(person_seed))
    p <- sample_demographics(p, state = state, city = city, gender = gender,
                             min_age = min_age, max_age = max_age, end_date = end_date)
    p <- simulate_life(p, modules, end_date)
    patients[[i]] <- p
  }
  patients
}
```

- [ ] **Step 4: Run tests**

```bash
Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-generator.R")'
```

Expected: 3 tests, 0 failures.

- [ ] **Step 5: Commit**

```bash
git add R/generator.R tests/testthat/test-generator.R
git commit -m "feat: implement generate_population() orchestrator"
```

---

## Task 13: Export to tibbles

**Files:**
- Create: `R/export.R`
- Create: `tests/testthat/test-export.R`

- [ ] **Step 1: Write failing tests**

```r
# tests/testthat/test-export.R
library(testthat)
library(tibble)

make_test_patient <- function(seed = 1L) {
  p <- Person(seed = as.integer(seed))
  p@attributes[["gender"]]     <- "M"
  p@attributes[["race"]]       <- "white"
  p@attributes[["birth_date"]] <- as.POSIXct("1980-01-01")
  p@attributes[["first_name"]] <- "John"
  p@attributes[["last_name"]]  <- "Doe"
  p@attributes[["state"]]      <- "Massachusetts"
  p@attributes[["city"]]       <- "Boston"

  # Add an encounter
  enc <- Encounter(
    id = "enc-001", time = as.POSIXct("2020-01-01"),
    codes = list(Code("SNOMED-CT", "185349003", "Wellness")),
    encounter_class = "ambulatory"
  )
  # Add a condition
  cond <- Condition(
    id = "cond-001", time = as.POSIXct("2019-01-01"),
    codes = list(Code("SNOMED-CT", "44054006", "T2DM"))
  )
  p@health_record@encounters  <- list(enc)
  p@health_record@conditions  <- list(cond)
  p
}

test_that("export_population returns named list with expected tibbles", {
  patients <- list(make_test_patient(1L), make_test_patient(2L))
  result <- export_population(patients)
  expect_named(result, c("patients", "encounters", "conditions", "medications",
                          "procedures", "observations", "immunizations", "allergies",
                          "careplans"))
  expect_true(is_tibble(result$patients))
  expect_true(is_tibble(result$encounters))
})

test_that("patients tibble has one row per patient", {
  patients <- list(make_test_patient(1L), make_test_patient(2L))
  result <- export_population(patients)
  expect_equal(nrow(result$patients), 2L)
})

test_that("encounters tibble contains patient_id column", {
  patients <- list(make_test_patient(1L))
  result <- export_population(patients)
  expect_true("patient_id" %in% names(result$encounters))
  expect_equal(nrow(result$encounters), 1L)
})

test_that("conditions tibble contains is_active column", {
  patients <- list(make_test_patient(1L))
  result <- export_population(patients)
  expect_true("is_active" %in% names(result$conditions))
})
```

- [ ] **Step 2: Run to confirm failures**

```bash
Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-export.R")'
```

- [ ] **Step 3: Implement `R/export.R`**

```r
# R/export.R

export_population <- function(patients, output_dir = NULL) {
  tbls <- list(
    patients      = .patients_tibble(patients),
    encounters    = .encounters_tibble(patients),
    conditions    = .conditions_tibble(patients),
    medications   = .medications_tibble(patients),
    procedures    = .procedures_tibble(patients),
    observations  = .observations_tibble(patients),
    immunizations = .immunizations_tibble(patients),
    allergies     = .allergies_tibble(patients),
    careplans     = .careplans_tibble(patients)
  )
  if (!is.null(output_dir)) {
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
    for (nm in names(tbls)) {
      utils::write.csv(tbls[[nm]],
                       file.path(output_dir, paste0(nm, ".csv")),
                       row.names = FALSE)
    }
  }
  tbls
}

.patients_tibble <- function(patients) {
  rows <- lapply(patients, function(p) {
    a <- p@attributes
    tibble::tibble(
      id         = p@id,
      birth_date = a[["birth_date"]],
      death_date = a[["death_date"]] %||% NA,
      is_alive   = p@is_alive,
      gender     = a[["gender"]] %||% NA_character_,
      race       = a[["race"]] %||% NA_character_,
      ethnicity  = a[["ethnicity"]] %||% NA_character_,
      first_name = a[["first_name"]] %||% NA_character_,
      last_name  = a[["last_name"]] %||% NA_character_,
      state      = a[["state"]] %||% NA_character_,
      city       = a[["city"]] %||% NA_character_
    )
  })
  dplyr::bind_rows(rows)
}

.encounters_tibble <- function(patients) {
  rows <- lapply(patients, function(p) {
    lapply(p@health_record@encounters, function(e) {
      tibble::tibble(
        id              = e@id,
        patient_id      = p@id,
        time            = e@time,
        end_time        = e@end_time %||% NA,
        encounter_class = e@encounter_class,
        code            = if (length(e@codes) > 0) e@codes[[1]]@code else NA_character_,
        code_system     = if (length(e@codes) > 0) e@codes[[1]]@system else NA_character_,
        description     = if (length(e@codes) > 0) e@codes[[1]]@display else NA_character_
      )
    })
  })
  dplyr::bind_rows(unlist(rows, recursive = FALSE))
}

.conditions_tibble <- function(patients) {
  rows <- lapply(patients, function(p) {
    lapply(p@health_record@conditions, function(c) {
      tibble::tibble(
        id          = c@id,
        patient_id  = p@id,
        onset_time  = c@time,
        end_time    = c@end_time %||% NA,
        is_active   = c@is_active,
        code        = if (length(c@codes) > 0) c@codes[[1]]@code else NA_character_,
        code_system = if (length(c@codes) > 0) c@codes[[1]]@system else NA_character_,
        description = if (length(c@codes) > 0) c@codes[[1]]@display else NA_character_
      )
    })
  })
  dplyr::bind_rows(unlist(rows, recursive = FALSE))
}

.medications_tibble <- function(patients) {
  rows <- lapply(patients, function(p) {
    lapply(p@health_record@medications, function(m) {
      tibble::tibble(
        id          = m@id,
        patient_id  = p@id,
        start_time  = m@time,
        end_time    = m@end_time %||% NA,
        is_active   = m@is_active,
        code        = if (length(m@codes) > 0) m@codes[[1]]@code else NA_character_,
        code_system = if (length(m@codes) > 0) m@codes[[1]]@system else NA_character_,
        description = if (length(m@codes) > 0) m@codes[[1]]@display else NA_character_
      )
    })
  })
  dplyr::bind_rows(unlist(rows, recursive = FALSE))
}

.procedures_tibble <- function(patients) {
  rows <- lapply(patients, function(p) {
    lapply(p@health_record@procedures, function(pr) {
      tibble::tibble(
        id          = pr@id,
        patient_id  = p@id,
        time        = pr@time,
        code        = if (length(pr@codes) > 0) pr@codes[[1]]@code else NA_character_,
        code_system = if (length(pr@codes) > 0) pr@codes[[1]]@system else NA_character_,
        description = if (length(pr@codes) > 0) pr@codes[[1]]@display else NA_character_
      )
    })
  })
  dplyr::bind_rows(unlist(rows, recursive = FALSE))
}

.observations_tibble <- function(patients) {
  rows <- lapply(patients, function(p) {
    lapply(p@health_record@observations, function(o) {
      tibble::tibble(
        id          = o@id,
        patient_id  = p@id,
        time        = o@time,
        value       = as.character(o@value %||% NA),
        unit        = o@unit %||% NA_character_,
        category    = o@category %||% NA_character_,
        code        = if (length(o@codes) > 0) o@codes[[1]]@code else NA_character_,
        code_system = if (length(o@codes) > 0) o@codes[[1]]@system else NA_character_,
        description = if (length(o@codes) > 0) o@codes[[1]]@display else NA_character_
      )
    })
  })
  dplyr::bind_rows(unlist(rows, recursive = FALSE))
}

.immunizations_tibble <- function(patients) {
  rows <- lapply(patients, function(p) {
    lapply(p@health_record@immunizations, function(i) {
      tibble::tibble(
        id          = i@id,
        patient_id  = p@id,
        time        = i@time,
        code        = if (length(i@codes) > 0) i@codes[[1]]@code else NA_character_,
        description = if (length(i@codes) > 0) i@codes[[1]]@display else NA_character_
      )
    })
  })
  dplyr::bind_rows(unlist(rows, recursive = FALSE))
}

.allergies_tibble <- function(patients) {
  rows <- lapply(patients, function(p) {
    lapply(p@health_record@allergies, function(a) {
      tibble::tibble(
        id          = a@id,
        patient_id  = p@id,
        onset_time  = a@time,
        end_time    = a@end_time %||% NA,
        is_active   = a@is_active,
        code        = if (length(a@codes) > 0) a@codes[[1]]@code else NA_character_,
        description = if (length(a@codes) > 0) a@codes[[1]]@display else NA_character_
      )
    })
  })
  dplyr::bind_rows(unlist(rows, recursive = FALSE))
}

.careplans_tibble <- function(patients) {
  rows <- lapply(patients, function(p) {
    lapply(p@health_record@careplans, function(cp) {
      tibble::tibble(
        id          = cp@id,
        patient_id  = p@id,
        start_time  = cp@time,
        end_time    = cp@end_time %||% NA,
        is_active   = cp@is_active,
        code        = if (length(cp@codes) > 0) cp@codes[[1]]@code else NA_character_,
        description = if (length(cp@codes) > 0) cp@codes[[1]]@display else NA_character_
      )
    })
  })
  dplyr::bind_rows(unlist(rows, recursive = FALSE))
}
```

- [ ] **Step 4: Run tests**

```bash
Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-export.R")'
```

Expected: 4 tests, 0 failures.

- [ ] **Step 5: Commit**

```bash
git add R/export.R tests/testthat/test-export.R
git commit -m "feat: implement export_population() returning named list of tibbles"
```

---

## Task 14: Bundle GMF modules and resources

**Files:**
- Populate: `inst/extdata/modules/` (231 JSON files from py-synthea)
- Populate: `inst/extdata/resources/` (biometrics, names, immunization schedule)

- [ ] **Step 1: Clone py-synthea and copy resources**

```bash
cd /tmp
git clone --depth 1 https://github.com/TIET-AI/tietai-synthea tietai-synthea-tmp

cp -r /tmp/tietai-synthea-tmp/resources/modules/. \
  /Users/jlcanadas/proyectos/proyectos_personales/r-synthea/inst/extdata/modules/

mkdir -p /Users/jlcanadas/proyectos/proyectos_personales/r-synthea/inst/extdata/resources
cp /tmp/tietai-synthea-tmp/resources/biometrics.yml \
   /tmp/tietai-synthea-tmp/resources/names.yml \
   /tmp/tietai-synthea-tmp/resources/immunization_schedule.json \
   /Users/jlcanadas/proyectos/proyectos_personales/r-synthea/inst/extdata/resources/

rm -rf /tmp/tietai-synthea-tmp
```

- [ ] **Step 2: Verify module count**

```bash
find /Users/jlcanadas/proyectos/proyectos_personales/r-synthea/inst/extdata/modules \
  -name "*.json" | wc -l
```

Expected: ≥ 100 JSON files.

- [ ] **Step 3: Smoke test loading all modules**

```bash
Rscript -e '
devtools::load_all()
mods <- load_all_modules()
cat("Loaded", length(mods), "modules\n")
stopifnot(length(mods) > 50)
'
```

Expected: `Loaded N modules` with N > 50.

- [ ] **Step 4: Commit**

```bash
cd /Users/jlcanadas/proyectos/proyectos_personales/r-synthea
git add inst/extdata/
git commit -m "feat: bundle 231 GMF disease modules and resource files"
```

---

## Task 15: Integration test + full run

**Files:**
- Create: `tests/testthat/test-integration.R`

- [ ] **Step 1: Write integration test**

```r
# tests/testthat/test-integration.R
library(testthat)

test_that("generate_population with real modules produces valid tibbles", {
  skip_if_not(
    length(list.files(system.file("extdata/modules", package = "rsynthea"),
                      pattern = "\\.json$", recursive = TRUE)) > 10,
    "Full module set not installed"
  )

  set.seed(42)
  patients <- generate_population(
    n        = 5L,
    seed     = 42L,
    end_date = as.POSIXct("2020-12-31")
  )
  result <- export_population(patients)

  expect_equal(nrow(result$patients), 5L)
  expect_true(all(c("id", "gender", "birth_date", "is_alive") %in%
                    names(result$patients)))

  # At least some patients should have clinical events
  total_events <- nrow(result$encounters) + nrow(result$conditions) +
                  nrow(result$medications) + nrow(result$immunizations)
  expect_gt(total_events, 0L)
})

test_that("generated patient IDs are unique", {
  patients <- generate_population(n = 10L, seed = 1L, modules = list(),
                                  end_date = as.POSIXct("2000-12-31"))
  ids <- vapply(patients, function(p) p@id, character(1))
  expect_equal(length(unique(ids)), 10L)
})

test_that("export_population writes CSV files when output_dir provided", {
  tmp <- tempdir()
  patients <- generate_population(n = 2L, seed = 5L, modules = list(),
                                  end_date = as.POSIXct("2000-12-31"))
  export_population(patients, output_dir = tmp)
  expect_true(file.exists(file.path(tmp, "patients.csv")))
  expect_true(file.exists(file.path(tmp, "encounters.csv")))
})
```

- [ ] **Step 2: Run integration tests**

```bash
Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-integration.R")'
```

Expected: 3 tests, 0 failures (first test skipped if modules not bundled yet).

- [ ] **Step 3: Run full test suite**

```bash
Rscript -e 'devtools::load_all(); testthat::test_dir("tests/testthat")'
```

Expected: All tests pass, 0 failures.

- [ ] **Step 4: Manual smoke run**

```bash
Rscript -e '
devtools::load_all()
cat("Generating 10 patients...\n")
patients <- generate_population(n = 10L, seed = 42L, end_date = as.POSIXct("2020-12-31"))
result   <- export_population(patients)
cat("Patients:     ", nrow(result$patients),     "\n")
cat("Encounters:   ", nrow(result$encounters),   "\n")
cat("Conditions:   ", nrow(result$conditions),   "\n")
cat("Medications:  ", nrow(result$medications),  "\n")
cat("Immunizations:", nrow(result$immunizations),"\n")
'
```

- [ ] **Step 5: Final commit**

```bash
git add tests/testthat/test-integration.R
git commit -m "test: add integration tests for full generate_population + export pipeline"
```

---

## Self-Review

### Spec coverage

| Requirement | Task |
|---|---|
| S7 classes with value semantics | Tasks 2–3 |
| 27 state types | Tasks 7–9 |
| All condition types (15+) | Task 4 |
| All transition types | Task 5 |
| JSON module loading (231 modules) | Tasks 6, 14 |
| Demographics sampling | Task 10 |
| Simulation loop (weekly timestep) | Task 11 |
| Generator orchestrator | Task 12 |
| Export to tibbles/CSV | Task 13 |
| Integration test | Task 15 |
| No FHIR in v1 | ✓ excluded |

### Type consistency check

- `GMFState@transition` is always the result of `parse_transition()` — a list with `$type` and `$target`/`$entries`, or `NULL`. Used consistently in `resolve_transition()`.
- `process_state()` always returns `list(person = Person, next_state = character | NULL)`.
- `advance_module()` takes and returns a `Person`. Consumes `list(person, next_state)` internally.
- `%||%` defined once in `logic.R`, available globally via `devtools::load_all()`.

### Known limitations (v1)

- `CallSubmodule` state stores the call intent but full inline submodule execution relies on `all_modules` being passed through. Works if all modules are loaded into one list.
- LookupTable transitions fall back to `default_probability`; CSV lookup files from py-synthea not bundled.
- Demographics use simplified national distributions; no geographic breakdown by state/city in v1.
