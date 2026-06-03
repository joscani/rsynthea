# tests/testthat/test-state-clinical.R
library(testthat)

make_person_clinical <- function() {
  p <- Person(seed = 1L)
  p@attributes[["birth_date"]] <- as.POSIXct("1980-01-01")
  p@attributes[["gender"]] <- "M"
  .REC$e <- p@.record
  p
}

# --- Encounter ---

test_that("Encounter adds entry to .record$encounters", {
  s <- GMFState(
    name = "AnnualVisit", type = "Encounter",
    definition = list(
      type = "Encounter",
      encounter_class = "ambulatory",
      codes = list(list(system = "SNOMED-CT", code = "185349003", display = "Wellness")),
      direct_transition = "Next"
    ),
    transition = parse_transition(list(direct_transition = "Next"))
  )
  p <- make_person_clinical()
  r <- process_state(s, p, as.POSIXct("2020-06-01"))
  expect_equal(length(r$person@.record$encounters), 1L)
  expect_equal(r$person@.record$encounters[[1]]$encounter_class, "ambulatory")
  expect_false(is.null(r$person@.record[["__current_encounter_env__"]]))
  expect_equal(r$next_state, "Next")
})

test_that("EncounterEnd sets end_time on current encounter", {
  p <- make_person_clinical()
  enc_env <- new.env(parent = emptyenv())
  enc_env$id <- "enc-001"
  enc_env$time <- as.POSIXct("2020-06-01")
  enc_env$end_time <- NULL
  enc_env$codes <- list()
  enc_env$encounter_class <- "ambulatory"
  p@.record$encounters <- list(enc_env)
  p@.record[["__current_encounter_env__"]] <- enc_env

  s <- GMFState(
    name = "EndVisit", type = "EncounterEnd",
    definition = list(type = "EncounterEnd", direct_transition = "Next"),
    transition = parse_transition(list(direct_transition = "Next"))
  )
  r <- process_state(s, p, as.POSIXct("2020-06-01"))
  expect_false(is.null(r$person@.record$encounters[[1]]$end_time))
  expect_null(r$person@.record[["__current_encounter_env__"]])
})

# --- ConditionOnset / ConditionEnd ---

test_that("ConditionOnset adds active condition", {
  s <- GMFState(
    name = "DiabetesOnset", type = "ConditionOnset",
    definition = list(
      type = "ConditionOnset",
      codes = list(list(system = "SNOMED-CT", code = "44054006", display = "T2DM")),
      direct_transition = "Next"
    ),
    transition = parse_transition(list(direct_transition = "Next"))
  )
  r <- process_state(s, make_person_clinical(), as.POSIXct("2020-01-01"))
  conds <- r$person@.record$conditions
  expect_equal(length(conds), 1L)
  expect_true(conds[[1]]$is_active)
  expect_equal(conds[[1]]$codes[[1]][["code"]], "44054006")
})

test_that("ConditionEnd deactivates the matching condition", {
  p <- make_person_clinical()
  cond_env <- new.env(parent = emptyenv())
  cond_env$id <- "c1"
  cond_env$time <- as.POSIXct("2019-01-01")
  cond_env$codes <- list(list(system = "SNOMED-CT", code = "44054006", display = "T2DM"))
  cond_env$is_active <- TRUE
  cond_env$end_time <- NULL
  p@.record$conditions <- list(cond_env)
  p@.record[["__condition_env__DiabetesOnset"]] <- cond_env

  s <- GMFState(
    name = "DiabetesEnd", type = "ConditionEnd",
    definition = list(type = "ConditionEnd", condition_onset = "DiabetesOnset",
                      direct_transition = "Next"),
    transition = parse_transition(list(direct_transition = "Next"))
  )
  r <- process_state(s, p, as.POSIXct("2022-01-01"))
  expect_false(r$person@.record$conditions[[1]]$is_active)
  expect_false(is.null(r$person@.record$conditions[[1]]$end_time))
})

# --- MedicationOrder / MedicationEnd ---

test_that("MedicationOrder adds active medication", {
  s <- GMFState(
    name = "PrescribeMetformin", type = "MedicationOrder",
    definition = list(
      type = "MedicationOrder",
      codes = list(list(system = "RxNorm", code = "860975", display = "Metformin")),
      direct_transition = "Next"
    ),
    transition = parse_transition(list(direct_transition = "Next"))
  )
  r <- process_state(s, make_person_clinical(), as.POSIXct("2020-03-01"))
  meds <- r$person@.record$medications
  expect_equal(length(meds), 1L)
  expect_true(meds[[1]]$is_active)
  expect_equal(meds[[1]]$codes[[1]][["code"]], "860975")
})

test_that("MedicationEnd deactivates the matching medication", {
  p <- make_person_clinical()
  med_env <- new.env(parent = emptyenv())
  med_env$id <- "m1"
  med_env$time <- as.POSIXct("2020-01-01")
  med_env$codes <- list(list(system = "RxNorm", code = "860975", display = "Metformin"))
  med_env$is_active <- TRUE
  med_env$end_time <- NULL
  p@.record$medications <- list(med_env)
  p@.record[["__medication_env__PrescribeMetformin"]] <- med_env

  s <- GMFState(
    name = "StopMetformin", type = "MedicationEnd",
    definition = list(type = "MedicationEnd", medication_order = "PrescribeMetformin",
                      direct_transition = "Next"),
    transition = parse_transition(list(direct_transition = "Next"))
  )
  r <- process_state(s, p, as.POSIXct("2021-01-01"))
  expect_false(r$person@.record$medications[[1]]$is_active)
  expect_false(is.null(r$person@.record$medications[[1]]$end_time))
})

# --- Procedure ---

test_that("Procedure adds a procedure to health record", {
  s <- GMFState(
    name = "AppendectomyProc", type = "Procedure",
    definition = list(
      type = "Procedure",
      codes = list(list(system = "SNOMED-CT", code = "80146002", display = "Appendectomy")),
      direct_transition = "Next"
    ),
    transition = parse_transition(list(direct_transition = "Next"))
  )
  r <- process_state(s, make_person_clinical(), as.POSIXct("2020-05-01"))
  expect_equal(length(r$person@.record$procedures), 1L)
  expect_equal(r$person@.record$procedures[[1]]$codes[[1]][["code"]], "80146002")
})

# --- Vaccine ---

test_that("Vaccine adds an immunization to health record", {
  s <- GMFState(
    name = "FluShot", type = "Vaccine",
    definition = list(
      type = "Vaccine",
      codes = list(list(system = "CVX", code = "141", display = "Influenza vaccine")),
      direct_transition = "Next"
    ),
    transition = parse_transition(list(direct_transition = "Next"))
  )
  r <- process_state(s, make_person_clinical(), as.POSIXct("2020-10-01"))
  expect_equal(length(r$person@.record$immunizations), 1L)
})

# --- AllergyOnset ---

test_that("AllergyOnset adds an allergy to health record", {
  s <- GMFState(
    name = "PenicillinAllergy", type = "AllergyOnset",
    definition = list(
      type = "AllergyOnset",
      codes = list(list(system = "RxNorm", code = "7980", display = "Penicillin")),
      direct_transition = "Next"
    ),
    transition = parse_transition(list(direct_transition = "Next"))
  )
  r <- process_state(s, make_person_clinical(), as.POSIXct("2020-01-01"))
  expect_equal(length(r$person@.record$allergies), 1L)
  expect_true(r$person@.record$allergies[[1]]$is_active)
})

test_that("AllergyEnd deactivates the matching allergy", {
  p <- make_person_clinical()
  alg_env <- new.env(parent = emptyenv())
  alg_env$id <- "a1"
  alg_env$time <- as.POSIXct("2020-01-01")
  alg_env$codes <- list(list(system = "RxNorm", code = "7980", display = "Penicillin"))
  alg_env$is_active <- TRUE
  alg_env$end_time <- NULL
  p@.record$allergies <- list(alg_env)
  p@.record[["__allergy_env__PenicillinAllergy"]] <- alg_env

  s <- GMFState(
    name = "PenicillinAllergyEnd", type = "AllergyEnd",
    definition = list(type = "AllergyEnd", allergy_onset = "PenicillinAllergy",
                      direct_transition = "Next"),
    transition = parse_transition(list(direct_transition = "Next"))
  )
  r <- process_state(s, p, as.POSIXct("2025-01-01"))
  expect_false(r$person@.record$allergies[[1]]$is_active)
  expect_false(is.null(r$person@.record$allergies[[1]]$end_time))
})

# --- CarePlan ---

test_that("CarePlanStart adds active care plan", {
  s <- GMFState(
    name = "DiabetesCare", type = "CarePlanStart",
    definition = list(
      type = "CarePlanStart",
      codes = list(list(system = "SNOMED-CT", code = "408290009", display = "Diabetes care")),
      activities = list(list(system = "SNOMED-CT", code = "310627008", display = "Physical activity")),
      direct_transition = "Next"
    ),
    transition = parse_transition(list(direct_transition = "Next"))
  )
  r <- process_state(s, make_person_clinical(), as.POSIXct("2020-01-01"))
  expect_equal(length(r$person@.record$careplans), 1L)
  expect_true(r$person@.record$careplans[[1]]$is_active)
  expect_equal(length(r$person@.record$careplans[[1]]$activities), 1L)
})

test_that("CarePlanEnd deactivates the matching care plan", {
  p <- make_person_clinical()
  cp_env <- new.env(parent = emptyenv())
  cp_env$id <- "cp1"
  cp_env$time <- as.POSIXct("2020-01-01")
  cp_env$codes <- list(list(system = "SNOMED-CT", code = "408290009", display = "Diabetes care"))
  cp_env$activities <- list()
  cp_env$is_active <- TRUE
  cp_env$end_time <- NULL
  p@.record$careplans <- list(cp_env)
  p@.record[["__careplan_env__DiabetesCare"]] <- cp_env

  s <- GMFState(
    name = "DiabetesCareEnd", type = "CarePlanEnd",
    definition = list(type = "CarePlanEnd", careplan = "DiabetesCare", direct_transition = "Next"),
    transition = parse_transition(list(direct_transition = "Next"))
  )
  r <- process_state(s, p, as.POSIXct("2022-01-01"))
  expect_false(r$person@.record$careplans[[1]]$is_active)
  expect_false(is.null(r$person@.record$careplans[[1]]$end_time))
})

# --- Observation ---

test_that("Observation adds observation with numeric value from range", {
  s <- GMFState(
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
  r <- process_state(s, p, as.POSIXct("2020-01-15"))
  obs <- r$person@.record$observations
  expect_equal(length(obs), 1L)
  expect_gte(as.numeric(obs[[1]]$value), 6.5)
  expect_lte(as.numeric(obs[[1]]$value), 8.0)
  expect_equal(obs[[1]]$unit, "%")
})

test_that("Observation with exact value", {
  s <- GMFState(
    name = "RecordWeight", type = "Observation",
    definition = list(
      type = "Observation",
      unit = "kg",
      codes = list(list(system = "LOINC", code = "29463-7", display = "Body weight")),
      exact = list(quantity = 75),
      direct_transition = "Next"
    ),
    transition = parse_transition(list(direct_transition = "Next"))
  )
  r <- process_state(s, make_person_clinical(), Sys.time())
  expect_equal(as.numeric(r$person@.record$observations[[1]]$value), 75)
})

test_that("Observation with textual exact quantity keeps text value", {
  s <- GMFState(
    name = "RecordNarrative", type = "Observation",
    definition = list(
      type = "Observation",
      unit = "{#}",
      codes = list(list(system = "LOINC", code = "29554-3", display = "Procedure Narrative")),
      exact = list(quantity = "CABG Grafts: 1"),
      direct_transition = "Next"
    ),
    transition = parse_transition(list(direct_transition = "Next"))
  )
  r <- process_state(s, make_person_clinical(), Sys.time())
  expect_equal(r$person@.record$observations[[1]]$value, "CABG Grafts: 1")
})

test_that("Observation updates latest and by-code indices", {
  s1 <- GMFState(
    name = "HemoglobinLow", type = "Observation",
    definition = list(
      type = "Observation",
      unit = "g/dL",
      codes = list(list(system = "LOINC", code = "718-7", display = "Hemoglobin")),
      exact = list(quantity = 10),
      direct_transition = "Next"
    ),
    transition = parse_transition(list(direct_transition = "Next"))
  )
  s2 <- GMFState(
    name = "HemoglobinNormal", type = "Observation",
    definition = list(
      type = "Observation",
      unit = "g/dL",
      codes = list(list(system = "LOINC", code = "718-7", display = "Hemoglobin")),
      exact = list(quantity = 12),
      direct_transition = "Next"
    ),
    transition = parse_transition(list(direct_transition = "Next"))
  )
  p <- make_person_clinical()
  p <- process_state(s1, p, as.POSIXct("2020-01-01"))$person
  p <- process_state(s2, p, as.POSIXct("2020-02-01"))$person

  rec <- p@.record
  expect_equal(rec$.latest_observations[["718-7"]]$value, 12)
  expect_equal(length(rec$.observations_by_code[["718-7"]]), 2L)
  expect_equal(rec$.observations_by_code[["718-7"]][[1L]]$value, 10)
})

test_that("Observation condition uses latest indexed observation", {
  s1 <- GMFState(
    name = "HemoglobinLow", type = "Observation",
    definition = list(
      type = "Observation",
      codes = list(list(system = "LOINC", code = "718-7", display = "Hemoglobin")),
      exact = list(quantity = 10),
      direct_transition = "Next"
    ),
    transition = parse_transition(list(direct_transition = "Next"))
  )
  s2 <- GMFState(
    name = "HemoglobinHigh", type = "Observation",
    definition = list(
      type = "Observation",
      codes = list(list(system = "LOINC", code = "718-7", display = "Hemoglobin")),
      exact = list(quantity = 13),
      direct_transition = "Next"
    ),
    transition = parse_transition(list(direct_transition = "Next"))
  )
  p <- make_person_clinical()
  p <- process_state(s1, p, as.POSIXct("2020-01-01"))$person
  p <- process_state(s2, p, as.POSIXct("2020-02-01"))$person

  expect_true(evaluate_condition(
    list(condition_type = "Observation",
         codes = list(list(system = "LOINC", code = "718-7", display = "Hemoglobin")),
         operator = ">=", value = 12),
    p, Sys.time()
  ))
})

test_that("MultiObservation updates by-code index for each sub-observation", {
  s <- GMFState(
    name = "BloodPressure", type = "MultiObservation",
    definition = list(
      type = "MultiObservation",
      observations = list(
        list(unit = "mmHg",
             codes = list(list(system = "LOINC", code = "8480-6", display = "Systolic")),
             exact = list(quantity = 120)),
        list(unit = "mmHg",
             codes = list(list(system = "LOINC", code = "8462-4", display = "Diastolic")),
             exact = list(quantity = 80))
      ),
      direct_transition = "Next"
    ),
    transition = parse_transition(list(direct_transition = "Next"))
  )
  r <- process_state(s, make_person_clinical(), Sys.time())
  rec <- r$person@.record

  expect_equal(rec$.latest_observations[["8480-6"]]$value, 120)
  expect_equal(rec$.latest_observations[["8462-4"]]$value, 80)
  expect_equal(length(rec$.observations_by_code[["8480-6"]]), 1L)
})

# --- VitalSign ---

test_that("VitalSign updates vital_signs on person", {
  s <- GMFState(
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
  r <- process_state(s, make_person_clinical(), Sys.time())
  expect_equal(r$person@vital_signs[["Systolic Blood Pressure"]][["value"]], 120)
  expect_equal(r$person@vital_signs[["Systolic Blood Pressure"]][["unit"]], "mmHg")
})

# --- Symptom ---

test_that("Symptom updates symptom map on person (clamped to 0-100)", {
  s <- GMFState(
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
  r <- process_state(s, make_person_clinical(), Sys.time())
  expect_equal(r$person@symptoms[["Pain"]][["value"]], 30)
  expect_equal(r$person@symptoms[["Pain"]][["cause"]], "Cold")
})

# --- ImagingStudy ---

test_that("ImagingStudy adds to .record$imaging", {
  s <- GMFState(
    name = "ChestXray", type = "ImagingStudy",
    definition = list(
      type = "ImagingStudy",
      codes = list(list(system = "SNOMED-CT", code = "399208008", display = "Chest X-ray")),
      series = list(),
      direct_transition = "Next"
    ),
    transition = parse_transition(list(direct_transition = "Next"))
  )
  r <- process_state(s, make_person_clinical(), Sys.time())
  expect_equal(length(r$person@.record$imaging), 1L)
})

test_that("Device with singular code adds coded device", {
  s <- GMFState(
    name = "Wheelchair", type = "Device",
    definition = list(
      type = "Device",
      code = list(system = "SNOMED-CT", code = "228869008", display = "Manual wheelchair"),
      direct_transition = "Next"
    ),
    transition = parse_transition(list(direct_transition = "Next"))
  )
  r <- process_state(s, make_person_clinical(), Sys.time())
  expect_equal(length(r$person@.record$devices), 1L)
  expect_equal(r$person@.record$devices[[1]]$codes[[1]][["code"]], "228869008")
})

test_that("SupplyList adds one record per supply", {
  s <- GMFState(
    name = "BloodSupplies", type = "SupplyList",
    definition = list(
      type = "SupplyList",
      supplies = list(
        list(quantity = 2, code = list(system = "SNOMED-CT", code = "431069006",
                                      display = "Packed red blood cells")),
        list(quantity = 1, code = list(system = "SNOMED-CT", code = "126261006",
                                      display = "Human platelets"))
      ),
      direct_transition = "Next"
    ),
    transition = parse_transition(list(direct_transition = "Next"))
  )
  r <- process_state(s, make_person_clinical(), Sys.time())
  expect_equal(length(r$person@.record$supplies), 2L)
  expect_equal(r$person@.record$supplies[[1]]$quantity, 2)
  expect_equal(r$person@.record$supplies[[2]]$codes[[1]][["code"]], "126261006")
})
