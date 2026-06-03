# tests/testthat/test-state-clinical.R
library(testthat)

make_person_clinical <- function() {
  p <- Person(seed = 1L)
  p@attributes[["birth_date"]] <- as.POSIXct("1980-01-01")
  p@attributes[["gender"]] <- "M"
  p
}

# --- Encounter ---

test_that("Encounter adds entry to health_record@encounters", {
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
  expect_equal(length(r$person@health_record@encounters), 1L)
  expect_equal(r$person@health_record@encounters[[1]]@encounter_class, "ambulatory")
  expect_false(is.null(r$person@attributes[["__current_encounter__"]]))
  expect_equal(r$next_state, "Next")
})

test_that("EncounterEnd sets end_time on current encounter", {
  p <- make_person_clinical()
  enc <- Encounter(id = "enc-001", time = as.POSIXct("2020-06-01"),
                   codes = list(), encounter_class = "ambulatory")
  p@health_record@encounters <- list(enc)
  p@attributes[["__current_encounter__"]] <- "enc-001"

  s <- GMFState(
    name = "EndVisit", type = "EncounterEnd",
    definition = list(type = "EncounterEnd", direct_transition = "Next"),
    transition = parse_transition(list(direct_transition = "Next"))
  )
  r <- process_state(s, p, as.POSIXct("2020-06-01"))
  expect_false(is.null(r$person@health_record@encounters[[1]]@end_time))
  expect_null(r$person@attributes[["__current_encounter__"]])
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
  conds <- r$person@health_record@conditions
  expect_equal(length(conds), 1L)
  expect_true(conds[[1]]@is_active)
  expect_equal(conds[[1]]@codes[[1]]@code, "44054006")
})

test_that("ConditionEnd deactivates the matching condition", {
  p <- make_person_clinical()
  dm_code <- Code(system = "SNOMED-CT", code = "44054006", display = "T2DM")
  cond_entry <- Condition(id = "c1", time = as.POSIXct("2019-01-01"), codes = list(dm_code))
  p@health_record@conditions <- list(cond_entry)
  p@attributes[["__condition_ref__DiabetesOnset"]] <- "c1"

  s <- GMFState(
    name = "DiabetesEnd", type = "ConditionEnd",
    definition = list(type = "ConditionEnd", condition_onset = "DiabetesOnset",
                      direct_transition = "Next"),
    transition = parse_transition(list(direct_transition = "Next"))
  )
  r <- process_state(s, p, as.POSIXct("2022-01-01"))
  expect_false(r$person@health_record@conditions[[1]]@is_active)
  expect_false(is.null(r$person@health_record@conditions[[1]]@end_time))
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
  meds <- r$person@health_record@medications
  expect_equal(length(meds), 1L)
  expect_true(meds[[1]]@is_active)
  expect_equal(meds[[1]]@codes[[1]]@code, "860975")
})

test_that("MedicationEnd deactivates the matching medication", {
  p <- make_person_clinical()
  med_code <- Code(system = "RxNorm", code = "860975", display = "Metformin")
  med <- Medication(id = "m1", time = as.POSIXct("2020-01-01"), codes = list(med_code))
  p@health_record@medications <- list(med)
  p@attributes[["__medication_ref__PrescribeMetformin"]] <- "m1"

  s <- GMFState(
    name = "StopMetformin", type = "MedicationEnd",
    definition = list(type = "MedicationEnd", medication_order = "PrescribeMetformin",
                      direct_transition = "Next"),
    transition = parse_transition(list(direct_transition = "Next"))
  )
  r <- process_state(s, p, as.POSIXct("2021-01-01"))
  expect_false(r$person@health_record@medications[[1]]@is_active)
  expect_false(is.null(r$person@health_record@medications[[1]]@end_time))
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
  expect_equal(length(r$person@health_record@procedures), 1L)
  expect_equal(r$person@health_record@procedures[[1]]@codes[[1]]@code, "80146002")
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
  expect_equal(length(r$person@health_record@immunizations), 1L)
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
  expect_equal(length(r$person@health_record@allergies), 1L)
  expect_true(r$person@health_record@allergies[[1]]@is_active)
})

test_that("AllergyEnd deactivates active allergies", {
  p <- make_person_clinical()
  allergy <- AllergyIntolerance(
    id = "a1",
    time = as.POSIXct("2020-01-01"),
    codes = list(Code(system = "RxNorm", code = "7980", display = "Penicillin")),
    is_active = TRUE
  )
  p@health_record@allergies <- list(allergy)

  s <- GMFState(
    name = "PenicillinAllergyEnd", type = "AllergyEnd",
    definition = list(type = "AllergyEnd", direct_transition = "Next"),
    transition = parse_transition(list(direct_transition = "Next"))
  )
  r <- process_state(s, p, as.POSIXct("2025-01-01"))
  expect_false(r$person@health_record@allergies[[1]]@is_active)
  expect_false(is.null(r$person@health_record@allergies[[1]]@end_time))
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
  expect_equal(length(r$person@health_record@careplans), 1L)
  expect_true(r$person@health_record@careplans[[1]]@is_active)
  expect_equal(length(r$person@health_record@careplans[[1]]@activities), 1L)
})

test_that("CarePlanEnd deactivates the matching care plan", {
  p <- make_person_clinical()
  cp <- CarePlan(
    id = "cp1",
    time = as.POSIXct("2020-01-01"),
    codes = list(Code(system = "SNOMED-CT", code = "408290009", display = "Diabetes care")),
    activities = list(),
    is_active = TRUE
  )
  p@health_record@careplans <- list(cp)
  p@attributes[["__careplan_ref__DiabetesCare"]] <- "cp1"

  s <- GMFState(
    name = "DiabetesCareEnd", type = "CarePlanEnd",
    definition = list(type = "CarePlanEnd", careplan = "DiabetesCare", direct_transition = "Next"),
    transition = parse_transition(list(direct_transition = "Next"))
  )
  r <- process_state(s, p, as.POSIXct("2022-01-01"))
  expect_false(r$person@health_record@careplans[[1]]@is_active)
  expect_false(is.null(r$person@health_record@careplans[[1]]@end_time))
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
  obs <- r$person@health_record@observations
  expect_equal(length(obs), 1L)
  expect_gte(as.numeric(obs[[1]]@value), 6.5)
  expect_lte(as.numeric(obs[[1]]@value), 8.0)
  expect_equal(obs[[1]]@unit, "%")
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
  expect_equal(as.numeric(r$person@health_record@observations[[1]]@value), 75)
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

test_that("ImagingStudy adds to health_record@imaging", {
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
  expect_equal(length(r$person@health_record@imaging), 1L)
})
