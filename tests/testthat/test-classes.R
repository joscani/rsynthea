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
  expect_true(inherits(enc, "rsynthea_Encounter"))
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

test_that("Medication is_active TRUE by default, end_time NULL", {
  med <- Medication(
    id = "med-001",
    time = as.POSIXct("2020-01-01"),
    codes = list(Code(system = "RxNorm", code = "860975", display = "Metformin"))
  )
  expect_true(med@is_active)
  expect_null(med@end_time)
})
