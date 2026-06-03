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

  enc  <- Encounter(id = "enc-001", time = as.POSIXct("2020-01-01"),
                    codes = list(Code("SNOMED-CT", "185349003", "Wellness")),
                    encounter_class = "ambulatory")
  cond <- Condition(id = "cond-001", time = as.POSIXct("2019-01-01"),
                    codes = list(Code("SNOMED-CT", "44054006", "T2DM")))
  med  <- Medication(id = "med-001", time = as.POSIXct("2019-03-01"),
                     codes = list(Code("RxNorm", "860975", "Metformin")))

  p@health_record@encounters  <- list(enc)
  p@health_record@conditions  <- list(cond)
  p@health_record@medications <- list(med)
  p
}

test_that("export_population returns named list with 9 tibbles", {
  patients <- list(make_test_patient(1L), make_test_patient(2L))
  result   <- export_population(patients)
  expected_names <- c("patients", "encounters", "conditions", "medications",
                      "procedures", "observations", "immunizations", "allergies",
                      "careplans")
  expect_named(result, expected_names)
  expect_true(all(vapply(result, tibble::is_tibble, logical(1))))
})

test_that("patients tibble has one row per patient with key columns", {
  result <- export_population(list(make_test_patient(1L), make_test_patient(2L)))
  expect_equal(nrow(result$patients), 2L)
  expect_true(all(c("id", "gender", "birth_date", "is_alive", "race") %in%
                    names(result$patients)))
})

test_that("encounters tibble has patient_id column linking to patients", {
  result <- export_population(list(make_test_patient(1L)))
  expect_equal(nrow(result$encounters), 1L)
  expect_true("patient_id" %in% names(result$encounters))
  expect_equal(result$encounters$patient_id[[1]], result$patients$id[[1]])
})

test_that("conditions tibble has is_active and code columns", {
  result <- export_population(list(make_test_patient(1L)))
  expect_equal(nrow(result$conditions), 1L)
  expect_true(all(c("patient_id", "is_active", "code", "description") %in%
                    names(result$conditions)))
})

test_that("empty health records produce zero-row tibbles with correct columns", {
  p <- Person(seed = 99L)
  p@attributes[["gender"]]     <- "F"
  p@attributes[["birth_date"]] <- as.POSIXct("1990-01-01")
  p@attributes[["first_name"]] <- "Jane"
  p@attributes[["last_name"]]  <- "Smith"
  result <- export_population(list(p))
  expect_equal(nrow(result$encounters),  0L)
  expect_equal(nrow(result$conditions),  0L)
  expect_equal(nrow(result$medications), 0L)
})

test_that("export_population writes CSV files when output_dir provided", {
  tmp <- tempdir()
  export_population(list(make_test_patient(1L)), output_dir = tmp)
  expect_true(file.exists(file.path(tmp, "patients.csv")))
  expect_true(file.exists(file.path(tmp, "encounters.csv")))
  expect_true(file.exists(file.path(tmp, "conditions.csv")))
})
