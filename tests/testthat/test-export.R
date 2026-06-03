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

  rec <- p@.record
  rec$encounters[[1]] <- list(
    id = "enc-001", time = as.POSIXct("2020-01-01"), end_time = NULL,
    codes = list(list(system = "SNOMED-CT", code = "185349003", display = "Wellness")),
    encounter_class = "ambulatory"
  )
  rec$conditions[[1]] <- list(
    id = "cond-001", time = as.POSIXct("2019-01-01"), end_time = NULL,
    codes = list(list(system = "SNOMED-CT", code = "44054006", display = "T2DM")),
    is_active = TRUE
  )
  rec$medications[[1]] <- list(
    id = "med-001", time = as.POSIXct("2019-03-01"), end_time = NULL,
    codes = list(list(system = "RxNorm", code = "860975", display = "Metformin")),
    is_active = TRUE
  )
  rec$imaging[[1]] <- list(
    id = "img-001", time = as.POSIXct("2020-02-01"),
    codes = list(list(system = "SNOMED-CT", code = "399208008", display = "Chest X-ray")),
    series = list(list(body_site = "Chest"))
  )
  rec$devices[[1]] <- list(
    id = "dev-001", time = as.POSIXct("2020-03-01"), end_time = NULL,
    codes = list(list(system = "SNOMED-CT", code = "228869008", display = "Wheelchair")),
    is_active = TRUE
  )
  rec$reports[[1]] <- list(
    id = "rep-001", time = as.POSIXct("2020-04-01"),
    codes = list(list(system = "LOINC", code = "58410-2", display = "CBC panel")),
    observations = list(list(
      id = "obs-rep-001", time = as.POSIXct("2020-04-01"), value = 10,
      unit = "g/dL",
      codes = list(list(system = "LOINC", code = "718-7", display = "Hemoglobin"))
    ))
  )
  rec$supplies[[1]] <- list(
    id = "sup-001", time = as.POSIXct("2020-05-01"), quantity = 2,
    codes = list(list(system = "SNOMED-CT", code = "431069006", display = "Packed red blood cells"))
  )
  p
}

test_that("export_population returns named list with all supported tibbles", {
  patients <- list(make_test_patient(1L), make_test_patient(2L))
  result   <- export_population(patients)
  expected_names <- c("patients", "encounters", "conditions", "medications",
                      "procedures", "observations", "immunizations", "allergies",
                      "careplans", "imaging", "devices", "reports",
                      "report_observations", "supplies")
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

test_that("additional clinical domains are exported", {
  result <- export_population(list(make_test_patient(1L)))
  expect_equal(nrow(result$imaging), 1L)
  expect_equal(nrow(result$devices), 1L)
  expect_equal(nrow(result$reports), 1L)
  expect_equal(nrow(result$report_observations), 1L)
  expect_equal(nrow(result$supplies), 1L)
  expect_equal(result$devices$code[[1]], "228869008")
  expect_equal(result$supplies$quantity[[1]], "2")
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
