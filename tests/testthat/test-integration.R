# tests/testthat/test-integration.R
library(testthat)

test_that("generate_population with no modules produces valid patients tibble", {
  set.seed(42)
  patients <- generate_population(
    n        = 5L,
    seed     = 42L,
    modules  = list(),
    end_date = as.POSIXct("2020-12-31")
  )
  result <- export_population(patients)

  expect_equal(nrow(result$patients), 5L)
  expect_true(all(c("id", "gender", "birth_date", "is_alive") %in%
                    names(result$patients)))
  expect_true(all(result$patients$gender %in% c("M", "F")))
  expect_true(all(!is.na(result$patients$birth_date)))
})

test_that("patient IDs are unique across a population", {
  patients <- generate_population(n = 10L, seed = 1L, modules = list(),
                                  end_date = as.POSIXct("2000-12-31"))
  ids <- vapply(patients, function(p) p@id, character(1))
  expect_equal(length(unique(ids)), 10L)
})

test_that("export_population writes CSV files that can be read back", {
  tmp <- tempdir()
  patients <- generate_population(n = 3L, seed = 5L, modules = list(),
                                  end_date = as.POSIXct("2000-12-31"))
  export_population(patients, output_dir = tmp)

  df <- utils::read.csv(file.path(tmp, "patients.csv"))
  expect_equal(nrow(df), 3L)
  expect_true("id" %in% names(df))
})

test_that("prediabetic patient is diagnosed with Prediabetes, not T2DM", {
  all_mods <- load_all_modules()
  sub_keys <- grep("^metabolic_syndrome/", names(all_mods), value = TRUE)
  use_mods <- c(
    all_mods["vital_signs_basic"],
    all_mods["metabolic_syndrome_care"],
    all_mods[sub_keys]
  )

  person <- Person(seed = 1L)
  person@attributes[["birth_date"]] <- as.POSIXct("1980-01-01")
  person@attributes[["prediabetes"]] <- "prediabetes"

  result <- simulate_life(person, use_mods, end_date = as.POSIXct("1983-01-01"))

  cond_codes <- vapply(
    result@.record$conditions,
    function(c) c$codes[[1L]]$code,
    character(1L)
  )
  expect_true("714628002" %in% cond_codes,
              label = "prediabetic must receive Prediabetes condition")
  expect_false("44054006" %in% cond_codes,
               label = "prediabetic must NOT receive T2DM condition")
})

test_that("metabolic_syndrome_care fires wellness encounters every year for diabetics", {
  all_mods <- load_all_modules()
  sub_keys <- grep("^metabolic_syndrome/", names(all_mods), value = TRUE)
  use_mods <- c(
    all_mods["vital_signs_basic"],
    all_mods["metabolic_syndrome_care"],
    all_mods[sub_keys]
  )

  person <- Person(seed = 1L)
  person@attributes[["birth_date"]] <- as.POSIXct("1980-01-01")
  person@attributes[["diabetes"]]          <- "t2dm"
  person@attributes[["diabetes_severity"]] <- 1L

  result <- simulate_life(person, use_mods, end_date = as.POSIXct("1985-01-01"))

  # 5 years => at least 3 annual wellness encounters
  expect_gte(length(result@.record$encounters), 3L,
             label = "diabetic needs annual wellness for 5 years")
})

test_that("female_reproduction does not terminate when contraceptive_type is null", {
  all_mods <- load_all_modules()
  repro_keys <- c("pregnancy", "female_reproduction", "contraceptives",
                  "sexual_activity", "contraceptive_maintenance")
  sub_keys <- grep("^contraceptives/", names(all_mods), value = TRUE)
  use_mods <- c(all_mods[repro_keys], all_mods[sub_keys])

  person <- Person(seed = 7L)
  person@attributes[["birth_date"]] <- as.POSIXct("1960-01-01")
  person@attributes[["gender"]]     <- "F"
  person@attributes[["sexually_active"]] <- TRUE

  # No contraceptive_type set — simulates gap between clear_contraceptive and reassignment
  set.seed(7L)
  result <- simulate_life(person, use_mods, end_date = as.POSIXct("1985-01-01"))

  # With null type and 19.3% monthly rate, probability of 0 pregnancies in 5 years is ~0.
  # If module terminates early we get 0 pregnancies.
  n_pregnancies <- sum(vapply(result@.record$conditions, function(c) {
    identical(c$codes[[1L]]$code, "72892002")
  }, logical(1L)))
  expect_gt(n_pregnancies, 0L,
            label = "module should not terminate on null contraceptive_type")
})

test_that("pregnancy TFR is approximately 2.1 for a female population", {
  all_mods <- load_all_modules()
  repro_keys <- c("pregnancy", "female_reproduction", "contraceptives",
                  "sexual_activity", "contraceptive_maintenance")
  sub_keys <- grep("^contraceptives/", names(all_mods), value = TRUE)
  use_mods <- c(all_mods[repro_keys], all_mods[sub_keys])

  set.seed(1L)
  patients <- generate_population(30L, seed = 1L, modules = use_mods,
                                  end_date = as.POSIXct("2020-01-01"))
  live_births <- vapply(patients, function(p) {
    if (isTRUE(p@attributes[["gender"]] == "M")) return(NA_real_)
    as.numeric(p@attributes[["number_of_children"]] %||% 0L)
  }, numeric(1L))
  tfr <- mean(live_births[!is.na(live_births)])
  expect_gte(tfr, 1.3, label = "TFR should be at least 1.3")
  expect_lte(tfr, 3.0, label = "TFR should be at most 3.0")
})

test_that("generate_population with real modules produces clinical events", {
  n_modules <- length(list.files(
    system.file("extdata/modules", package = "rsynthea"),
    pattern = "\\.json$", recursive = TRUE
  ))
  skip_if(n_modules < 10, "Full module set not installed")

  set.seed(42)
  patients <- generate_population(
    n        = 3L,
    seed     = 42L,
    end_date = as.POSIXct("2020-12-31")
  )
  result <- export_population(patients)

  expect_equal(nrow(result$patients), 3L)
  # With 242 modules over 30+ years, should produce clinical events
  total_events <- nrow(result$encounters) + nrow(result$conditions) +
                  nrow(result$medications) + nrow(result$immunizations)
  expect_gt(total_events, 0L)
})
