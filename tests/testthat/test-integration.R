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
