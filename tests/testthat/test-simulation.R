# tests/testthat/test-simulation.R
library(testthat)

test_that("simulate_life with no modules returns person unchanged", {
  p <- Person(seed = 1L)
  p@attributes[["birth_date"]] <- as.POSIXct("2000-01-01")
  result <- simulate_life(p, modules = list(), end_date = as.POSIXct("2001-01-01"))
  expect_true(result@is_alive)
  expect_equal(result@seed, 1L)
})

test_that("advance_module on test_cold runs without error", {
  path <- system.file("extdata/modules/test_cold.json", package = "rsynthea")
  skip_if(nchar(path) == 0)
  m <- load_module(path)
  p <- Person(seed = 42L)
  p@attributes[["birth_date"]] <- as.POSIXct("1980-01-01")
  p@attributes[["gender"]] <- "M"
  time <- as.POSIXct("2010-01-01")
  result <- advance_module(p, m, time)
  expect_true(inherits(result, "Person"))
})

test_that("simulate_life stops when person dies", {
  death_json <- '{
    "name": "instant_death",
    "states": {
      "Initial": {"type": "Initial", "direct_transition": "Die"},
      "Die": {"type": "Death"}
    }
  }'
  tmp <- tempfile(fileext = ".json")
  writeLines(death_json, tmp)
  m <- load_module(tmp)
  unlink(tmp)

  p <- Person(seed = 5L)
  p@attributes[["birth_date"]] <- as.POSIXct("1990-01-01")
  result <- simulate_life(p, modules = list(instant_death = m),
                          end_date = as.POSIXct("2050-01-01"))
  expect_false(result@is_alive)
})

test_that("simulate_life starts from birth_date not before", {
  # Module that sets an attribute on first run
  marker_json <- '{
    "name": "marker",
    "states": {
      "Initial": {"type": "Initial", "direct_transition": "Mark"},
      "Mark": {"type": "SetAttribute", "attribute": "sim_started", "value": true,
               "direct_transition": "Terminal"},
      "Terminal": {"type": "Terminal"}
    }
  }'
  tmp <- tempfile(fileext = ".json")
  writeLines(marker_json, tmp)
  m <- load_module(tmp)
  unlink(tmp)

  p <- Person(seed = 1L)
  birth <- as.POSIXct("2000-06-01")
  p@attributes[["birth_date"]] <- birth
  result <- simulate_life(p, modules = list(marker = m),
                          end_date = as.POSIXct("2000-12-31"))
  expect_true(isTRUE(result@attributes[["sim_started"]]))
})
