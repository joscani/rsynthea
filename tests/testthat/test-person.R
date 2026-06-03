# tests/testthat/test-person.R
library(testthat)

test_that("Person initializes with seed and a HealthRecord", {
  p <- Person(seed = 42L)
  expect_equal(p@seed, 42L)
  expect_true(is.character(p@id) && nchar(p@id) > 0)
  expect_equal(length(p@attributes), 0L)
  expect_true(inherits(p@health_record, "HealthRecord"))
})

test_that("age_at returns correct age in years", {
  p <- Person(seed = 1L)
  p@attributes[["birth_date"]] <- as.POSIXct("1990-01-01")
  expect_equal(floor(age_at(p, as.POSIXct("2020-01-01"))), 30)
})

test_that("is_alive is TRUE by default", {
  p <- Person(seed = 1L)
  expect_true(p@is_alive)
})

test_that("module_history starts empty", {
  p <- Person(seed = 1L)
  expect_equal(length(p@module_history), 0L)
})

test_that("Person without seed gets random seed", {
  p1 <- Person()
  p2 <- Person()
  # Different seeds (overwhelmingly likely)
  expect_false(p1@seed == p2@seed)
})

test_that("vital_signs and symptoms start empty", {
  p <- Person(seed = 1L)
  expect_equal(length(p@vital_signs), 0L)
  expect_equal(length(p@symptoms), 0L)
})
