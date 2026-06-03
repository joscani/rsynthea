# tests/testthat/test-logic.R
library(testthat)

make_person <- function(gender = "M", birth_year = 1980, race = "white",
                        extra_attrs = list()) {
  p <- Person(seed = 1L)
  p@attributes <- c(
    list(
      gender     = gender,
      birth_date = as.POSIXct(paste0(birth_year, "-01-01")),
      race       = race
    ),
    extra_attrs
  )
  .REC$e <- p@.record
  p
}

test_that("Gender condition matches correctly", {
  p <- make_person(gender = "M")
  t <- Sys.time()
  expect_true(evaluate_condition(list(condition_type = "Gender", gender = "M"), p, t))
  expect_false(evaluate_condition(list(condition_type = "Gender", gender = "F"), p, t))
})

test_that("Age condition uses operator correctly", {
  p <- make_person(birth_year = 1980)
  time <- as.POSIXct("2020-01-01")
  expect_true(evaluate_condition(
    list(condition_type = "Age", operator = ">=", quantity = 30, unit = "years"), p, time))
  expect_false(evaluate_condition(
    list(condition_type = "Age", operator = ">", quantity = 50, unit = "years"), p, time))
})

test_that("And requires all sub-conditions", {
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

  cond_fail <- list(
    condition_type = "And",
    conditions = list(
      list(condition_type = "Gender", gender = "F"),
      list(condition_type = "Age", operator = ">=", quantity = 30, unit = "years")
    )
  )
  expect_false(evaluate_condition(cond_fail, p, time))
})

test_that("Or requires at least one sub-condition", {
  p <- make_person(gender = "F")
  t <- Sys.time()
  cond <- list(
    condition_type = "Or",
    conditions = list(
      list(condition_type = "Gender", gender = "M"),
      list(condition_type = "Gender", gender = "F")
    )
  )
  expect_true(evaluate_condition(cond, p, t))
})

test_that("Not inverts condition", {
  p <- make_person(gender = "M")
  cond <- list(
    condition_type = "Not",
    condition = list(condition_type = "Gender", gender = "F")
  )
  expect_true(evaluate_condition(cond, p, Sys.time()))
})

test_that("Attribute condition with == operator", {
  p <- make_person(extra_attrs = list(diabetes = TRUE))
  expect_true(evaluate_condition(
    list(condition_type = "Attribute", attribute = "diabetes", operator = "==", value = TRUE),
    p, Sys.time()
  ))
  expect_false(evaluate_condition(
    list(condition_type = "Attribute", attribute = "diabetes", operator = "==", value = FALSE),
    p, Sys.time()
  ))
})

test_that("Active Condition checks health record", {
  p <- make_person()
  cond_env <- new.env(parent = emptyenv())
  cond_env$id <- "c1"
  cond_env$time <- Sys.time()
  cond_env$codes <- list(list(system = "SNOMED-CT", code = "44054006", display = "T2DM"))
  cond_env$is_active <- TRUE
  cond_env$end_time <- NULL
  p@.record$conditions <- list(cond_env)
  p@.record$.active_conditions[["44054006"]] <- cond_env

  expect_true(evaluate_condition(
    list(condition_type = "Active Condition",
         codes = list(list(system = "SNOMED-CT", code = "44054006", display = "T2DM"))),
    p, Sys.time()
  ))
  expect_false(evaluate_condition(
    list(condition_type = "Active Condition",
         codes = list(list(system = "SNOMED-CT", code = "99999999", display = "Unknown"))),
    p, Sys.time()
  ))
})

test_that("True and False conditions", {
  p <- make_person()
  expect_true(evaluate_condition(list(condition_type = "True"), p, Sys.time()))
  expect_false(evaluate_condition(list(condition_type = "False"), p, Sys.time()))
})

test_that("Unknown condition_type returns FALSE", {
  p <- make_person()
  expect_false(evaluate_condition(list(condition_type = "WhatIsThis"), p, Sys.time()))
})

test_that("Race condition", {
  p <- make_person(race = "black")
  expect_true(evaluate_condition(list(condition_type = "Race", race = "black"), p, Sys.time()))
  expect_false(evaluate_condition(list(condition_type = "Race", race = "white"), p, Sys.time()))
})
