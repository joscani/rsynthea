# tests/testthat/test-transition.R
library(testthat)

test_that("parse_transition returns direct type", {
  t <- parse_transition(list(direct_transition = "Next_State"))
  expect_equal(t$type, "direct")
  expect_equal(t$target, "Next_State")
})

test_that("parse_transition returns NULL when no transition key", {
  expect_null(parse_transition(list()))
  expect_null(parse_transition(list(type = "Terminal")))
})

test_that("resolve_transition direct always returns same target", {
  t <- parse_transition(list(direct_transition = "Next_State"))
  p <- Person(seed = 1L)
  results <- replicate(5, resolve_transition(t, p, Sys.time()))
  expect_true(all(results == "Next_State"))
})

test_that("resolve_transition distributed respects weights", {
  set.seed(42)
  t <- parse_transition(list(distributed_transition = list(
    list(distribution = 0.9, transition = "Likely"),
    list(distribution = 0.1, transition = "Unlikely")
  )))
  p <- Person(seed = 1L)
  results <- replicate(500, resolve_transition(t, p, Sys.time()))
  expect_gt(mean(results == "Likely"), 0.75)
})

test_that("resolve_transition conditional picks first matching", {
  t <- parse_transition(list(conditional_transition = list(
    list(condition = list(condition_type = "Gender", gender = "F"),
         transition = "Female_Branch"),
    list(transition = "Default_Branch")
  )))
  p_f <- Person(seed = 1L); p_f@attributes[["gender"]] <- "F"
  p_m <- Person(seed = 2L); p_m@attributes[["gender"]] <- "M"

  expect_equal(resolve_transition(t, p_f, Sys.time()), "Female_Branch")
  expect_equal(resolve_transition(t, p_m, Sys.time()), "Default_Branch")
})

test_that("parse_transition handles 'transition' key (simple form)", {
  t <- parse_transition(list(transition = "SomeState"))
  expect_equal(t$type, "direct")
  expect_equal(t$target, "SomeState")
})

test_that("resolve_transition on NULL returns NULL", {
  p <- Person(seed = 1L)
  expect_null(resolve_transition(NULL, p, Sys.time()))
})
