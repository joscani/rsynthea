# tests/testthat/test-generator.R
library(testthat)

test_that("generate_population returns list of Person objects", {
  result <- generate_population(
    n        = 3L,
    seed     = 42L,
    modules  = list(),
    end_date = as.POSIXct("2000-12-31")
  )
  expect_equal(length(result), 3L)
  expect_true(all(vapply(result, function(p) inherits(p, "Person"), logical(1))))
})

test_that("generate_population respects gender filter", {
  result <- generate_population(
    n = 10L, seed = 1L, gender = "F",
    modules = list(), end_date = as.POSIXct("2000-12-31")
  )
  genders <- vapply(result, function(p) p@attributes[["gender"]], character(1))
  expect_true(all(genders == "F"))
})

test_that("generate_population is reproducible with same seed", {
  r1 <- generate_population(n = 2L, seed = 99L, modules = list(),
                             end_date = as.POSIXct("2000-12-31"))
  r2 <- generate_population(n = 2L, seed = 99L, modules = list(),
                             end_date = as.POSIXct("2000-12-31"))
  expect_equal(r1[[1]]@attributes[["gender"]], r2[[1]]@attributes[["gender"]])
  expect_equal(r1[[1]]@attributes[["race"]],   r2[[1]]@attributes[["race"]])
})

test_that("generate_population patient IDs are unique", {
  result <- generate_population(n = 5L, seed = 7L, modules = list(),
                                end_date = as.POSIXct("2000-12-31"))
  ids <- vapply(result, function(p) p@id, character(1))
  expect_equal(length(unique(ids)), 5L)
})

test_that("generate_population respects age range", {
  result <- generate_population(
    n = 10L, seed = 42L, min_age = 20L, max_age = 30L,
    modules = list(), end_date = as.POSIXct("2020-01-01")
  )
  ages <- vapply(result, function(p) age_at(p, as.POSIXct("2020-01-01")), numeric(1))
  expect_true(all(ages >= 19 & ages <= 31))  # small tolerance for date math
})
