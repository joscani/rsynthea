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

test_that("generate_population validates scalar counts", {
  expect_error(
    generate_population(n = 0L, modules = list(), end_date = as.POSIXct("2000-12-31")),
    "`n` must be a single integer"
  )
  expect_error(
    generate_population(n = 1.5, modules = list(), end_date = as.POSIXct("2000-12-31")),
    "`n` must be a single integer"
  )
  expect_error(
    generate_population(n = 1L, mc.cores = 0L, modules = list(),
                        end_date = as.POSIXct("2000-12-31")),
    "`mc.cores` must be a single integer"
  )
})

test_that("generate_population validates demographics inputs", {
  expect_error(
    generate_population(n = 1L, gender = "X", modules = list(),
                        end_date = as.POSIXct("2000-12-31")),
    "`gender` must be NULL"
  )
  expect_error(
    generate_population(n = 1L, min_age = 40L, max_age = 20L, modules = list(),
                        end_date = as.POSIXct("2000-12-31")),
    "`min_age` must be less than or equal"
  )
  expect_error(
    generate_population(n = 1L, min_age = -1L, modules = list(),
                        end_date = as.POSIXct("2000-12-31")),
    "`min_age` must be a single integer"
  )
})

test_that("generate_population validates seed, date, and modules", {
  expect_error(
    generate_population(n = 2L, seed = .Machine$integer.max, modules = list(),
                        end_date = as.POSIXct("2000-12-31")),
    "`seed \\+ n - 1`"
  )
  expect_error(
    generate_population(n = 1L, modules = list(), end_date = "2000-12-31"),
    "`end_date` must be a single non-missing POSIXct"
  )
  expect_error(
    generate_population(n = 1L, modules = list(not_a_module = list()),
                        end_date = as.POSIXct("2000-12-31")),
    "`modules` must contain only Module objects"
  )
})
