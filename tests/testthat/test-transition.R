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

test_that("lookup_table_transition uses matching CSV row probabilities", {
  csv <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(
    gender = c("M", "F"),
    A = c(1, 0),
    B = c(0, 1),
    check.names = FALSE
  ), csv, row.names = FALSE)

  t <- parse_transition(list(lookup_table_transition = list(
    list(transition = "A", lookup_table_name = csv, default_probability = 0),
    list(transition = "B", lookup_table_name = csv, default_probability = 1)
  )))
  p <- Person(seed = 1L)
  p@attributes[["gender"]] <- "M"

  expect_equal(resolve_transition(t, p, Sys.time()), "A")
})

test_that("lookup_table_transition supports age range criteria", {
  csv <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(
    age = c("0-17", "18-140"),
    Child = c(1, 0),
    Adult = c(0, 1),
    check.names = FALSE
  ), csv, row.names = FALSE)

  t <- parse_transition(list(lookup_table_transition = list(
    list(transition = "Child", lookup_table_name = csv, default_probability = 0),
    list(transition = "Adult", lookup_table_name = csv, default_probability = 1)
  )))
  p <- Person(seed = 1L)
  p@attributes[["birth_date"]] <- as.POSIXct("2010-01-01")

  expect_equal(resolve_transition(t, p, as.POSIXct("2020-01-01")), "Child")
})

test_that("lookup_table_transition treats binary non-transition columns as criteria", {
  csv <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(
    smoker = c(0, 1),
    LowRisk = c(1, 0),
    HighRisk = c(0, 1),
    check.names = FALSE
  ), csv, row.names = FALSE)

  t <- parse_transition(list(lookup_table_transition = list(
    list(transition = "LowRisk", lookup_table_name = csv, default_probability = 1),
    list(transition = "HighRisk", lookup_table_name = csv, default_probability = 0)
  )))
  p <- Person(seed = 1L)
  p@attributes[["smoker"]] <- 1

  expect_equal(resolve_transition(t, p, Sys.time()), "HighRisk")
})

test_that("lookup_table_transition supports time range criteria", {
  csv <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(
    time = c("946684800000-978220800000", "978307200000-1009843200000"),
    Early = c(1, 0),
    Later = c(0, 1),
    check.names = FALSE
  ), csv, row.names = FALSE)

  t <- parse_transition(list(lookup_table_transition = list(
    list(transition = "Early", lookup_table_name = csv, default_probability = 0),
    list(transition = "Later", lookup_table_name = csv, default_probability = 1)
  )))
  p <- Person(seed = 1L)
  .REC$e <- p@.record
  .REC$e$.t_num <- as.numeric(as.POSIXct("2000-06-01", tz = "UTC"))

  expect_equal(resolve_transition(t, p, as.POSIXct("2000-06-01", tz = "UTC")), "Early")
})

test_that("lookup_table_transition finds packaged lookup tables", {
  path <- .lookup_table_path("covid-19-severity-outcomes.csv")
  expect_false(is.null(path))
  expect_true(file.exists(path))
})

test_that("lookup_table_transition falls back to default probabilities", {
  t <- parse_transition(list(lookup_table_transition = list(
    list(transition = "A", lookup_table_name = "missing.csv", default_probability = 0),
    list(transition = "B", lookup_table_name = "missing.csv", default_probability = 1)
  )))
  p <- Person(seed = 1L)

  expect_equal(resolve_transition(t, p, Sys.time()), "B")
})
