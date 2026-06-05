# tests/testthat/test-state-flow.R
library(testthat)

make_state <- function(type, extra_def = list()) {
  def <- c(list(type = type), extra_def)
  GMFState(
    name       = "TestState",
    type       = type,
    definition = def,
    transition = parse_transition(c(def, list(direct_transition = "Next")))
  )
}

make_person_basic <- function() {
  p <- Person(seed = 1L)
  p@attributes[["birth_date"]] <- as.POSIXct("1990-01-01")
  p@attributes[["gender"]]     <- "M"
  .REC$e <- p@.record
  p
}

test_that("process_state on Initial returns next_state = 'Next'", {
  s <- make_state("Initial")
  p <- make_person_basic()
  r <- process_state(s, p, Sys.time())
  expect_equal(r$next_state, "Next")
  expect_true(r$person@is_alive)
})

test_that("process_state on Simple returns next_state", {
  s <- make_state("Simple")
  r <- process_state(s, make_person_basic(), Sys.time())
  expect_equal(r$next_state, "Next")
})

test_that("process_state on Terminal returns NULL next_state", {
  s <- GMFState(name = "Terminal", type = "Terminal",
                definition = list(type = "Terminal"), transition = NULL)
  r <- process_state(s, make_person_basic(), Sys.time())
  expect_null(r$next_state)
})

test_that("Delay blocks on first call, transitions after delay elapsed", {
  s <- GMFState(
    name = "WaitState", type = "Delay",
    definition = list(type = "Delay",
                      exact = list(quantity = 30, unit = "days"),
                      direct_transition = "AfterDelay"),
    transition = parse_transition(list(direct_transition = "AfterDelay"))
  )
  p   <- make_person_basic()
  now <- as.POSIXct("2020-01-01")

  # First call: should stay (delay not elapsed)
  r1 <- process_state(s, p, now)
  expect_equal(r1$next_state, "WaitState")

  # After 31 days: should transition
  r2 <- process_state(s, r1$person, now + 31 * 86400)
  expect_equal(r2$next_state, "AfterDelay")
})

test_that("Guard blocks when condition false, passes when true", {
  s <- GMFState(
    name = "GuardTest", type = "Guard",
    definition = list(
      type = "Guard",
      allow = list(condition_type = "Gender", gender = "M"),
      direct_transition = "Allowed"
    ),
    transition = parse_transition(list(direct_transition = "Allowed"))
  )
  p_m <- make_person_basic()
  p_f <- make_person_basic(); p_f@attributes[["gender"]] <- "F"

  expect_equal(process_state(s, p_m, Sys.time())$next_state, "Allowed")
  expect_equal(process_state(s, p_f, Sys.time())$next_state, "GuardTest")
})

test_that("SetAttribute sets attribute and transitions", {
  s <- GMFState(
    name = "SetDiabetes", type = "SetAttribute",
    definition = list(type = "SetAttribute", attribute = "has_diabetes",
                      value = TRUE, direct_transition = "Next"),
    transition = parse_transition(list(direct_transition = "Next"))
  )
  r <- process_state(s, make_person_basic(), Sys.time())
  expect_true(r$person@attributes[["has_diabetes"]])
  expect_equal(r$next_state, "Next")
})

test_that("Delay with v2 distribution EXACT does not fire before its duration", {
  s <- GMFState(
    name = "WaitV2", type = "Delay",
    definition = list(
      type         = "Delay",
      distribution = list(kind = "EXACT", parameters = list(value = 1)),
      unit         = "years",
      direct_transition = "After"
    ),
    transition = parse_transition(list(direct_transition = "After"))
  )
  p   <- make_person_basic()
  now <- as.POSIXct("2020-01-01")

  r1 <- process_state(s, p, now)
  expect_equal(r1$next_state, "WaitV2")

  # 6 months later — should still be waiting (duration = 1 year)
  r2 <- process_state(s, r1$person, now + 180 * 86400)
  expect_equal(r2$next_state, "WaitV2")
})

test_that("SetAttribute with v2 GAUSSIAN distribution sets a numeric attribute", {
  s <- GMFState(
    name = "SampleAge", type = "SetAttribute",
    definition = list(
      type         = "SetAttribute",
      attribute    = "time_until_event",
      distribution = list(kind = "GAUSSIAN",
                          parameters = list(mean = 55, standardDeviation = 15)),
      direct_transition = "Next"
    ),
    transition = parse_transition(list(direct_transition = "Next"))
  )
  set.seed(42L)
  r <- process_state(s, make_person_basic(), Sys.time())
  val <- r$person@attributes[["time_until_event"]]
  expect_true(is.numeric(val))
  expect_false(is.null(val))
})

test_that("Counter increments attribute", {
  s <- GMFState(
    name = "Count", type = "Counter",
    definition = list(type = "Counter", attribute = "visit_count",
                      action = "increment", direct_transition = "Next"),
    transition = parse_transition(list(direct_transition = "Next"))
  )
  p  <- make_person_basic()
  r1 <- process_state(s, p, Sys.time())
  expect_equal(r1$person@attributes[["visit_count"]], 1)
  r2 <- process_state(s, r1$person, Sys.time())
  expect_equal(r2$person@attributes[["visit_count"]], 2)
})

test_that("Death marks person not alive and sets death_date", {
  s <- GMFState(
    name = "Die", type = "Death",
    definition = list(type = "Death"), transition = NULL
  )
  t <- as.POSIXct("2020-06-01")
  r <- process_state(s, make_person_basic(), t)
  expect_false(r$person@is_alive)
  expect_equal(r$person@attributes[["death_date"]], t)
  expect_null(r$next_state)
})
