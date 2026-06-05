# tests/testthat/test-module.R
library(testthat)

test_module_path <- system.file("extdata/modules/test_cold.json", package = "rsynthea")

test_that("load_module returns a Module with correct name", {
  skip_if(nchar(test_module_path) == 0, "test module not installed")
  m <- load_module(test_module_path)
  expect_true(inherits(m, "Module"))
  expect_equal(m$name, "test_cold")
})

test_that("load_module parses all states", {
  skip_if(nchar(test_module_path) == 0, "test module not installed")
  m <- load_module(test_module_path)
  expect_setequal(names(m$states),
    c("Initial", "Cold_Onset", "Cold_Duration", "Cold_Resolves", "Terminal"))
})

test_that("state types parsed correctly", {
  skip_if(nchar(test_module_path) == 0, "test module not installed")
  m <- load_module(test_module_path)
  expect_equal(m$states[["Initial"]][["type"]], "Initial")
  expect_equal(m$states[["Cold_Duration"]][["type"]], "Delay")
  expect_equal(m$states[["Terminal"]][["type"]], "Terminal")
})

test_that("state transition parsed for Initial state", {
  skip_if(nchar(test_module_path) == 0, "test module not installed")
  m <- load_module(test_module_path)
  t <- m$states[["Initial"]][["transition"]]
  expect_equal(t$type, "distributed")
  expect_equal(length(t$entries), 2L)
})

test_that("load_all_modules loads from a directory", {
  skip_if(nchar(test_module_path) == 0, "test module not installed")
  tmp <- tempdir()
  file.copy(test_module_path, file.path(tmp, "test_cold.json"), overwrite = TRUE)
  mods <- load_all_modules(tmp)
  expect_gte(length(mods), 1L)
  expect_true("test_cold" %in% names(mods))
})

# --- Submodule loading ---

test_that("load_all_modules keys submodule by relative path", {
  tmp <- file.path(tempdir(), "rsynthea_submod_test")
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
  subdir <- file.path(tmp, "heart")
  dir.create(subdir, showWarnings = FALSE)

  top_json <- '{"name":"TopMod","gmf_version":2,"states":{"Initial":{"type":"Initial","direct_transition":"Terminal"},"Terminal":{"type":"Terminal"}}}'
  sub_json <- '{"name":"HeartSub","gmf_version":2,"states":{"Initial":{"type":"Initial","direct_transition":"Terminal"},"Terminal":{"type":"Terminal"}}}'
  writeLines(top_json, file.path(tmp, "top_mod.json"))
  writeLines(sub_json, file.path(subdir, "heart_sub.json"))

  mods <- load_all_modules(tmp)
  expect_true("top_mod" %in% names(mods))
  expect_true("heart/heart_sub" %in% names(mods))
  expect_false(isTRUE(mods[["top_mod"]]$is_submodule))
  expect_true(isTRUE(mods[["heart/heart_sub"]]$is_submodule))
})

test_that("wellness_key differs across modules with the same state name", {
  make_well_state <- function(mod_name) {
    GMFState("WellVisit", "Encounter",
             list(type = "Encounter", wellness = TRUE, encounter_class = "ambulatory",
                  codes = list(), direct_transition = "WellVisit"),
             parse_transition(list(direct_transition = "WellVisit")),
             module_name = mod_name)
  }
  sa <- make_well_state("Mod_A")
  sb <- make_well_state("Mod_B")
  expect_false(identical(sa[["wellness_key"]], sb[["wellness_key"]]))
})

test_that("wellness encounters in two modules fire independently each year", {
  make_wellness_module <- function(mod_name) {
    mk <- function(nm, type, def, tr)
      GMFState(nm, type, def, tr, module_name = mod_name)
    states <- list2env(list(
      Initial  = mk("Initial", "Initial",
                    list(type = "Initial", direct_transition = "WellVisit"),
                    parse_transition(list(direct_transition = "WellVisit"))),
      WellVisit = mk("WellVisit", "Encounter",
                     list(type = "Encounter", wellness = TRUE,
                          encounter_class = "ambulatory", codes = list(),
                          direct_transition = "WellVisit"),
                     parse_transition(list(direct_transition = "WellVisit")))
    ), parent = emptyenv(), hash = TRUE)
    Module(name = mod_name, states = states, is_submodule = FALSE)
  }
  mods <- list(Mod_A = make_wellness_module("Mod_A"),
               Mod_B = make_wellness_module("Mod_B"))
  p <- Person(seed = 1L)
  p@attributes[["birth_date"]] <- as.POSIXct("2000-01-01")
  p <- simulate_life(p, mods, end_date = as.POSIXct("2010-01-01"))
  # Both modules should fire ~10 wellness encounters each = ~20 total
  expect_gte(length(p@.record$encounters), 15L)
})

test_that("simulate_life does not run submodule modules", {
  tmp <- file.path(tempdir(), "rsynthea_submod_sim_test")
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
  subdir <- file.path(tmp, "dangerous")
  dir.create(subdir, showWarnings = FALSE)

  # Submodule that kills patient on first state — should never run
  danger_json <- '{"name":"Danger","gmf_version":2,"states":{"Initial":{"type":"Initial","direct_transition":"Kill"},"Kill":{"type":"Death"}}}'
  writeLines(danger_json, file.path(subdir, "danger.json"))

  mods <- load_all_modules(tmp)
  p <- Person(seed = 1L)
  p@attributes[["birth_date"]] <- as.POSIXct("2019-06-01")
  p <- simulate_life(p, mods, end_date = as.POSIXct("2020-01-01"))
  expect_true(p@is_alive)
})
