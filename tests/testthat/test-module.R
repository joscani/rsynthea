# tests/testthat/test-module.R
library(testthat)

test_module_path <- system.file("extdata/modules/test_cold.json", package = "rsynthea")

test_that("load_module returns a Module with correct name", {
  skip_if(nchar(test_module_path) == 0, "test module not installed")
  m <- load_module(test_module_path)
  expect_true(inherits(m, "Module"))
  expect_equal(m@name, "test_cold")
})

test_that("load_module parses all states", {
  skip_if(nchar(test_module_path) == 0, "test module not installed")
  m <- load_module(test_module_path)
  expect_setequal(names(m@states),
    c("Initial", "Cold_Onset", "Cold_Duration", "Cold_Resolves", "Terminal"))
})

test_that("state types parsed correctly", {
  skip_if(nchar(test_module_path) == 0, "test module not installed")
  m <- load_module(test_module_path)
  expect_equal(m@states[["Initial"]]@type, "Initial")
  expect_equal(m@states[["Cold_Duration"]]@type, "Delay")
  expect_equal(m@states[["Terminal"]]@type, "Terminal")
})

test_that("state transition parsed for Initial state", {
  skip_if(nchar(test_module_path) == 0, "test module not installed")
  m <- load_module(test_module_path)
  t <- m@states[["Initial"]]@transition
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
