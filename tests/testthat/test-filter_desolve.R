test_that("desolve_export writes a file containing library(deSolve) and ode_fn", {
  ir  <- parse_model("dX/dt = r * X\nX[0] = 1\nr = 0.5\nt0 = 0\ntmax = 10\ndt = 0.1")
  tmp <- tempfile(fileext = ".R")
  on.exit(unlink(tmp))
  desolve_export(ir, tmp)
  expect_true(file.exists(tmp))
  lines <- readLines(tmp)
  expect_true(any(grepl("library(deSolve)", lines, fixed = TRUE)))
  expect_true(any(grepl("ode_fn", lines, fixed = TRUE)))
  expect_true(any(grepl("params", lines, fixed = TRUE)))
})

test_that("desolve_export script parses as valid R", {
  ir  <- parse_model("dX/dt = -k * X\nX[0] = 100\nk = 0.3\nt0 = 0\ntmax = 20\ndt = 0.1")
  tmp <- tempfile(fileext = ".R")
  on.exit(unlink(tmp))
  desolve_export(ir, tmp)
  expect_no_error(parse(tmp))
})

test_that("desolve_export script contains correct parameter and IC values", {
  ir  <- parse_model("dX/dt = -k * X\nX[0] = 100\nk = 0.3\nt0 = 0\ntmax = 20\ndt = 0.1")
  tmp <- tempfile(fileext = ".R")
  on.exit(unlink(tmp))
  desolve_export(ir, tmp)
  content <- paste(readLines(tmp), collapse = "\n")
  expect_true(grepl("0.3", content, fixed = TRUE))
  expect_true(grepl("100", content, fixed = TRUE))
  expect_true(grepl("-k * X", content, fixed = TRUE) || grepl("-k\\*X", content))
})

test_that("desolve_export script contains deSolve::ode() call with correct tmax", {
  ir  <- parse_model("dX/dt = 0\nX[0] = 5\nt0 = 0\ntmax = 77\ndt = 0.5")
  tmp <- tempfile(fileext = ".R")
  on.exit(unlink(tmp))
  desolve_export(ir, tmp)
  content <- paste(readLines(tmp), collapse = "\n")
  expect_true(grepl("77", content, fixed = TRUE))
  expect_true(grepl("deSolve::ode", content, fixed = TRUE))
})

test_that("desolve_export handles a two-state model correctly", {
  ir  <- parse_model(paste(
    "dX/dt = r * X * (1 - X / K)",
    "dY/dt = a * X * Y - b * Y",
    "X[0] = 10", "Y[0] = 2",
    "r = 1.2", "K = 100", "a = 0.01", "b = 0.5",
    "t0 = 0", "tmax = 50", "dt = 0.1",
    sep = "\n"
  ))
  tmp <- tempfile(fileext = ".R")
  on.exit(unlink(tmp))
  desolve_export(ir, tmp)
  expect_no_error(parse(tmp))
  content <- paste(readLines(tmp), collapse = "\n")
  expect_true(grepl("dX", content, fixed = TRUE))
  expect_true(grepl("dY", content, fixed = TRUE))
  expect_true(grepl("list(c(dX, dY))", content, fixed = TRUE))
})

test_that("desolve_export handles a model with no parameters", {
  # Model with numeric literal IC — no parameters at all
  ir  <- parse_model("dX/dt = -0.5 * X\nX[0] = 10\nt0 = 0\ntmax = 5\ndt = 0.1")
  tmp <- tempfile(fileext = ".R")
  on.exit(unlink(tmp))
  desolve_export(ir, tmp)
  expect_no_error(parse(tmp))
  content <- paste(readLines(tmp), collapse = "\n")
  expect_true(grepl("library(deSolve)", content, fixed = TRUE))
})

test_that("desolve_export returns an error for difference-equation (DDE) models", {
  ir <- parse_model("X[t+1] = X[t] * 1.1\nX[0] = 10\nt0 = 0\ntmax = 20\ndt = 1")
  tmp <- tempfile(fileext = ".R")
  on.exit(unlink(tmp))
  expect_error(desolve_export(ir, tmp), "ODE model")
})
