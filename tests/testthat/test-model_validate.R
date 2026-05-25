test_that("validate_model returns list with errors and warnings keys", {
  ir <- parse_model("dX/dt = X\nX[0] = 1")
  result <- validate_model(ir)
  expect_named(result, c("errors", "warnings"))
})

test_that("validate_model: no errors for a complete valid model", {
  ir <- parse_model(paste(
    "dX/dt = r * X",
    "X[0] = 1",
    "r = 1.2",
    "t0 = 0",
    "tmax = 10",
    "dt = 0.1",
    sep = "\n"
  ))
  result <- validate_model(ir)
  expect_equal(length(result$errors), 0L)
})

test_that("validate_model: error when no states defined", {
  ir <- parse_model("r = 1.2")
  result <- validate_model(ir)
  expect_true(length(result$errors) > 0L)
  expect_match(result$errors[1L], "No equations found", ignore.case = TRUE)
})

test_that("validate_model: error when state has IC but no ODE equation", {
  # X[0] = 10 with no dX/dt line — parser adds X to states with init_expr but ode_expr = NULL
  model <- "X[0] = 10\nr = 1.2"
  ir <- parse_model(model)
  result <- validate_model(ir)
  expect_true(any(grepl("X", result$errors)))
})

test_that("validate_model: error on non-positive dt", {
  ir <- parse_model("dX/dt = X\nX[0] = 1")
  ir$time$dt <- -0.1
  result <- validate_model(ir)
  expect_true(any(grepl("dt", result$errors)))
})

test_that("validate_model: error when tmax <= t0", {
  ir <- parse_model("dX/dt = X\nX[0] = 1\nt0 = 10\ntmax = 5\ndt = 0.1")
  result <- validate_model(ir)
  expect_true(any(grepl("tmax", result$errors, ignore.case = TRUE)))
})

test_that("validate_model: DDE state with disc_expr is valid", {
  ir <- parse_model("X[t+1] = 1.1 * X[t]\nX[0] = 1\ntmax = 10\ndt = 1")
  errs <- validate_model(ir)
  expect_length(errs$errors, 0L)
})

test_that("validate_model: mixed type is an error", {
  ir <- parse_model("dX/dt = X\nY[t+1] = 1.1 * Y[t]")
  errs <- validate_model(ir)
  expect_true(any(grepl("Mixed", errs$errors, ignore.case = TRUE)))
})

test_that("validate_model: state event targeting a parameter is an error", {
  # 'r' is a parameter, not a state — assigning to it in a state event is invalid
  ir <- parse_model("dX/dt = r * X\nX[0] = 1\nr = 1.0\nat(X > 80): r = 0")
  result <- validate_model(ir)
  expect_true(length(result$errors) > 0L)
  expect_true(any(grepl("r", result$errors)))
})

test_that("validate_model: state event with unknown trigger state is an error", {
  # 'Z' is not a state variable
  ir <- parse_model("dX/dt = 1\nX[0] = 0\nat(Z > 50): X = 0")
  result <- validate_model(ir)
  expect_true(length(result$errors) > 0L)
  expect_true(any(grepl("Z", result$errors)))
})

test_that("validate_model: valid state event produces no errors", {
  ir <- parse_model("dX/dt = 1\nX[0] = 0\nat(X > 80): X = 0")
  result <- validate_model(ir)
  expect_length(result$errors, 0L)
})

test_that("validate_model: combining time and state events is an error", {
  ir <- parse_model("dX/dt = 1\nX[0] = 0\nat(t == 10): X = 0\nat(X > 80): X = 0")
  result <- validate_model(ir)
  expect_true(length(result$errors) > 0L)
  expect_true(any(grepl("not currently supported", result$errors)))
})

test_that("validate_model: events in DDE model is an error", {
  ir <- parse_model("X[t+1] = 1.1 * X[t]\nX[0] = 1\ntmax = 20\ndt = 1\nat(X > 3): X = 0")
  result <- validate_model(ir)
  expect_true(length(result$errors) > 0L)
  expect_true(any(grepl("difference-equation", result$errors)))
})
