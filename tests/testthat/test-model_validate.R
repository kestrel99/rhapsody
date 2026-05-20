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
  expect_match(result$errors[1L], "No ODE", ignore.case = TRUE)
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
