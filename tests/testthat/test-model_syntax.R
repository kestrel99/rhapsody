test_that("parse_model returns correct IR top-level structure", {
  ir <- parse_model("")
  expect_named(ir, c("meta", "type", "states", "parameters", "auxiliary",
                      "events", "time", "raw_source"))
})

test_that("parse_model preserves raw_source verbatim", {
  src <- "dX/dt = r * X\nX[0] = 1\nr = 1.0"
  ir <- parse_model(src)
  expect_identical(ir$raw_source, src)
})

test_that("parse_model identifies ODE state and expression", {
  ir <- parse_model("dX/dt = r * X")
  expect_equal(names(ir$states), "X")
  expect_equal(ir$states$X$ode_expr, "r * X")
})

test_that("parse_model sets default init_expr '0' when only ODE given", {
  ir <- parse_model("dX/dt = r * X")
  expect_equal(ir$states$X$init_expr, "0")
})

test_that("parse_model parses initial condition", {
  ir <- parse_model("dX/dt = r * X\nX[0] = 10")
  expect_equal(ir$states$X$init_expr, "10")
})

test_that("parse_model parses bare numeric parameter", {
  ir <- parse_model("r = 1.2")
  expect_equal(ir$parameters$r$value, 1.2)
  expect_true(is.na(ir$parameters$r$range_min))
  expect_false(ir$parameters$r$log_scale)
})

test_that("parse_model parses parameter with inline range", {
  ir <- parse_model("r = 1.2 # [0.1, 5]")
  expect_equal(ir$parameters$r$value, 1.2)
  expect_equal(ir$parameters$r$range_min, 0.1)
  expect_equal(ir$parameters$r$range_max, 5)
  expect_false(ir$parameters$r$log_scale)
})

test_that("parse_model parses log-scale parameter range", {
  ir <- parse_model("K = 100 # [10, 1000, log]")
  expect_true(ir$parameters$K$log_scale)
  expect_equal(ir$parameters$K$range_min, 10)
  expect_equal(ir$parameters$K$range_max, 1000)
})

test_that("parse_model parses time settings", {
  ir <- parse_model("t0 = 5\ntmax = 200\ndt = 0.5")
  expect_equal(ir$time$t0, 5)
  expect_equal(ir$time$tmax, 200)
  expect_equal(ir$time$dt, 0.5)
})

test_that("parse_model uses default time settings when absent", {
  ir <- parse_model("dX/dt = X")
  expect_equal(ir$time$t0, 0)
  expect_equal(ir$time$tmax, 100)
  expect_equal(ir$time$dt, 0.1)
})

test_that("parse_model skips full-line ## comments", {
  ir <- parse_model("## logistic growth model\ndX/dt = r * X")
  expect_equal(names(ir$states), "X")
})

test_that("parse_model strips trailing ## comment from ODE expression", {
  ir <- parse_model("dX/dt = r * X ## exponential growth")
  expect_equal(ir$states$X$ode_expr, "r * X")
})

test_that("parse_model handles Lotka-Volterra", {
  model <- paste(
    "dX/dt = r * X - a * X * Y",
    "dY/dt = b * X * Y - m * Y",
    "X[0] = 10",
    "Y[0] = 2",
    "r = 1.0",
    "a = 0.1",
    "b = 0.075",
    "m = 1.5",
    "tmax = 50",
    "dt = 0.1",
    sep = "\n"
  )
  ir <- parse_model(model)
  expect_equal(sort(names(ir$states)), c("X", "Y"))
  expect_equal(length(ir$parameters), 4)
  expect_equal(ir$time$tmax, 50)
  expect_equal(ir$type, "ode")
})

test_that("parse_model classifies unrecognised expr lines as auxiliary", {
  ir <- parse_model("dX/dt = X\ntotal = X + 1")
  expect_equal(ir$auxiliary$total, "X + 1")
})

test_that("parse_model strips trailing single # comment from ODE expression", {
  ir <- parse_model("dX/dt = r * X # growth term")
  expect_equal(ir$states$X$ode_expr, "r * X")
})

test_that("parse_model: X[t+1] line sets disc_expr and type='dde'", {
  ir <- parse_model("X[t+1] = 1.1 * X[t]\nX[0] = 10")
  expect_equal(ir$type, "dde")
  expect_equal(ir$states$X$disc_expr, "1.1 * X")
  expect_null(ir$states$X$ode_expr)
})

test_that("parse_model: substitutes all X[t] refs in disc_expr", {
  ir <- parse_model("X[t+1] = r * X[t] * (1 - X[t] / K)\nr = 1.2\nK = 100\nX[0] = 10")
  expect_equal(ir$states$X$disc_expr, "r * X * (1 - X / K)")
})

test_that("parse_model: multiple DDE states all get disc_expr", {
  ir <- parse_model("X[t+1] = 1.1 * X[t]\nY[t+1] = 0.9 * Y[t]\nX[0] = 5\nY[0] = 3")
  expect_equal(ir$type, "dde")
  expect_false(is.null(ir$states$X$disc_expr))
  expect_false(is.null(ir$states$Y$disc_expr))
})

test_that("parse_model: mixed ODE + DDE sets type='mixed'", {
  ir <- parse_model("dX/dt = X\nY[t+1] = 1.1 * Y[t]")
  expect_equal(ir$type, "mixed")
})

test_that("parse_model: time-based event is parsed into ir$events", {
  ir <- parse_model("dX/dt = 0\nX[0] = 10\nat(t == 50): X = X + 20")
  expect_length(ir$events, 1L)
  ev <- ir$events[[1]]
  expect_equal(ev$type, "time")
  expect_equal(ev$time, 50)
  expect_equal(ev$var,  "X")
  expect_equal(ev$expr, "X + 20")
})

test_that("parse_model: multiple events are all captured in order", {
  m <- "dX/dt = 0\nX[0] = 10\nat(t == 20): X = 5\nat(t == 80): X = X * 2"
  ir <- parse_model(m)
  expect_length(ir$events, 2L)
  expect_equal(ir$events[[2]]$time, 80)
  expect_equal(ir$events[[2]]$expr, "X * 2")
})
