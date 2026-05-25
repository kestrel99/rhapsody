test_that("solve_ode returns a data.frame with a time column", {
  ir <- parse_model("dX/dt = 0\nX[0] = 5\nt0 = 0\ntmax = 10\ndt = 1")
  result <- solve_ode(ir)
  expect_s3_class(result, "data.frame")
  expect_true("time" %in% names(result))
  expect_true("X" %in% names(result))
})

test_that("solve_ode: constant system stays at initial condition", {
  # dX/dt = 0, X[0] = 5  ->  X(t) = 5 for all t
  ir <- parse_model("dX/dt = 0\nX[0] = 5\nt0 = 0\ntmax = 10\ndt = 1")
  result <- solve_ode(ir)
  expect_true(all(abs(result$X - 5) < 1e-6))
})

test_that("solve_ode: exponential growth matches e^t at t=1", {
  # dX/dt = X, X[0] = 1  ->  X(1) ~= e = 2.71828
  ir <- parse_model("dX/dt = X\nX[0] = 1\nt0 = 0\ntmax = 1\ndt = 0.01")
  result <- solve_ode(ir)
  final_X <- tail(result$X, 1L)
  expect_true(abs(final_X - exp(1)) < 0.01)
})

test_that("solve_ode: params argument overrides IR parameter values", {
  # r = 0 -> flat; override r = 1 -> X grows to ~e at t=1
  ir <- parse_model("dX/dt = r * X\nX[0] = 1\nr = 0\nt0 = 0\ntmax = 1\ndt = 0.01")
  flat    <- solve_ode(ir)
  growing <- solve_ode(ir, params = list(r = 1))
  expect_true(tail(flat$X,    1L) < 1.001)
  expect_true(tail(growing$X, 1L) > 2)
})

test_that("solve_ode: number of rows equals length of time sequence", {
  ir <- parse_model("dX/dt = 0\nX[0] = 1\nt0 = 0\ntmax = 10\ndt = 1")
  result <- solve_ode(ir)
  expected_rows <- length(seq(0, 10, by = 1))
  expect_equal(nrow(result), expected_rows)
})

test_that("solve_ode handles two-state Lotka-Volterra without crashing", {
  model <- paste(
    "dX/dt = r * X - a * X * Y",
    "dY/dt = b * X * Y - m * Y",
    "X[0] = 10",
    "Y[0] = 2",
    "r = 1.0",
    "a = 0.1",
    "b = 0.075",
    "m = 1.5",
    "tmax = 30",
    "dt = 0.1",
    sep = "\n"
  )
  ir <- parse_model(model)
  result <- solve_ode(ir)
  # Lotka-Volterra oscillates; both should remain positive
  expect_true(all(result$X > 0))
  expect_true(all(result$Y > 0))
  expect_true(all(c("time", "X", "Y") %in% names(result)))
})

test_that("solve_ode includes auxiliary variable columns in output", {
  model <- paste(
    "dX/dt = 0",
    "X[0] = 5",
    "tmax = 2",
    "dt = 1",
    "doubled = X * 2",
    sep = "\n"
  )
  ir     <- parse_model(model)
  result <- solve_ode(ir)
  expect_true("doubled" %in% names(result))
  # X stays at 5, so doubled = 10 at every row
  expect_true(all(abs(result$doubled - 10) < 1e-6))
})

test_that("solve_ode auxiliary uses current state values (not derivatives)", {
  # dX/dt = X means X grows; aux = X + 1 should track X
  ir     <- parse_model("dX/dt = X\nX[0] = 1\ntmax = 1\ndt = 0.5\nxp1 = X + 1")
  result <- solve_ode(ir)
  # At t=0, X≈1, xp1≈2; at later rows xp1 = X + 1 must hold exactly
  expect_true(all(abs(result$xp1 - (result$X + 1)) < 1e-6))
})

test_that("solve_ode: ics argument overrides IR initial conditions", {
  # IR has X[0] = 1; pass ics = list(X = 10)
  ir           <- parse_model("dX/dt = 0\nX[0] = 1\ntmax = 2\ndt = 1")
  default_run  <- solve_ode(ir)
  override_run <- solve_ode(ir, ics = list(X = 10))
  expect_true(abs(default_run$X[1]  -  1) < 1e-6)
  expect_true(abs(override_run$X[1] - 10) < 1e-6)
  # With dX/dt = 0 the IC is preserved throughout
  expect_true(all(abs(override_run$X - 10) < 1e-6))
})

test_that("solve_ode applies time-based event: X = X + 20 at t=50", {
  m <- paste(
    "dX/dt = 0",
    "X[0] = 10",
    "tmax = 100",
    "dt   = 10",
    "at(t == 50): X = X + 20",
    sep = "\n"
  )
  ir     <- parse_model(m)
  result <- solve_ode(ir)
  before <- result$X[result$time < 50]
  after  <- result$X[result$time > 50]
  expect_true(all(abs(before - 10) < 1e-6))
  expect_true(all(abs(after  - 30) < 1e-6))
})

test_that("solve_ode applies assignment event: X = 99 at t=5", {
  m <- "dX/dt = 0\nX[0] = 10\ntmax = 10\ndt = 1\nat(t == 5): X = 99"
  ir     <- parse_model(m)
  result <- solve_ode(ir)
  after  <- result$X[result$time > 5]
  expect_true(all(abs(after - 99) < 1e-6))
})

test_that("solve_ode: t0/tmax/dt arguments override IR time settings", {
  # IR says tmax=100, dt=1; overrides say tmax=5, dt=1
  ir <- parse_model("dX/dt = 0\nX[0] = 7\nt0 = 0\ntmax = 100\ndt = 1")
  result <- solve_ode(ir, tmax = 5, dt = 1)
  expect_equal(max(result$time), 5)
  expect_equal(min(result$time), 0)
  expect_true(all(abs(result$X - 7) < 1e-6))
})

test_that("solve_ode: t0 override shifts the start time", {
  ir <- parse_model("dX/dt = 0\nX[0] = 3\nt0 = 0\ntmax = 10\ndt = 1")
  result <- solve_ode(ir, t0 = 2, tmax = 5, dt = 1)
  expect_equal(min(result$time), 2)
  expect_equal(max(result$time), 5)
})

test_that("solve_ode applies state-based event: reset X to 0 when X > 80", {
  # dX/dt = 1 — linear growth; state event resets X to 0 when X crosses 80
  # With tmax=250 and dt=1 we expect several resets
  m <- paste(
    "dX/dt = 1",
    "X[0] = 0",
    "tmax = 250",
    "dt   = 1",
    "at(X > 80): X = 0",
    sep = "\n"
  )
  ir     <- parse_model(m)
  result <- solve_ode(ir)
  # X should never exceed 80 (within solver tolerance)
  expect_true(max(result$X) <= 80 + 1e-3)
  # and should have been reset at least twice (hit 80 at t≈80, t≈160, t≈240)
  n_resets <- sum(diff(result$X) < -70)
  expect_true(n_resets >= 2L)
})

test_that("solve_ode: state event with < comparator fires when state falls below threshold", {
  # dX/dt = -1 — linear decay; fires X = 100 when X < 10
  m <- paste(
    "dX/dt = -1",
    "X[0] = 100",
    "tmax = 350",
    "dt   = 1",
    "at(X < 10): X = 100",
    sep = "\n"
  )
  ir     <- parse_model(m)
  result <- solve_ode(ir)
  expect_true(min(result$X) >= 9.9)
  n_resets <- sum(diff(result$X) > 70)
  expect_true(n_resets >= 2L)
})

test_that("solve_ode: two state events — only the triggered one fires", {
  # X grows (dX/dt = 1), Y is constant (dY/dt = 0)
  # at(X > 50): X = 0 should fire; at(Y < 10): Y = 99 should NOT fire (Y stays at 50)
  m <- paste(
    "dX/dt = 1",
    "dY/dt = 0",
    "X[0] = 0",
    "Y[0] = 50",
    "tmax = 200",
    "dt   = 1",
    "at(X > 50): X = 0",
    "at(Y < 10): Y = 99",
    sep = "\n"
  )
  ir     <- parse_model(m)
  result <- solve_ode(ir)
  # Y should stay at 50 throughout — the Y event should never fire
  expect_true(all(abs(result$Y - 50) < 1e-6))
  # X should reset multiple times
  n_resets <- sum(diff(result$X) < -40)
  expect_true(n_resets >= 2L)
})

test_that("solve_ode: state event < fires on downward crossing only", {
  # X decays (dX/dt = -1), state event fires when X < 10, resets X to 100
  m <- paste(
    "dX/dt = -1",
    "X[0] = 100",
    "tmax = 300",
    "dt   = 1",
    "at(X < 10): X = 100",
    sep = "\n"
  )
  ir     <- parse_model(m)
  result <- solve_ode(ir)
  # X should never drop below 10 (within tolerance)
  expect_true(min(result$X) >= 9.5)
  # Should reset multiple times
  n_resets <- sum(diff(result$X) > 50)
  expect_true(n_resets >= 2L)
})
