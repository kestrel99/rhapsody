test_that("solve_discrete returns data.frame with time and state columns", {
  ir <- parse_model("X[t+1] = 1.1 * X[t]\nX[0] = 1\ntmax = 3\ndt = 1")
  result <- solve_discrete(ir)
  expect_s3_class(result, "data.frame")
  expect_true(all(c("time", "X") %in% names(result)))
  expect_equal(nrow(result), 4L)   # t = 0, 1, 2, 3
})

test_that("solve_discrete: geometric growth is exact", {
  ir <- parse_model("X[t+1] = 2 * X[t]\nX[0] = 1\ntmax = 4\ndt = 1")
  result <- solve_discrete(ir)
  expect_true(all(abs(result$X - 2^result$time) < 1e-9))
})

test_that("solve_discrete: ics override replaces IR initial condition and propagates", {
  ir <- parse_model("X[t+1] = 2 * X[t]\nX[0] = 1\ntmax = 2\ndt = 1")
  result <- solve_discrete(ir, ics = list(X = 5))
  # X doubles each step starting from 5: 5, 10, 20
  expect_equal(result$X[1], 5)
  expect_true(all(abs(result$X - 5 * 2^result$time) < 1e-9))
})

test_that("solve_discrete: auxiliary columns included in output", {
  ir <- parse_model("X[t+1] = 2 * X[t]\nX[0] = 1\ntmax = 2\ndt = 1\ndoubled = X * 2")
  result <- solve_discrete(ir)
  expect_true("doubled" %in% names(result))
  expect_true(all(abs(result$doubled - result$X * 2) < 1e-9))
})

test_that("solve_discrete: tmax override shortens the simulation", {
  ir <- parse_model("X[t+1] = X[t] * 1.1\nX[0] = 10\nt0 = 0\ntmax = 100\ndt = 1")
  result <- solve_discrete(ir, tmax = 5)
  expect_equal(max(result$time), 5)
})

test_that("solve_discrete: t0 override shifts the start time", {
  ir <- parse_model("X[t+1] = X[t] * 1.1\nX[0] = 10\nt0 = 0\ntmax = 10\ndt = 1")
  result <- solve_discrete(ir, t0 = 3, tmax = 6, dt = 1)
  expect_equal(min(result$time), 3)
  expect_equal(max(result$time), 6)
})
