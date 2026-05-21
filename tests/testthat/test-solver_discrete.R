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

test_that("solve_discrete: ics override replaces IR initial condition", {
  ir <- parse_model("X[t+1] = 2 * X[t]\nX[0] = 1\ntmax = 2\ndt = 1")
  result <- solve_discrete(ir, ics = list(X = 5))
  expect_equal(result$X[1], 5)
})

test_that("solve_discrete: auxiliary columns included in output", {
  ir <- parse_model("X[t+1] = 2 * X[t]\nX[0] = 1\ntmax = 2\ndt = 1\ndoubled = X * 2")
  result <- solve_discrete(ir)
  expect_true("doubled" %in% names(result))
  expect_true(all(abs(result$doubled - result$X * 2) < 1e-9))
})
