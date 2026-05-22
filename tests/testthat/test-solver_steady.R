test_that("solve_steady: trivial flat system stays at IC", {
  # dX/dt = 0 → every value is a SS; runsteady returns starting point
  ir <- parse_model("dX/dt = 0\nX[0] = 5\nt0=0\ntmax=100\ndt=1")
  result <- solve_steady(ir)
  expect_false(inherits(result, "rhapsody_error"))
  expect_named(result, "X")
  expect_equal(as.numeric(result["X"]), 5, tolerance = 1e-4)
})

test_that("solve_steady: damped linear system converges to a=10", {
  # dX/dt = -X + a → SS at X = a
  ir <- parse_model("dX/dt = -X + a\nX[0]=1\na=10\nt0=0\ntmax=100\ndt=0.1")
  result <- solve_steady(ir)
  expect_false(inherits(result, "rhapsody_error"))
  expect_equal(as.numeric(result["X"]), 10, tolerance = 1e-4)
})

test_that("solve_steady: params override shifts SS", {
  ir <- parse_model("dX/dt = -X + a\nX[0]=1\na=10\nt0=0\ntmax=100\ndt=0.1")
  result <- solve_steady(ir, params = list(a = 25))
  expect_equal(as.numeric(result["X"]), 25, tolerance = 1e-4)
})

test_that("solve_steady: stode method matches runsteady for damped linear", {
  ir <- parse_model("dX/dt = -X + a\nX[0]=1\na=10\nt0=0\ntmax=100\ndt=0.1")
  result <- solve_steady(ir, method = "stode")
  expect_false(inherits(result, "rhapsody_error"))
  expect_equal(as.numeric(result["X"]), 10, tolerance = 1e-4)
})
