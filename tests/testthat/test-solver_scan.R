test_that("scan_params: returns list of length == steps", {
  ir  <- parse_model("dX/dt = r*X\nX[0]=1\nr=1\nt0=0\ntmax=1\ndt=0.1")
  out <- scan_params(ir, scan_spec = list(parameter="r", from=0.5, to=2, steps=5, log_scale=FALSE))
  expect_length(out$results,      5L)
  expect_length(out$param_values, 5L)
  expect_equal(out$param_name, "r")
})

test_that("scan_params: each result is a data.frame with time and X", {
  ir  <- parse_model("dX/dt = r*X\nX[0]=1\nr=1\nt0=0\ntmax=1\ndt=0.1")
  out <- scan_params(ir, scan_spec = list(parameter="r", from=0.5, to=2, steps=3, log_scale=FALSE))
  for (res in out$results) {
    expect_s3_class(res, "data.frame")
    expect_true("time" %in% names(res))
    expect_true("X"    %in% names(res))
  }
})

test_that("scan_params: higher r produces larger final X at t=1", {
  # dX/dt = r*X, X(t)=exp(r*t); final X increases with r
  ir  <- parse_model("dX/dt = r*X\nX[0]=1\nr=1\nt0=0\ntmax=1\ndt=0.01")
  out <- scan_params(ir, scan_spec = list(parameter="r", from=0.1, to=2, steps=3, log_scale=FALSE))
  final_X <- vapply(out$results, function(df) tail(df$X, 1L), numeric(1L))
  expect_true(all(diff(final_X) > 0))
})

test_that("scan_params: log_scale=TRUE uses geometric spacing", {
  ir  <- parse_model("dX/dt = r*X\nX[0]=1\nr=1\nt0=0\ntmax=1\ndt=0.1")
  out <- scan_params(ir, scan_spec = list(parameter="r", from=1, to=100, steps=3, log_scale=TRUE))
  expect_equal(out$param_values, c(1, 10, 100), tolerance = 1e-6)
})

test_that("scan_params: log_scale with non-positive from raises an error", {
  ir <- parse_model("dX/dt = r*X\nX[0]=1\nr=1\nt0=0\ntmax=1\ndt=0.1")
  expect_error(
    scan_params(ir, scan_spec = list(parameter="r", from=-1, to=2, steps=3, log_scale=TRUE)),
    "Log scale requires strictly positive"
  )
  expect_error(
    scan_params(ir, scan_spec = list(parameter="r", from=0, to=2, steps=3, log_scale=TRUE)),
    "Log scale requires strictly positive"
  )
})
