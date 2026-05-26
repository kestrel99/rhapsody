test_that("mmd_import parses d/dt notation, INIT, STARTTIME, STOPTIME, DT, and parameters", {
  mmd_text <- c(
    "STARTTIME = 0",
    "STOPTIME  = 50",
    "DT        = 0.1",
    "d/dt(X) = r * X * (1 - X / K)",
    "d/dt(Y) = a * X * Y - b * Y",
    "INIT X = 10",
    "INIT Y = 2",
    "r = 1.2",
    "K = 100",
    "a = 0.01",
    "b = 0.5"
  )
  tmp <- tempfile(fileext = ".mmd")
  on.exit(unlink(tmp))
  writeLines(mmd_text, tmp)
  ir <- mmd_import(tmp)
  expect_false(inherits(ir, "rhapsody_error"))
  expect_true("X" %in% names(ir$states))
  expect_true("Y" %in% names(ir$states))
  expect_equal(ir$states$X$init_expr, "10")
  expect_equal(ir$states$Y$init_expr, "2")
  expect_equal(ir$time$t0,   0)
  expect_equal(ir$time$tmax, 50)
  expect_equal(ir$time$dt,   0.1)
  expect_equal(ir$parameters$r$value, 1.2)
  expect_equal(ir$parameters$K$value, 100)
})

test_that("mmd_import strips block comments {..} and ; line comments", {
  mmd_text <- c(
    "{ This is a BM block comment }",
    "STARTTIME = 0 ; inline comment",
    "STOPTIME = 10",
    "DT = 0.1",
    "d/dt(X) = -X  { decay }",
    "INIT X = 5"
  )
  tmp <- tempfile(fileext = ".mmd")
  on.exit(unlink(tmp))
  writeLines(mmd_text, tmp)
  ir <- mmd_import(tmp)
  expect_false(inherits(ir, "rhapsody_error"))
  expect_true("X" %in% names(ir$states))
  expect_equal(ir$time$t0, 0)
})

test_that("mmd_import attaches import_warnings for unsupported BM keywords", {
  mmd_text <- c(
    "STARTTIME = 0", "STOPTIME = 10", "DT = 0.1",
    "d/dt(X) = -X",
    "INIT X = 1",
    "METHOD RK4",
    "GRAPH X"
  )
  tmp <- tempfile(fileext = ".mmd")
  on.exit(unlink(tmp))
  writeLines(mmd_text, tmp)
  ir <- mmd_import(tmp)
  expect_false(inherits(ir, "rhapsody_error"))
  w <- attr(ir, "import_warnings")
  expect_true(!is.null(w) && length(w) > 0L)
})

test_that("mmd_import returns rhapsody_error if no ODE equations found", {
  mmd_text <- c("STARTTIME = 0", "STOPTIME = 10", "DT = 0.1", "r = 1.2")
  tmp <- tempfile(fileext = ".mmd")
  on.exit(unlink(tmp))
  writeLines(mmd_text, tmp)
  result <- mmd_import(tmp)
  expect_s3_class(result, "rhapsody_error")
})

test_that("mmd_export writes BM syntax with d/dt, INIT, STARTTIME, STOPTIME, DT", {
  tmp <- tempfile(fileext = ".mmd")
  on.exit(unlink(tmp))
  ir  <- parse_model("dX/dt = r * X\nX[0] = 10\nr = 1.2\nt0 = 0\ntmax = 50\ndt = 0.1")
  mmd_export(ir, tmp)
  expect_true(file.exists(tmp))
  lines <- readLines(tmp)
  expect_true(any(grepl("d/dt(X)", lines, fixed = TRUE)))
  expect_true(any(grepl("INIT X", lines, fixed = TRUE)))
  expect_true(any(grepl("STARTTIME", lines)))
  expect_true(any(grepl("STOPTIME", lines)))
  stoptime_line <- lines[grepl("^STOPTIME", lines)]
  expect_true(length(stoptime_line) > 0L && any(grepl("50", stoptime_line, fixed = TRUE)))
})

test_that("mmd_export -> mmd_import round-trip preserves states, params, and time", {
  tmp <- tempfile(fileext = ".mmd")
  on.exit(unlink(tmp))
  ir <- parse_model(paste(
    "dX/dt = r * X * (1 - X / K)",
    "X[0] = 10",
    "r = 1.2",
    "K = 100",
    "t0 = 0",
    "tmax = 50",
    "dt = 0.1",
    sep = "\n"
  ))
  mmd_export(ir, tmp)
  ir2 <- mmd_import(tmp)
  expect_false(inherits(ir2, "rhapsody_error"))
  expect_equal(names(ir2$states), names(ir$states))
  expect_equal(ir2$parameters$r$value, 1.2)
  expect_equal(ir2$parameters$K$value, 100)
  expect_equal(ir2$time$tmax, 50)
})

test_that("mmd_import strips multiline block comments spanning multiple lines", {
  mmd_text <- c(
    "{ This comment",
    "  spans multiple",
    "  lines }",
    "STARTTIME = 0",
    "STOPTIME = 10",
    "DT = 0.1",
    "d/dt(X) = -X",
    "INIT X = 3"
  )
  tmp <- tempfile(fileext = ".mmd")
  on.exit(unlink(tmp))
  writeLines(mmd_text, tmp)
  ir <- mmd_import(tmp)
  expect_false(inherits(ir, "rhapsody_error"))
  expect_true("X" %in% names(ir$states))
  expect_equal(ir$time$t0, 0)
})
