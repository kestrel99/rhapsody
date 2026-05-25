test_that("rhy_import extracts IR from a full session .rhy file", {
  model_src <- "dX/dt = r * X\nX[0] = 1\nr = 0.5\nt0 = 0\ntmax = 10\ndt = 0.1"
  json_str  <- session_to_json(
    model  = model_src,
    params = list(r = 0.5),
    ics    = list(X = 1),
    solver = list(method = "lsoda", t0 = 0, tmax = 10, dt = 0.1, atol = 1e-6, rtol = 1e-6)
  )
  tmp <- tempfile(fileext = ".rhy")
  writeLines(json_str, tmp)
  on.exit(unlink(tmp))
  ir <- rhy_import(tmp)
  expect_false(inherits(ir, "rhapsody_error"))
  expect_true("X" %in% names(ir$states))
  expect_equal(ir$parameters$r$value, 0.5)
})

test_that("rhy_import returns rhapsody_error for malformed JSON", {
  tmp <- tempfile(fileext = ".rhy")
  writeLines("not json {{{", tmp)
  on.exit(unlink(tmp))
  result <- rhy_import(tmp)
  expect_s3_class(result, "rhapsody_error")
})

test_that("rhy_import returns rhapsody_error when model field is missing", {
  tmp <- tempfile(fileext = ".rhy")
  writeLines('{"rhapsody_version": "0.1.0"}', tmp)
  on.exit(unlink(tmp))
  result <- rhy_import(tmp)
  expect_s3_class(result, "rhapsody_error")
})

test_that("rhy_export writes a JSON file with model and rhapsody_version fields", {
  ir  <- parse_model("dX/dt = r * X\nX[0] = 1\nr = 0.5\nt0 = 0\ntmax = 10\ndt = 0.1")
  tmp <- tempfile(fileext = ".rhy")
  on.exit(unlink(tmp))
  rhy_export(ir, tmp)
  expect_true(file.exists(tmp))
  raw <- jsonlite::fromJSON(tmp, simplifyVector = FALSE)
  expect_true("model" %in% names(raw))
  expect_true("rhapsody_version" %in% names(raw))
  expect_true(nzchar(raw$model))
})

test_that("rhy_export -> rhy_import round-trip preserves states and parameters", {
  original_src <- "dX/dt = r * X * (1 - X / K)\nX[0] = 10\nr = 1.2\nK = 100\nt0 = 0\ntmax = 50\ndt = 0.1"
  ir  <- parse_model(original_src)
  tmp <- tempfile(fileext = ".rhy")
  on.exit(unlink(tmp))
  rhy_export(ir, tmp)
  ir2 <- rhy_import(tmp)
  expect_false(inherits(ir2, "rhapsody_error"))
  expect_equal(names(ir2$states), names(ir$states))
  expect_equal(names(ir2$parameters), names(ir$parameters))
  expect_equal(ir2$parameters$r$value, 1.2)
})
