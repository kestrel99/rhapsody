test_that("session_to_json produces valid JSON with required fields", {
  json_str <- session_to_json(
    model  = "dX/dt = 0\nX[0] = 1",
    params = list(r = 1.2),
    ics    = list(X = 5),
    solver = list(method = "lsoda", atol = 1e-6, rtol = 1e-6,
                  t0 = 0, tmax = 100, dt = 0.1)
  )
  expect_type(json_str, "character")
  parsed <- jsonlite::fromJSON(json_str, simplifyVector = FALSE)
  expect_true(all(c("rhapsody_version", "model", "params", "ics", "solver") %in% names(parsed)))
  expect_equal(parsed$model, "dX/dt = 0\nX[0] = 1")
  expect_equal(parsed$params$r, 1.2)
})

test_that("session_from_json round-trips a session_to_json output", {
  original_model  <- "dX/dt = r * X\nX[0] = 1\nr = 0.5"
  original_params <- list(r = 0.5)
  original_ics    <- list(X = 10)
  original_solver <- list(method = "rk4", atol = 1e-8, rtol = 1e-8,
                          t0 = 0, tmax = 50, dt = 0.5)

  json_str <- session_to_json(
    model  = original_model,
    params = original_params,
    ics    = original_ics,
    solver = original_solver
  )

  tmp <- tempfile(fileext = ".rhy")
  writeLines(json_str, tmp)
  on.exit(unlink(tmp))

  loaded <- session_from_json(tmp)
  expect_false(inherits(loaded, "rhapsody_error"))
  expect_equal(loaded$model, original_model)
  expect_equal(loaded$params$r, 0.5)
  expect_equal(loaded$ics$X, 10)
  expect_equal(loaded$solver$method, "rk4")
  expect_equal(loaded$solver$tmax, 50)
})

test_that("session_from_json returns rhapsody_error for malformed JSON", {
  tmp <- tempfile(fileext = ".rhy")
  writeLines("this is not json {{{", tmp)
  on.exit(unlink(tmp))
  result <- session_from_json(tmp)
  expect_s3_class(result, "rhapsody_error")
})

test_that("session_from_json returns rhapsody_error for missing required fields", {
  tmp <- tempfile(fileext = ".rhy")
  writeLines('{"model": "dX/dt = 0"}', tmp)  # missing params, ics, solver
  on.exit(unlink(tmp))
  result <- session_from_json(tmp)
  expect_s3_class(result, "rhapsody_error")
})
