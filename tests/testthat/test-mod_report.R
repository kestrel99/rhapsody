test_that("desolve_export produces a syntactically valid R script (shared engine for report R script)", {
  ir  <- parse_model("dX/dt = -k * X\nX[0] = 50\nk = 0.2\nt0 = 0\ntmax = 10\ndt = 0.1")
  tmp <- tempfile(fileext = ".R")
  on.exit(unlink(tmp))
  desolve_export(ir, tmp)
  expect_no_error(parse(tmp))
  content <- paste(readLines(tmp), collapse = "\n")
  expect_true(grepl("library(deSolve)", content, fixed = TRUE))
  expect_true(grepl("deSolve::ode", content, fixed = TRUE))
})

test_that("report.Rmd template file exists in inst/templates/", {
  template <- system.file("templates", "report.Rmd", package = "rhapsody")
  expect_true(nzchar(template))
  expect_true(file.exists(template))
})

test_that("report.Rmd renders to HTML without error", {
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")
  skip_if_not_installed("ggplot2")
  template <- system.file("templates", "report.Rmd", package = "rhapsody")
  skip_if(!file.exists(template), "report.Rmd not found")

  ir  <- parse_model("dX/dt = r * X\nX[0] = 1\nr = 0.5\nt0 = 0\ntmax = 5\ndt = 0.1")
  sim <- as.data.frame(solve_ode(ir))
  params_df <- data.frame(
    name  = "r",
    value = 0.5,
    min   = NA_real_,
    max   = NA_real_,
    stringsAsFactors = FALSE
  )
  out <- tempfile(fileext = ".html")
  on.exit(unlink(out))

  expect_no_error(
    rmarkdown::render(
      input         = template,
      output_format = "html_document",
      output_file   = out,
      params        = list(
        report_title  = "Test Report",
        report_author = "Tester",
        report_date   = "2026-05-25",
        model_source  = ir$raw_source,
        params_df     = params_df,
        sim_data      = sim,
        sections      = c("model", "params", "plot", "table")
      ),
      envir = new.env(parent = globalenv()),
      quiet = TRUE
    )
  )
  expect_true(file.exists(out))
  content <- paste(readLines(out), collapse = "\n")
  expect_true(nzchar(content))
})

test_that("report.Rmd renders to Word (.docx) without error", {
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")
  template <- system.file("templates", "report.Rmd", package = "rhapsody")
  skip_if(!file.exists(template), "report.Rmd not found")

  ir  <- parse_model("dX/dt = -X\nX[0] = 5\nt0 = 0\ntmax = 5\ndt = 0.5")
  sim <- as.data.frame(solve_ode(ir))
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out))

  expect_no_error(
    rmarkdown::render(
      input         = template,
      output_format = "word_document",
      output_file   = out,
      params        = list(
        report_title  = "Word Test",
        report_author = "",
        report_date   = "2026-05-25",
        model_source  = ir$raw_source,
        params_df     = data.frame(name = character(0), value = numeric(0),
                                   min  = numeric(0),   max   = numeric(0)),
        sim_data      = sim,
        sections      = c("model", "plot")
      ),
      envir = new.env(parent = globalenv()),
      quiet = TRUE
    )
  )
  expect_true(file.exists(out))
})
