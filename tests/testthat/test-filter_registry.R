test_that("register_filter stores a filter and filter_for_ext retrieves it", {
  dummy_import <- function(path) list()
  dummy_export <- function(ir, path) invisible(NULL)
  register_filter("t1fmt", "T1 Format", ".t1ext99",
                  import = dummy_import, export = dummy_export)
  result <- filter_for_ext(".t1ext99")
  expect_equal(length(result), 1L)
  filt <- result[[1L]]
  expect_equal(filt$name, "t1fmt")
  expect_equal(filt$label, "T1 Format")
  expect_equal(filt$ext, ".t1ext99")
  expect_true(filt$can_import)
  expect_true(filt$can_export)
})

test_that("export-only filter has can_import = FALSE", {
  register_filter("t1exp", "Export Only", ".t1xo99",
                  export = function(ir, path) invisible(NULL))
  result <- filter_for_ext(".t1xo99")
  expect_equal(length(result), 1L)
  expect_false(result[[1L]]$can_import)
  expect_true(result[[1L]]$can_export)
})

test_that("get_importable_filters returns only can_import filters", {
  register_filter("t1imp", "Import Only", ".t1im99",
                  import = function(path) list())
  importable <- get_importable_filters()
  expect_true(all(vapply(importable, `[[`, logical(1L), "can_import")))
})

test_that("get_exportable_filters returns only can_export filters", {
  exportable <- get_exportable_filters()
  expect_true(all(vapply(exportable, `[[`, logical(1L), "can_export")))
})

test_that("filter_for_ext returns empty list for unknown extension", {
  result <- filter_for_ext(".zzznomatch999")
  expect_equal(length(result), 0L)
})

test_that("filter_for_ext matches one of multiple extensions", {
  register_filter("t1multi", "Multi Ext", c(".t1ma99", ".t1mb99"),
                  import = function(path) list())
  expect_equal(length(filter_for_ext(".t1ma99")), 1L)
  expect_equal(length(filter_for_ext(".t1mb99")), 1L)
  expect_equal(filter_for_ext(".t1ma99")[[1L]]$name, "t1multi")
})

test_that("filter_for_ext returns multiple filters when two filters share an extension", {
  register_filter("t1shared_a", "Shared A", ".t1shared99", import = function(p) list())
  register_filter("t1shared_b", "Shared B", ".t1shared99", import = function(p) list())
  result <- filter_for_ext(".t1shared99")
  expect_equal(length(result), 2L)
  names_found <- vapply(result, `[[`, character(1L), "name")
  expect_true("t1shared_a" %in% names_found)
  expect_true("t1shared_b" %in% names_found)
})
