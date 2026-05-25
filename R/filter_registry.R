#' @noRd
.filter_env <- new.env(parent = emptyenv())

#' @noRd
register_filter <- function(name, label, ext, import = NULL, export = NULL) {
  .filter_env[[name]] <- list(
    name       = name,
    label      = label,
    ext        = ext,
    import     = import,
    export     = export,
    can_import = !is.null(import),
    can_export = !is.null(export)
  )
  invisible(name)
}

#' @noRd
get_importable_filters <- function() {
  Filter(function(f) isTRUE(f$can_import), as.list(.filter_env))
}

#' @noRd
get_exportable_filters <- function() {
  Filter(function(f) isTRUE(f$can_export), as.list(.filter_env))
}

#' @noRd
filter_for_ext <- function(ext) {
  Filter(function(f) ext %in% f$ext, as.list(.filter_env))
}

#' @noRd
filter_by_name <- function(name) {
  .filter_env[[name]]
}
