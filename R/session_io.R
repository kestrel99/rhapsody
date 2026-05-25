# Serialize current session state to a JSON string.
# Returns a JSON character string (pretty-printed).
#' @noRd
session_to_json <- function(model, params, ics, solver) {
  data <- list(
    rhapsody_version = as.character(utils::packageVersion("rhapsody")),
    model            = model,
    params           = params,
    ics              = ics,
    solver           = solver
  )
  jsonlite::toJSON(data, auto_unbox = TRUE, pretty = TRUE)
}

# Load a session from a .rhy JSON file.
# Returns a named list with fields: rhapsody_version, model, params, ics,
# solver, or a rhapsody_error if the file is invalid.
#' @noRd
session_from_json <- function(path) {
  tryCatch({
    raw <- jsonlite::fromJSON(path, simplifyVector = FALSE)
    required <- c("model", "params", "ics", "solver")
    missing_fields <- setdiff(required, names(raw))
    if (length(missing_fields) > 0L) {
      stop(sprintf(
        "Invalid session file: missing required field(s): %s.",
        paste(missing_fields, collapse = ", ")
      ))
    }
    raw
  }, error = function(e) {
    structure(list(message = conditionMessage(e)), class = "rhapsody_error")
  })
}
