# Import a .rhy file (full session or model-only) and return an IR.
# Reads the "model" field from the JSON and passes it through parse_model().
#' @noRd
rhy_import <- function(path) {
  raw <- tryCatch(
    jsonlite::fromJSON(path, simplifyVector = FALSE),
    error = function(e) structure(list(message = conditionMessage(e)), class = "rhapsody_error")
  )
  if (inherits(raw, "rhapsody_error")) return(raw)
  model_src <- raw[["model"]]
  if (is.null(model_src) || !nzchar(trimws(as.character(model_src)))) {
    return(structure(list(message = "No model source found in .rhy file."),
                     class = "rhapsody_error"))
  }
  tryCatch(
    parse_model(as.character(model_src)),
    error = function(e) structure(list(message = conditionMessage(e)), class = "rhapsody_error")
  )
}

# Export an IR to a .rhy JSON file containing the model source only.
# (Full session save — including params and solver — is handled separately
# by session_to_json / session_from_json via the Save/Load toolbar buttons.)
#' @noRd
rhy_export <- function(ir, path) {
  data <- list(
    rhapsody_version = as.character(utils::packageVersion("rhapsody")),
    model            = ir$raw_source
  )
  writeLines(jsonlite::toJSON(data, auto_unbox = TRUE, pretty = TRUE), path)
}
