#' Run a synchronous parameter scan over an ODE or DDE model
#'
#' @param ir Canonical IR list from parse_model()
#' @param params Named list of baseline parameter overrides (optional)
#' @param ics Named list of IC overrides (optional)
#' @param scan_spec Named list with fields:
#'   \describe{
#'     \item{parameter}{Name of the parameter to scan (character)}
#'     \item{from}{Scan start value (numeric)}
#'     \item{to}{Scan end value (numeric)}
#'     \item{steps}{Number of evenly-spaced values (integer)}
#'     \item{log_scale}{Use geometric spacing if TRUE (logical)}
#'   }
#' @return Named list with fields:
#'   \describe{
#'     \item{param_name}{The scanned parameter name}
#'     \item{param_values}{Numeric vector of tested values}
#'     \item{results}{List of data.frames (one per step), or rhapsody_error on failure}
#'   }
#' @export
scan_params <- function(ir, params = NULL, ics = NULL, scan_spec) {
  param_nm  <- scan_spec$parameter
  from_val  <- scan_spec$from
  to_val    <- scan_spec$to
  steps     <- as.integer(scan_spec$steps)
  log_scale <- isTRUE(scan_spec$log_scale)

  param_values <- if (log_scale) {
    exp(seq(log(from_val), log(to_val), length.out = steps))
  } else {
    seq(from_val, to_val, length.out = steps)
  }

  results <- lapply(param_values, function(pv) {
    p_override <- c(as.list(params %||% list()), setNames(list(pv), param_nm))
    tryCatch({
      if (isTRUE(ir$type == "dde")) {
        solve_discrete(ir, params = p_override, ics = ics)
      } else {
        solve_ode(ir, params = p_override, ics = ics)
      }
    }, error = function(e) {
      structure(list(message = conditionMessage(e)), class = "rhapsody_error")
    })
  })

  list(param_name = param_nm, param_values = param_values, results = results)
}
