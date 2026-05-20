#' Validate a parsed model IR
#'
#' @param ir Canonical IR list from parse_model()
#' @return list with two character vectors: `errors` (fatal) and `warnings`
#' @export
validate_model <- function(ir) {
  errors   <- character(0)
  warnings <- character(0)

  # Must have at least one state
  if (length(ir$states) == 0L) {
    errors <- c(errors, "No ODE equations found. Add at least one line like 'dX/dt = ...'.")
    return(list(errors = errors, warnings = warnings))
  }

  # Every state needs a non-empty ODE expression (IC is optional; defaults to "0")
  for (nm in names(ir$states)) {
    expr <- ir$states[[nm]]$ode_expr
    if (is.null(expr) || !nzchar(trimws(expr))) {
      errors <- c(errors, sprintf(
        "State '%s' has an initial condition but no ODE equation (d%s/dt = ...).", nm, nm
      ))
    }
  }

  # Time settings must be finite
  if (!is.finite(ir$time$t0))   errors <- c(errors, "t0 must be a finite number.")
  if (!is.finite(ir$time$tmax)) errors <- c(errors, "tmax must be a finite number.")
  if (!is.finite(ir$time$dt) || ir$time$dt <= 0) {
    errors <- c(errors, "dt must be a positive finite number.")
  }
  if (is.finite(ir$time$t0) && is.finite(ir$time$tmax) && ir$time$tmax <= ir$time$t0) {
    errors <- c(errors, "tmax must be greater than t0.")
  }

  list(errors = errors, warnings = warnings)
}
