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
    errors <- c(errors, "No equations found. Add at least one ODE (dX/dt = ...) or difference equation (X[t+1] = ...).")
    return(list(errors = errors, warnings = warnings))
  }

  # Mixed-type model cannot be solved
  if (isTRUE(ir$type == "mixed")) {
    errors <- c(errors,
      "Mixed ODE and difference equation models are not supported. Use d/dt or [t+1] syntax, not both.")
    return(list(errors = errors, warnings = warnings))
  }

  for (nm in names(ir$states)) {
    has_ode  <- !is.null(ir$states[[nm]]$ode_expr)  && nzchar(trimws(ir$states[[nm]]$ode_expr))
    has_disc <- !is.null(ir$states[[nm]]$disc_expr) && nzchar(trimws(ir$states[[nm]]$disc_expr))
    if (!has_ode && !has_disc) {
      errors <- c(errors, sprintf(
        "State '%s' has no equation (d%s/dt = ... or %s[t+1] = ...).", nm, nm, nm
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

  # State-based events: trigger state and target var must be state variables
  state_names_set <- names(ir$states)
  for (ev in ir$events) {
    if (!isTRUE(ev$type == "state")) next
    if (!ev$state %in% state_names_set) {
      errors <- c(errors, sprintf(
        "State event trigger '%s' is not a state variable.", ev$state
      ))
    }
    if (!ev$var %in% state_names_set) {
      errors <- c(errors, sprintf(
        "State event target '%s' must be a state variable (not a parameter).",
        ev$var
      ))
    }
  }

  # Time events and state events cannot be combined in the current implementation
  # (deSolve ignores scheduled times when root-finding is active)
  has_time_ev  <- any(vapply(ir$events, function(ev) isTRUE(ev$type == "time"),  logical(1L)))
  has_state_ev <- any(vapply(ir$events, function(ev) isTRUE(ev$type == "state"), logical(1L)))
  if (has_time_ev && has_state_ev) {
    errors <- c(errors,
      "Combining time-based and state-based events in the same model is not currently supported.")
  }

  list(errors = errors, warnings = warnings)
}
