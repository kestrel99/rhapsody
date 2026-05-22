#' Find the steady state of an ODE model
#'
#' @param ir Canonical IR list from parse_model()
#' @param params Named list of parameter overrides (optional)
#' @param ics Named list of starting-point IC overrides (optional)
#' @param method "runsteady" (default) or "stode"
#' @param stol Steady-state tolerance passed to rootSolve; default 1e-8
#' @return Named numeric vector of steady-state values, or object of
#'   class "rhapsody_error" if the solver fails
#' @export
solve_steady <- function(ir, params = NULL, ics = NULL,
                         method = "runsteady", stol = 1e-8) {
  state_names <- names(ir$states)

  parms <- lapply(ir$parameters, `[[`, "value")
  names(parms) <- names(ir$parameters)
  if (!is.null(params)) {
    for (nm in names(params)) parms[[nm]] <- params[[nm]]
  }

  safe_parent <- new.env(parent = baseenv())
  param_env   <- list2env(parms, parent = safe_parent)

  y0 <- vapply(state_names, function(nm) {
    if (!is.null(ics) && !is.null(ics[[nm]])) return(as.numeric(ics[[nm]]))
    ic <- ir$states[[nm]]$init_expr %||% "0"
    tryCatch(
      eval(parse(text = ic), envir = param_env),
      error = function(e) {
        stop("Error evaluating initial condition for '", nm, "': ", conditionMessage(e))
      }
    )
  }, numeric(1L))
  names(y0) <- state_names

  ode_parsed <- lapply(ir$states, function(s) parse(text = s$ode_expr))

  ode_fn <- function(t, y, parms) {
    env <- list2env(
      c(as.list(parms), as.list(y), list(t = t)),
      parent = safe_parent
    )
    dydt <- vapply(ode_parsed, eval, numeric(1L), envir = env)
    list(dydt)
  }

  tryCatch({
    # If derivatives are already all (near) zero at y0, we are already at SS
    dy0 <- ode_fn(ir$time$t0, y0, parms)[[1]]
    if (all(abs(dy0) <= stol)) {
      return(setNames(as.numeric(y0), state_names))
    }

    ss <- if (method == "stode") {
      rootSolve::stode(y = y0, func = ode_fn, parms = parms,
                       rtol = stol, atol = stol, ctol = stol,
                       positive = FALSE)
    } else {
      rootSolve::runsteady(
        y = y0, times = c(ir$time$t0, Inf),
        func = ode_fn, parms = parms, stol = stol
      )
    }
    setNames(as.numeric(ss$y), state_names)
  }, error = function(e) {
    structure(list(message = conditionMessage(e)), class = "rhapsody_error")
  })
}
