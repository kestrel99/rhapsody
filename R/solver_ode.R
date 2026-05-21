#' Run an ODE simulation from a parsed model IR
#'
#' @param ir Canonical IR list from parse_model()
#' @param params Named list of parameter overrides (optional)
#' @param method deSolve method string; default "lsoda"
#' @return data.frame with columns: time, and one column per state variable
#' @export
solve_ode <- function(ir, params = NULL, method = "lsoda") {
  state_names <- names(ir$states)

  # Build parameter list: IR defaults, then any caller overrides
  parms <- lapply(ir$parameters, `[[`, "value")
  names(parms) <- names(ir$parameters)
  if (!is.null(params)) {
    for (nm in names(params)) parms[[nm]] <- params[[nm]]
  }

  # Evaluate initial conditions in the parameter environment
  param_env <- list2env(parms, parent = .GlobalEnv)
  y0 <- vapply(state_names, function(nm) {
    ic <- ir$states[[nm]]$init_expr %||% "0"
    tryCatch(
      eval(parse(text = ic), envir = param_env),
      error = function(e) stop(
        "Error evaluating initial condition for '", nm, "': ", conditionMessage(e)
      )
    )
  }, numeric(1L))
  names(y0) <- state_names

  # Pre-parse ODE expressions once for performance
  ode_parsed <- lapply(ir$states, function(s) parse(text = s$ode_expr))

  # deSolve-compatible ODE function
  ode_fn <- function(t, y, parms) {
    env <- list2env(c(as.list(parms), as.list(y), list(t = t)), parent = .GlobalEnv)
    dydt <- vapply(ode_parsed, eval, numeric(1L), envir = env)
    list(dydt)
  }

  times <- seq(ir$time$t0, ir$time$tmax, by = ir$time$dt)
  out   <- deSolve::ode(y = y0, times = times, func = ode_fn, parms = parms, method = method)
  as.data.frame(out)
}

`%||%` <- function(x, y) if (is.null(x)) y else x
