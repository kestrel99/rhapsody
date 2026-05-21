#' Run an ODE simulation from a parsed model IR
#'
#' @param ir Canonical IR list from parse_model()
#' @param params Named list of parameter overrides (optional)
#' @param ics Named list of initial condition overrides (optional).
#'   Values must be numeric scalars; names must match state names in `ir`.
#' @param method deSolve method string; default "lsoda"
#' @return data.frame with columns: time, one per state variable,
#'   and one per auxiliary variable (in declaration order)
#' @export
solve_ode <- function(ir, params = NULL, ics = NULL, method = "lsoda") {
  state_names <- names(ir$states)

  # Build parameter list: IR defaults, then any caller overrides
  parms <- lapply(ir$parameters, `[[`, "value")
  names(parms) <- names(ir$parameters)
  if (!is.null(params)) {
    for (nm in names(params)) parms[[nm]] <- params[[nm]]
  }

  # Restrict eval to base R only — prevents model code from touching Shiny
  # session state or calling arbitrary package functions
  safe_parent <- new.env(parent = baseenv())

  # Evaluate initial conditions: ics overrides take priority, then IR expr
  param_env <- list2env(parms, parent = safe_parent)
  y0 <- vapply(state_names, function(nm) {
    if (!is.null(ics) && !is.null(ics[[nm]])) {
      return(as.numeric(ics[[nm]]))
    }
    ic <- ir$states[[nm]]$init_expr %||% "0"
    tryCatch(
      eval(parse(text = ic), envir = param_env),
      error = function(e) stop(
        "Error evaluating initial condition for '", nm, "': ",
        conditionMessage(e)
      )
    )
  }, numeric(1L))
  names(y0) <- state_names

  # Pre-parse ODE expressions once; ode_fn closes over ode_parsed
  ode_parsed <- lapply(ir$states, function(s) parse(text = s$ode_expr))

  ode_fn <- function(t, y, parms) {
    env <- list2env(
      c(as.list(parms), as.list(y), list(t = t)),
      parent = safe_parent
    )
    dydt <- vapply(ode_parsed, eval, numeric(1L), envir = env)
    list(dydt)
  }

  times <- seq(ir$time$t0, ir$time$tmax, by = ir$time$dt)
  out   <- deSolve::ode(
    y = y0, times = times, func = ode_fn,
    parms = parms, method = method
  )
  out_df <- as.data.frame(out)

  # Evaluate auxiliary variables row-by-row from solved state values
  if (length(ir$auxiliary) > 0L) {
    aux_parsed <- lapply(ir$auxiliary, function(expr) parse(text = expr))
    aux_mat <- matrix(
      NA_real_,
      nrow     = nrow(out_df),
      ncol     = length(ir$auxiliary),
      dimnames = list(NULL, names(ir$auxiliary))
    )
    for (i in seq_len(nrow(out_df))) {
      row_env <- list2env(
        c(as.list(parms),
          as.list(out_df[i, state_names, drop = FALSE]),
          list(t = out_df$time[i])),
        parent = safe_parent
      )
      aux_mat[i, ] <- vapply(aux_parsed, eval, numeric(1L), envir = row_env)
    }
    out_df <- cbind(out_df, as.data.frame(aux_mat))
  }

  out_df
}

#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x
