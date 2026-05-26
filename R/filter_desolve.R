# Generate a standalone deSolve R script from an ODE IR.
# The script requires only base R and deSolve — no rhapsody dependency.
#' @noRd
desolve_export <- function(ir, path) {
  if (!isTRUE(ir$type == "ode")) {
    stop("deSolve export requires an ODE model (d/dt syntax). ",
         "Difference-equation models are not supported.")
  }
  state_names <- names(ir$states)
  param_names <- names(ir$parameters)

  # Evaluate init expressions with parameter values available
  param_vals <- vapply(ir$parameters, `[[`, numeric(1L), "value")
  safe_parent <- list2env(as.list(param_vals), parent = baseenv())
  ic_vals    <- vapply(ir$states, function(s) {
    tryCatch(
      as.numeric(eval(parse(text = s$init_expr), envir = safe_parent)),
      error = function(e) 0
    )
  }, numeric(1L))

  # Build named-vector lines with trailing comma on all but last
  fmt_named_vec <- function(names, values) {
    if (length(names) == 0L) return(character(0L))
    entries <- paste0("  ", names, " = ", format(values, scientific = FALSE))
    ifelse(seq_along(entries) < length(entries),
           paste0(entries, ","), entries)
  }

  param_lines  <- fmt_named_vec(param_names, param_vals)
  ic_lines     <- fmt_named_vec(state_names, ic_vals)
  d_names      <- if (length(state_names) == 0L) character(0L) else paste0("d", state_names)
  deriv_lines  <- paste0("    ", d_names, " <- ",
                         vapply(ir$states, `[[`, character(1L), "ode_expr"))
  return_line  <- paste0("    list(c(", paste(d_names, collapse = ", "), "))")

  lines <- c(
    "library(deSolve)",
    "",
    "params <- c(",
    param_lines,
    ")",
    "",
    "y0 <- c(",
    ic_lines,
    ")",
    "",
    "ode_fn <- function(t, y, parms) {",
    "  with(as.list(c(y, parms)), {",
    deriv_lines,
    return_line,
    "  })",
    "}",
    "",
    paste0("times <- seq(", ir$time$t0, ", ", ir$time$tmax, ", by = ", ir$time$dt, ")"),
    "out <- deSolve::ode(y = y0, times = times, func = ode_fn, parms = params)",
    "plot(out)"
  )
  writeLines(lines, path)
}
