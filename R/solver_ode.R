#' Run an ODE simulation from a parsed model IR
#'
#' @param ir Canonical IR list from parse_model()
#' @param params Named list of parameter overrides (optional)
#' @param ics Named list of initial condition overrides (optional).
#'   Values must be numeric scalars; names must match state names in `ir`.
#' @param method deSolve method string; default "lsoda"
#' @param atol Absolute tolerance passed to deSolve; default 1e-6
#' @param rtol Relative tolerance passed to deSolve; default 1e-6
#' @param t0 Override simulation start time (NULL = use IR value)
#' @param tmax Override simulation end time (NULL = use IR value)
#' @param dt Override output time step (NULL = use IR value)
#' @return data.frame with columns: time, one per state variable,
#'   and one per auxiliary variable (in declaration order)
#' @export
solve_ode <- function(ir, params = NULL, ics = NULL, method = "lsoda",
                      atol = 1e-6, rtol = 1e-6, t0 = NULL, tmax = NULL,
                      dt = NULL) {
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
      error = function(e) {
        stop(
          "Error evaluating initial condition for '", nm, "': ",
          conditionMessage(e)
        )
      }
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

  # Separate time-based and state-based events
  time_events  <- Filter(function(ev) isTRUE(ev$type == "time"),  ir$events)
  state_events <- Filter(function(ev) isTRUE(ev$type == "state"), ir$events)

  # Pre-parse event expressions
  time_ev_parsed <- lapply(time_events, function(ev) {
    list(
      var  = ev$var,
      expr = parse(text = ev$expr)
    )
  })
  state_ev_parsed <- lapply(state_events, function(ev) {
    list(
      state      = ev$state,
      comparator = ev$comparator,
      threshold  = ev$threshold,
      var        = ev$var,
      expr       = parse(text = ev$expr)
    )
  })

  events_arg <- NULL

  if (length(state_events) > 0L) {
    # State events require root-finding; use lsoda which supports rootfunc
    # and continues integration after each event (lsodar stops at first root)
    if (!identical(method, "lsoda")) {
      warning("State-based events require method='lsoda'; ignoring caller-supplied method='", method, "'.")
    }
    method <- "lsoda"

    rootfunc <- function(t, y, parms) {
      vapply(state_ev_parsed, function(ev) {
        val <- as.numeric(y[ev$state])
        if (ev$comparator %in% c(">", ">=")) val - ev$threshold
        else                                   ev$threshold - val
      }, numeric(1L))
    }

    # Event function: apply state events only for the state that is actually
    # crossing its threshold.
    # lsoda calls event_fn once per root with no indication of which root triggered;
    # the proximity check prevents applying this event when a different state
    # crossed its threshold.
    # Time events are not handled here — deSolve ignores events$time when
    # root=TRUE, so mixed models are rejected at validation time.
    event_fn <- function(t, y, parms) {
      env <- list2env(
        c(as.list(parms), as.list(y), list(t = t)),
        parent = safe_parent
      )
      for (ev in state_ev_parsed) {
        val <- as.numeric(y[ev$state])
        near_threshold <- abs(val - ev$threshold) <= max(atol, rtol * abs(ev$threshold)) * 100
        if (near_threshold) {
          y[ev$var] <- eval(ev$expr, envir = env)
        }
      }
      y
    }

    events_arg <- list(func = event_fn, root = TRUE)

  } else if (length(time_events) > 0L) {
    # Time events only — original approach, no rootfunc needed
    ev_times <- sort(unique(vapply(time_events, `[[`, numeric(1L), "time")))
    event_fn <- function(t, y, parms) {
      env <- list2env(
        c(as.list(parms), as.list(y), list(t = t)),
        parent = safe_parent
      )
      for (ev in time_ev_parsed) y[ev$var] <- eval(ev$expr, envir = env)
      y
    }
    events_arg <- list(func = event_fn, time = ev_times)
  }

  t_start <- t0   %||% ir$time$t0
  t_end   <- tmax %||% ir$time$tmax
  t_step  <- dt   %||% ir$time$dt
  times <- seq(t_start, t_end, by = t_step)

  out   <- deSolve::ode(
    y = y0, times = times, func = ode_fn,
    parms = parms, method = method,
    atol = atol, rtol = rtol,
    events = events_arg,
    rootfunc = if (length(state_events) > 0L) rootfunc else NULL
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
