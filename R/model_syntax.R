#' Parse model source text into the Canonical Internal Representation
#'
#' @param source Character string of model code
#' @return Named list with fields: meta, type, states, parameters,
#'   auxiliary, events, time, raw_source
#' @export
parse_model <- function(source) {
  stopifnot(is.character(source), length(source) == 1L)

  ir <- list(
    meta = list(
      title             = "",
      description       = "",
      rhapsody_version  = as.character(utils::packageVersion("rhapsody"))
    ),
    type       = "ode",
    states     = list(),
    parameters = list(),
    auxiliary  = list(),
    events     = list(),
    time       = list(t0 = 0, tmax = 100, dt = 0.1),
    raw_source = source
  )

  lines <- strsplit(source, "\n", fixed = TRUE)[[1]]

  for (line in lines) {
    line <- trimws(line)

    # 1. Skip blank lines and full-line comments (# or ##)
    if (nchar(line) == 0L || grepl("^#", line)) next

    # 2. ODE: dX/dt = expr  (## inline comment stripped from expr)
    m <- regmatches(line, regexec("^d(\\w+)/dt\\s*=\\s*(.+)$", line, perl = TRUE))[[1]]
    if (length(m) == 3L) {
      nm   <- m[2L]
      expr <- trimws(sub("##.*$", "", m[3L], perl = TRUE))
      if (is.null(ir$states[[nm]])) ir$states[[nm]] <- list(ode_expr = NULL, init_expr = "0")
      ir$states[[nm]]$ode_expr <- expr
      next
    }

    # 3. Initial condition: X[0] = expr
    m <- regmatches(line, regexec("^(\\w+)\\[0\\]\\s*=\\s*(.+)$", line, perl = TRUE))[[1]]
    if (length(m) == 3L) {
      nm   <- m[2L]
      expr <- trimws(sub("##.*$", "", m[3L], perl = TRUE))
      if (is.null(ir$states[[nm]])) ir$states[[nm]] <- list(ode_expr = NULL, init_expr = NULL)
      ir$states[[nm]]$init_expr <- expr
      next
    }

    # 4. Time settings: t0, tmax, dt = number
    m <- regmatches(line, regexec("^(t0|tmax|dt)\\s*=\\s*([+-]?[\\d.eE+\\-]+)", line, perl = TRUE))[[1]]
    if (length(m) == 3L) {
      ir$time[[m[2L]]] <- as.numeric(m[3L])
      next
    }

    # 5. Parameter: name = bare_number  [# [min, max, log?]]
    m <- regmatches(line, regexec(
      "^(\\w+)\\s*=\\s*([+-]?(?:\\d+\\.?\\d*|\\.\\d+)(?:[eE][+-]?\\d+)?)\\s*(?:#\\s*(\\[[^\\]]+\\]))?",
      line, perl = TRUE
    ))[[1]]
    if (length(m) >= 3L && nchar(m[2L]) > 0L) {
      nm         <- m[2L]
      value      <- as.numeric(m[3L])
      range_str  <- if (length(m) == 4L && nchar(m[4L]) > 0L) m[4L] else NA_character_
      parsed_rng <- .parse_range_spec(range_str)
      ir$parameters[[nm]] <- list(
        value     = value,
        range_min = parsed_rng$min,
        range_max = parsed_rng$max,
        log_scale = parsed_rng$log
      )
      next
    }

    # 6. Auxiliary: name = expr  (anything else with =)
    m <- regmatches(line, regexec("^(\\w+)\\s*=\\s*(.+)$", line, perl = TRUE))[[1]]
    if (length(m) == 3L) {
      nm   <- m[2L]
      expr <- trimws(sub("##.*$", "", m[3L], perl = TRUE))
      ir$auxiliary[[nm]] <- expr
      next
    }
  }

  if (length(ir$states) > 0L) ir$type <- "ode"
  ir
}

.parse_range_spec <- function(range_str) {
  default <- list(min = NA_real_, max = NA_real_, log = FALSE)
  if (is.na(range_str) || !nzchar(trimws(range_str))) return(default)
  # range_str looks like "[0.1, 5]" or "[0.1, 5, log]"
  inner <- trimws(gsub("^\\[|\\]$", "", trimws(range_str), perl = TRUE))
  parts <- trimws(strsplit(inner, ",", fixed = TRUE)[[1]])
  if (length(parts) < 2L) return(default)
  list(
    min = suppressWarnings(as.numeric(parts[1L])),
    max = suppressWarnings(as.numeric(parts[2L])),
    log = length(parts) >= 3L && tolower(trimws(parts[3L])) == "log"
  )
}
