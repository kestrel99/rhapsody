# Berkeley Madonna keywords that have no rhapsody equivalent — skip with a warning
#' @noRd
.bm_skip_keywords <- c(
  "METHOD", "TOLERANCE", "DTMAX", "DTMIN", "GRAPH", "TABLE",
  "BATCH", "DISPLAY", "SPECS", "RUNTIME"
)

# Import a Berkeley Madonna (.mmd) file and return an IR.
#' @noRd
mmd_import <- function(path) {
  raw <- tryCatch(
    readLines(path, warn = FALSE),
    error = function(e) structure(list(message = conditionMessage(e)), class = "rhapsody_error")
  )
  if (inherits(raw, "rhapsody_error")) return(raw)

  # 1. Join all lines; strip block comments {...} (may span lines)
  raw_str <- paste(raw, collapse = "\n")
  raw_str <- gsub("\\{[^}]*\\}", "", raw_str, perl = TRUE)

  # Split back on newlines
  lines <- strsplit(raw_str, "\n", fixed = TRUE)[[1L]]

  # 2. Strip ; inline comments, trimws, drop blank lines
  lines <- vapply(lines, function(l) trimws(sub(";.*$", "", l, perl = TRUE)), character(1L))
  lines <- lines[nzchar(lines)]

  states     <- list()
  ics        <- list()
  parameters <- list()
  time       <- list(t0 = 0, tmax = 100, dt = 0.1)
  warnings   <- character(0L)

  # 3. Parse each line
  for (line in lines) {
    uline <- toupper(line)

    # a. ODE: d/dt(X) = expr
    m <- regmatches(line, regexec(
      "^d/dt\\s*\\(\\s*(\\w+)\\s*\\)\\s*=\\s*(.+)$", line, perl = TRUE
    ))[[1L]]
    if (length(m) == 3L) {
      nm   <- m[2L]
      expr <- trimws(m[3L])
      if (is.null(states[[nm]])) states[[nm]] <- list(ode_expr = NULL, disc_expr = NULL, init_expr = "0")
      states[[nm]]$ode_expr <- expr
      next
    }

    # b. INIT X = value (case-insensitive)
    m <- regmatches(line, regexec(
      "^INIT\\s+(\\w+)\\s*=\\s*(.+)$", line, perl = TRUE, ignore.case = TRUE
    ))[[1L]]
    if (length(m) == 3L) {
      ics[[m[2L]]] <- trimws(m[3L])
      next
    }

    # c. X(0) = value
    m <- regmatches(line, regexec(
      "^(\\w+)\\s*\\(0\\)\\s*=\\s*(.+)$", line, perl = TRUE
    ))[[1L]]
    if (length(m) == 3L) {
      ics[[m[2L]]] <- trimws(m[3L])
      next
    }

    # d. STARTTIME
    if (grepl("^STARTTIME\\s*=", uline, perl = TRUE)) {
      val <- as.numeric(trimws(sub("^STARTTIME\\s*=\\s*", "", uline, perl = TRUE)))
      if (!is.na(val)) time$t0 <- val
      next
    }

    # e. STOPTIME
    if (grepl("^STOPTIME\\s*=", uline, perl = TRUE)) {
      val <- as.numeric(trimws(sub("^STOPTIME\\s*=\\s*", "", uline, perl = TRUE)))
      if (!is.na(val)) time$tmax <- val
      next
    }

    # f. DT
    if (grepl("^DT\\s*=", uline, perl = TRUE)) {
      val <- as.numeric(trimws(sub("^DT\\s*=\\s*", "", uline, perl = TRUE)))
      if (!is.na(val)) time$dt <- val
      next
    }

    # g. Skip BM keywords
    first_word <- sub("^(\\w+).*$", "\\1", uline, perl = TRUE)
    if (first_word %in% .bm_skip_keywords) {
      warnings <- c(warnings, paste0("Skipped unsupported Berkeley Madonna keyword: ", line))
      next
    }

    # h. Parameter: name = numeric_literal (pure number RHS only)
    m <- regmatches(line, regexec(
      "^(\\w+)\\s*=\\s*([+-]?(?:\\d+\\.?\\d*|\\.\\d+)(?:[eE][+-]?\\d+)?)$",
      line, perl = TRUE
    ))[[1L]]
    if (length(m) == 3L) {
      nm    <- m[2L]
      value <- as.numeric(m[3L])
      parameters[[nm]] <- list(
        value     = value,
        range_min = NA_real_,
        range_max = NA_real_,
        log_scale = FALSE
      )
      next
    }

    # i. Unrecognized non-blank line
    warnings <- c(warnings, paste0("Unrecognized line skipped: ", line))
  }

  # 4. Apply collected ICs to states
  for (nm in names(ics)) {
    if (is.null(states[[nm]])) {
      states[[nm]] <- list(ode_expr = NULL, disc_expr = NULL, init_expr = ics[[nm]])
    } else {
      states[[nm]]$init_expr <- ics[[nm]]
    }
  }

  # 5. If no ODE states found → rhapsody_error
  if (length(states) == 0L) {
    return(structure(
      list(message = "No ODE equations found in Berkeley Madonna file."),
      class = "rhapsody_error"
    ))
  }

  # 6. Reconstruct rhapsody syntax
  rhy_lines <- character(0L)

  for (nm in names(states)) {
    s <- states[[nm]]
    if (!is.null(s$ode_expr)) {
      rhy_lines <- c(rhy_lines, paste0("d", nm, "/dt = ", s$ode_expr))
    }
  }
  for (nm in names(states)) {
    s <- states[[nm]]
    ic <- if (!is.null(s$init_expr)) s$init_expr else "0"
    rhy_lines <- c(rhy_lines, paste0(nm, "[0] = ", ic))
  }
  for (nm in names(parameters)) {
    p <- parameters[[nm]]
    rhy_lines <- c(rhy_lines, paste0(nm, " = ", format(p$value, scientific = FALSE)))
  }
  rhy_lines <- c(
    rhy_lines,
    paste0("t0 = ", format(time$t0, scientific = FALSE)),
    paste0("tmax = ", format(time$tmax, scientific = FALSE)),
    paste0("dt = ", format(time$dt, scientific = FALSE))
  )

  # 7. Call parse_model
  ir <- tryCatch(
    parse_model(paste(rhy_lines, collapse = "\n")),
    error = function(e) structure(list(message = conditionMessage(e)), class = "rhapsody_error")
  )
  if (inherits(ir, "rhapsody_error")) return(ir)

  # 8. Attach warnings if any
  if (length(warnings) > 0L) {
    attr(ir, "import_warnings") <- warnings
  }

  # 9. Return IR
  ir
}

# Export an IR to a Berkeley Madonna (.mmd) text file.
#' @noRd
mmd_export <- function(ir, path) {
  lines <- character(0L)

  lines <- c(
    lines,
    paste0("STARTTIME = ", format(ir$time$t0,   scientific = FALSE)),
    paste0("STOPTIME  = ", format(ir$time$tmax,  scientific = FALSE)),
    paste0("DT        = ", format(ir$time$dt,    scientific = FALSE)),
    ""
  )

  for (nm in names(ir$states)) {
    s <- ir$states[[nm]]
    if (!is.null(s$ode_expr)) {
      lines <- c(lines, paste0("d/dt(", nm, ") = ", s$ode_expr))
    }
  }
  lines <- c(lines, "")

  for (nm in names(ir$states)) {
    s  <- ir$states[[nm]]
    ic <- if (!is.null(s$init_expr)) s$init_expr else "0"
    lines <- c(lines, paste0("INIT ", nm, " = ", ic))
  }
  lines <- c(lines, "")

  for (nm in names(ir$parameters)) {
    p <- ir$parameters[[nm]]
    lines <- c(lines, paste0(nm, " = ", format(p$value, scientific = FALSE)))
  }

  writeLines(lines, path)
}
