#' @noRd
app_server <- function(input, output, session) {
  model_code <- mod_editor_server("editor")

  parse_error <- shiny::reactiveVal(NULL)

  ir <- shiny::reactive({
    shiny::req(model_code())
    result <- tryCatch(
      { parse_error(NULL); parse_model(model_code()) },
      error = function(e) { parse_error(conditionMessage(e)); NULL }
    )
    shiny::req(!is.null(result))
    result
  })

  output$parse_error_ui <- shiny::renderUI({
    err <- parse_error()
    if (is.null(err)) return(NULL)
    shiny::div(
      class = "alert alert-danger mt-1 py-1 px-2 small",
      role  = "alert",
      err
    )
  })

  param_state  <- mod_params_server("params",  ir)
  solver_state <- mod_solver_server("solver", ir)

  trigger <- shiny::reactiveVal(0L)

  shiny::observeEvent(input$run, {
    trigger(trigger() + 1L)
  })

  shiny::observe({
    if (isTRUE(shiny::isolate(param_state$live_mode()))) {
      trigger(trigger() + 1L)
    }
  }) |> shiny::bindEvent(
    param_state$live_params(),
    param_state$live_ics(),
    ignoreInit = TRUE
  )

  solve_result <- shiny::eventReactive(trigger(), {
    shiny::req(ir())
    tryCatch({
      ir_val  <- ir()
      errs    <- validate_model(ir_val)
      if (length(errs$errors) > 0L) {
        stop(paste(errs$errors, collapse = "\n"))
      }
      cfg <- solver_state()
      if (!is.finite(cfg$t0) || !is.finite(cfg$tmax) || !is.finite(cfg$dt) ||
          cfg$dt <= 0 || cfg$tmax <= cfg$t0) {
        stop("Invalid solver time settings: t0 must be finite, dt must be positive, and tmax must be greater than t0.")
      }
      if (isTRUE(ir_val$type == "dde")) {
        solve_discrete(
          ir     = ir_val,
          params = param_state$params(),
          ics    = param_state$ics(),
          t0     = cfg$t0,
          tmax   = cfg$tmax,
          dt     = cfg$dt
        )
      } else {
        solve_ode(
          ir     = ir_val,
          params = param_state$params(),
          ics    = param_state$ics(),
          method = cfg$method,
          atol   = cfg$atol,
          rtol   = cfg$rtol,
          t0     = cfg$t0,
          tmax   = cfg$tmax,
          dt     = cfg$dt
        )
      }
    }, error = function(e) {
      structure(list(message = conditionMessage(e)), class = "rhapsody_error")
    })
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  mod_plot_server("plot", solve_result)

  ss_state <- mod_steadystate_server("steadystate", ir, param_state)

  shiny::observeEvent(ss_state$use_as_ics(), {
    ics <- ss_state$use_as_ics()
    if (is.null(ics) || inherits(ics, "rhapsody_error")) return()
    for (nm in names(ics)) {
      shiny::updateNumericInput(
        session = session,
        inputId = paste0("params-ic_", nm),
        value   = as.numeric(ics[[nm]])
      )
    }
  })

  mod_fft_server("fft", solve_result)
  mod_scan_server("scan", ir, param_state)

  mod_report_server("report", ir, param_state, solve_result)

  # ── Import ────────────────────────────────────────────────────
  import_warnings <- shiny::reactiveVal(NULL)

  shiny::observeEvent(input$import_btn, {
    import_warnings(NULL)
    importable <- get_importable_filters()
    exts <- unique(unlist(lapply(importable, `[[`, "ext")))
    accept_str <- paste(exts, collapse = ",")
    shiny::showModal(shiny::modalDialog(
      title  = "Import Model",
      shiny::fileInput(
        "import_file", label = NULL,
        accept      = accept_str,
        buttonLabel = "Choose file…",
        placeholder = "No file selected"
      ),
      shiny::uiOutput("import_warnings_ui"),
      footer = shiny::modalButton("Cancel"),
      size   = "s"
    ))
  })

  shiny::observeEvent(input$import_file, {
    info <- input$import_file
    shiny::req(!is.null(info))
    ext <- paste0(".", tolower(tools::file_ext(info$name)))
    candidates <- filter_for_ext(ext)
    if (length(candidates) == 0L) {
      shiny::showNotification(
        paste0("No import filter found for '", ext, "' files."),
        type = "error", duration = 6
      )
      return()
    }
    filt   <- candidates[[1L]]
    result <- filt$import(info$datapath)
    if (inherits(result, "rhapsody_error")) {
      shiny::showNotification(
        paste("Import failed:", result$message),
        type = "error", duration = 8
      )
      return()
    }
    import_warnings(attr(result, "import_warnings"))
    shinyAce::updateAceEditor(session, "editor-code", value = result$raw_source)
    shiny::removeModal()
  })

  output$import_warnings_ui <- shiny::renderUI({
    w <- import_warnings()
    if (is.null(w) || length(w) == 0L) return(NULL)
    shiny::div(
      class = "alert alert-warning mt-2 py-1 px-2 small",
      shiny::tags$strong("Import warnings:"),
      shiny::tags$ul(lapply(w, shiny::tags$li))
    )
  })

  # ── Export ────────────────────────────────────────────────────
  shiny::observeEvent(input$export_btn, {
    exportable <- get_exportable_filters()
    choices <- stats::setNames(
      vapply(exportable, `[[`, character(1L), "name"),
      vapply(exportable, `[[`, character(1L), "label")
    )
    has_result <- local({
      r <- shiny::isolate(solve_result())
      !is.null(r) && !inherits(r, "rhapsody_error")
    })
    shiny::showModal(shiny::modalDialog(
      title = "Export",
      shiny::tags$strong("Export Model"),
      shiny::radioButtons("export_format", label = NULL, choices = choices),
      shiny::downloadButton("export_model_dl", "Download",
                            class = "btn btn-primary btn-sm"),
      if (has_result) {
        shiny::tagList(
          shiny::hr(),
          shiny::tags$strong("Simulation Output"),
          shiny::br(),
          shiny::downloadButton("export_csv_dl", "Download CSV",
                                class = "btn btn-outline-secondary btn-sm mt-1")
        )
      },
      footer = shiny::modalButton("Cancel"),
      size   = "s"
    ))
  })

  output$export_model_dl <- shiny::downloadHandler(
    filename = function() {
      fmt  <- input$export_format
      filt <- filter_by_name(fmt)
      if (is.null(filt)) return("model.dat")
      paste0("model", filt$ext[[1L]])
    },
    content = function(file) {
      fmt  <- input$export_format
      filt <- filter_by_name(fmt)
      shiny::req(!is.null(filt), !is.null(filt$export))
      filt$export(ir(), file)
    }
  )

  output$export_csv_dl <- shiny::downloadHandler(
    filename = function() {
      paste0("simulation-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".csv")
    },
    content = function(file) {
      res <- solve_result()
      shiny::req(!is.null(res), !inherits(res, "rhapsody_error"))
      utils::write.csv(as.data.frame(res), file, row.names = FALSE)
    }
  )

  # ── Session: New ─────────────────────────────────────────────
  shiny::observeEvent(input$new_session, {
    shinyAce::updateAceEditor(session, "editor-code", value = .default_model_code)
  })

  # ── Session: Save ─────────────────────────────────────────────
  output$session_save <- shiny::downloadHandler(
    filename = function() {
      paste0("rhapsody-session-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".rhy")
    },
    content = function(file) {
      code <- model_code()
      if (is.null(code) || !nzchar(trimws(code))) {
        stop("No model to save.")
      }
      if (!is.null(parse_error())) {
        stop("Cannot save a session with a model parse error.")
      }
      json_str <- session_to_json(
        model  = code,
        params = param_state$params(),
        ics    = param_state$ics(),
        solver = solver_state()
      )
      writeLines(json_str, file)
    }
  )

  # ── Session: Load ─────────────────────────────────────────────
  pending_session <- shiny::reactiveVal(NULL)

  shiny::observeEvent(input$session_load, {
    info <- input$session_load
    shiny::req(!is.null(info))
    loaded <- session_from_json(info$datapath)
    if (inherits(loaded, "rhapsody_error")) {
      shiny::showNotification(
        paste("Failed to load session:", loaded$message),
        type = "error", duration = 8
      )
      return()
    }
    pending_session(loaded)
    shinyAce::updateAceEditor(session, "editor-code", value = loaded$model)
  })

  # NOTE: Shiny executes same-priority reactive observers in registration order.
  # mod_solver_server's ir() observer (registered at line 28) fires first and
  # resets t0/tmax/dt to model defaults. This observer fires second and
  # overwrites with the session's saved solver values. The ordering is intentional.
  shiny::observeEvent(ir(), {
    pd <- pending_session()
    if (is.null(pd)) return()
    pending_session(NULL)

    for (nm in names(pd$params)) {
      shiny::updateNumericInput(
        session = session,
        inputId = paste0("params-", nm),
        value   = as.numeric(pd$params[[nm]])
      )
    }
    for (nm in names(pd$ics)) {
      shiny::updateNumericInput(
        session = session,
        inputId = paste0("params-ic_", nm),
        value   = as.numeric(pd$ics[[nm]])
      )
    }
    slv <- pd$solver
    if (!is.null(slv)) {
      if (!is.null(slv$method)) shiny::updateSelectInput( session, "solver-method", selected = slv$method)
      if (!is.null(slv$atol))   shiny::updateNumericInput(session, "solver-atol",   value    = slv$atol)
      if (!is.null(slv$rtol))   shiny::updateNumericInput(session, "solver-rtol",   value    = slv$rtol)
      if (!is.null(slv$t0))     shiny::updateNumericInput(session, "solver-t0",     value    = slv$t0)
      if (!is.null(slv$tmax))   shiny::updateNumericInput(session, "solver-tmax",   value    = slv$tmax)
      if (!is.null(slv$dt))     shiny::updateNumericInput(session, "solver-dt",     value    = slv$dt)
    }

    # Notify about any session values that don't match the current model
    current_params <- names(ir()$parameters)
    current_states <- names(ir()$states)
    missing_params <- setdiff(names(pd$params), current_params)
    missing_ics    <- setdiff(names(pd$ics),    current_states)
    dropped <- c(missing_params, missing_ics)
    if (length(dropped) > 0L) {
      shiny::showNotification(
        paste0("Session loaded. The following saved values were not applied",
               " (not in current model): ",
               paste(dropped, collapse = ", "), "."),
        type = "warning", duration = 10
      )
    }
  }, ignoreInit = TRUE)
}
