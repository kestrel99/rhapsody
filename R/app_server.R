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

  # ── Session: New ─────────────────────────────────────────────
  shiny::observeEvent(input$new_session, {
    shinyAce::updateAceEditor(session, "editor-code", value = paste(
      "dX/dt = r * X * (1 - X / K)",
      "dY/dt = a * X * Y - b * Y",
      "X[0] = 10",
      "Y[0] = 2",
      "r = 1.2   # [0.1, 3]",
      "K = 100   # [10, 500]",
      "a = 0.01  # [0, 0.1]",
      "b = 0.5   # [0.01, 2]",
      "t0   = 0",
      "tmax = 100",
      "dt   = 0.1",
      sep = "\n"
    ))
  })

  # ── Session: Save ─────────────────────────────────────────────
  output$session_save <- shiny::downloadHandler(
    filename = function() {
      paste0("rhapsody-session-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".rhy")
    },
    content = function(file) {
      json_str <- session_to_json(
        model  = model_code(),
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
  }, ignoreNULL = TRUE)
}
