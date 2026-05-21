#' @noRd
app_server <- function(input, output, session) {
  model_code <- mod_editor_server("editor")

  # Reactive IR — only updates when parse succeeds; keeps last valid IR on error
  ir <- shiny::reactive({
    shiny::req(model_code())
    result <- tryCatch(parse_model(model_code()), error = function(e) NULL)
    shiny::req(!is.null(result))
    result
  })

  # Parameter + IC panel wired to the live IR
  param_state <- mod_params_server("params", ir)

  # Unified solve trigger: incremented by Run button OR live param change
  trigger <- shiny::reactiveVal(0L)

  shiny::observeEvent(input$run, {
    trigger(shiny::isolate(trigger()) + 1L)
  })

  shiny::observe({
    if (isTRUE(param_state$live_mode())) {
      param_state$live_params()   # register dependency
      param_state$live_ics()
      trigger(shiny::isolate(trigger()) + 1L)
    }
  })

  # Solve when triggered — passes current params and ICs from the panel
  solve_result <- shiny::eventReactive(trigger(), {
    shiny::req(ir())
    tryCatch({
      ir_val <- ir()
      errs   <- validate_model(ir_val)
      if (length(errs$errors) > 0L) {
        stop(paste(errs$errors, collapse = "\n"))
      }
      solve_ode(
        ir     = ir_val,
        params = param_state$params(),
        ics    = param_state$ics()
      )
    }, error = function(e) {
      structure(list(message = conditionMessage(e)), class = "rhapsody_error")
    })
  }, ignoreNULL = TRUE)

  mod_plot_server("plot", solve_result)
}
