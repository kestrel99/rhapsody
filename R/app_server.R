#' @noRd
app_server <- function(input, output, session) {
  model_code <- mod_editor_server("editor")

  # Reactive IR — silences downstream on parse failure; last valid IR stays
  # cached by Shiny's reactive caching until next successful parse.
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

  # Parameter + IC panel wired to the live IR
  param_state <- mod_params_server("params", ir)

  # Unified solve trigger: incremented by Run button OR live param change
  trigger <- shiny::reactiveVal(0L)

  shiny::observeEvent(input$run, {
    trigger(trigger() + 1L)
  })

  # Live-mode observer: only fires when param/IC values actually change,
  # not when Live is toggled. ignoreInit prevents a solve on startup.
  shiny::observe({
    if (isTRUE(shiny::isolate(param_state$live_mode()))) {
      trigger(trigger() + 1L)
    }
  }) |> shiny::bindEvent(
    param_state$live_params(),
    param_state$live_ics(),
    ignoreInit = TRUE
  )

  # Solve when triggered; ignoreInit prevents solving on startup (trigger=0).
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
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  mod_plot_server("plot", solve_result)
}
