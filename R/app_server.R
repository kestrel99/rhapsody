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
  solver_state <- mod_solver_server("solver")

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
      if (isTRUE(ir_val$type == "dde")) {
        solve_discrete(
          ir     = ir_val,
          params = param_state$params(),
          ics    = param_state$ics()
        )
      } else {
        solve_ode(
          ir     = ir_val,
          params = param_state$params(),
          ics    = param_state$ics(),
          method = cfg$method,
          atol   = cfg$atol,
          rtol   = cfg$rtol
        )
      }
    }, error = function(e) {
      structure(list(message = conditionMessage(e)), class = "rhapsody_error")
    })
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  mod_plot_server("plot", solve_result)
}
