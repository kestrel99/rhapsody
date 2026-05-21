#' Shiny application server
#' @param input,output,session Standard Shiny server arguments
#' @export
app_server <- function(input, output, session) {
  model_code <- mod_editor_server("editor")

  solve_result <- shiny::eventReactive(input$run, {
    shiny::req(model_code())
    tryCatch({
      ir   <- parse_model(model_code())
      errs <- validate_model(ir)
      if (length(errs$errors) > 0L) {
        stop(paste(errs$errors, collapse = "\n"))
      }
      solve_ode(ir)
    }, error = function(e) {
      structure(list(message = conditionMessage(e)), class = "rhapsody_error")
    })
  })

  mod_plot_server("plot", solve_result)
}
