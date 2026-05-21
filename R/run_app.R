#' Launch the rhapsody Shiny application
#'
#' @param ... Arguments passed to shiny::shinyApp()
#' @export
run_app <- function(...) {
  shiny::shinyApp(
    ui     = app_ui,
    server = app_server,
    ...
  )
}
