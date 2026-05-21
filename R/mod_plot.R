#' Plotly time-series plot module — UI
#' @param id Module namespace id
#' @export
mod_plot_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("error_panel")),
    plotly::plotlyOutput(ns("plot"), height = "420px")
  )
}

#' Plotly time-series plot module — server
#'
#' @param id Module namespace id
#' @param result Reactive returning either a data.frame (success)
#'   or an object of class "rhapsody_error" (failure)
#' @export
mod_plot_server <- function(id, result) {
  shiny::moduleServer(id, function(input, output, session) {
    output$error_panel <- shiny::renderUI({
      res <- result()
      if (inherits(res, "rhapsody_error")) {
        shiny::div(class = "alert alert-danger mt-2", res$message)
      }
    })

    output$plot <- plotly::renderPlotly({
      res <- result()
      if (inherits(res, "rhapsody_error")) return(plotly::plotly_empty())
      shiny::req(is.data.frame(res))

      state_cols <- setdiff(names(res), "time")
      p <- plotly::plot_ly()
      for (col in state_cols) {
        p <- plotly::add_lines(p, x = res$time, y = res[[col]], name = col)
      }
      plotly::layout(p,
        xaxis  = list(title = "Time"),
        yaxis  = list(title = "Value"),
        legend = list(orientation = "h", y = -0.2)
      )
    })
  })
}
