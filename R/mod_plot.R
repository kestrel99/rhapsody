#' Plotly plot module — UI
#' @param id Module namespace id
#' @export
mod_plot_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("error_panel")),
    shiny::div(
      class = "d-flex align-items-center gap-2 px-1 mb-1",
      shiny::tags$small("X:", class = "text-muted"),
      shiny::uiOutput(ns("xaxis_ui"))
    ),
    plotly::plotlyOutput(ns("plot"), height = "400px")
  )
}

#' Plotly plot module — server
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

    output$xaxis_ui <- shiny::renderUI({
      res <- result()
      if (!is.data.frame(res)) return(NULL)
      choices <- c("time", setdiff(names(res), "time"))
      shiny::selectInput(
        session$ns("xvar"), label = NULL,
        choices = choices, selected = "time", width = "130px"
      )
    })

    output$plot <- plotly::renderPlotly({
      res <- result()
      if (inherits(res, "rhapsody_error")) return(plotly::plotly_empty())
      shiny::req(is.data.frame(res))
      xvar   <- if (!is.null(input$xvar) && input$xvar %in% names(res)) {
        input$xvar
      } else {
        "time"
      }
      y_cols <- setdiff(names(res), xvar)
      p <- plotly::plot_ly()
      for (col in y_cols) {
        p <- plotly::add_lines(p, x = res[[xvar]], y = res[[col]], name = col)
      }
      plotly::layout(p,
        xaxis  = list(title = xvar),
        yaxis  = list(title = "Value"),
        legend = list(orientation = "h", y = -0.2)
      )
    })
  })
}
