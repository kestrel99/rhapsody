#' Solver settings module — UI
#'
#' Renders an integration method selector plus atol and rtol inputs.
#'
#' @param id Module namespace id
#' @export
mod_solver_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "p-2",
    shiny::div(
      class = "row g-2 align-items-end",
      shiny::div(
        class = "col-12 col-md-6",
        shiny::selectInput(
          ns("method"),
          "Integration method",
          choices = c(
            "lsoda (auto)"  = "lsoda",
            "euler"         = "euler",
            "rk4"           = "rk4",
            "ode23"         = "ode23",
            "ode45"         = "ode45",
            "adams"         = "adams",
            "lsode (stiff)" = "lsode",
            "vode (stiff)"  = "vode",
            "radau (stiff)" = "radau",
            "bdf (stiff)"   = "bdf"
          ),
          selected = "lsoda",
          width    = "100%"
        )
      ),
      shiny::div(
        class = "col-6 col-md-3",
        shiny::numericInput(
          ns("atol"), "atol", value = 1e-6, min = 0, step = NA, width = "100%"
        )
      ),
      shiny::div(
        class = "col-6 col-md-3",
        shiny::numericInput(
          ns("rtol"), "rtol", value = 1e-6, min = 0, step = NA, width = "100%"
        )
      )
    )
  )
}

#' Solver settings module — server
#'
#' @param id Module namespace id
#' @return Reactive returning list(method, atol, rtol)
#' @export
mod_solver_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::reactive({
      list(
        method = input$method %||% "lsoda",
        atol   = input$atol   %||% 1e-6,
        rtol   = input$rtol   %||% 1e-6
      )
    })
  })
}
