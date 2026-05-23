#' Solver settings module — UI
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
      ),
      shiny::div(
        class = "col-4 col-md-4",
        shiny::numericInput(
          ns("t0"), "t0", value = 0, step = NA, width = "100%"
        )
      ),
      shiny::div(
        class = "col-4 col-md-4",
        shiny::numericInput(
          ns("tmax"), "tmax", value = 100, min = 0, step = NA, width = "100%"
        )
      ),
      shiny::div(
        class = "col-4 col-md-4",
        shiny::numericInput(
          ns("dt"), "dt", value = 0.1, min = 1e-9, step = NA, width = "100%"
        )
      )
    )
  )
}

#' Solver settings module — server
#'
#' @param id Module namespace id
#' @param ir Reactive returning the parsed model IR (from parse_model())
#' @return Reactive returning list(method, atol, rtol, t0, tmax, dt)
#' @export
mod_solver_server <- function(id, ir) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(ir(), {
      t <- ir()$time
      shiny::updateNumericInput(session, "t0",   value = t$t0)
      shiny::updateNumericInput(session, "tmax", value = t$tmax)
      shiny::updateNumericInput(session, "dt",   value = t$dt)
    })

    shiny::reactive({
      list(
        method = input$method %||% "lsoda",
        atol   = input$atol   %||% 1e-6,
        rtol   = input$rtol   %||% 1e-6,
        t0     = input$t0     %||% 0,
        tmax   = input$tmax   %||% 100,
        dt     = input$dt     %||% 0.1
      )
    })
  })
}
