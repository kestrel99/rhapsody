#' Steady-state module — UI
#' @param id Module namespace id
#' @export
mod_steadystate_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "p-2",
    shiny::fluidRow(
      shiny::column(6,
        shiny::selectInput(ns("method"), "Method",
          choices  = c("Run to SS" = "runsteady", "Newton (stode)" = "stode"),
          selected = "runsteady"
        )
      ),
      shiny::column(6,
        shiny::numericInput(ns("stol"), "Tolerance", value = 1e-8,
                            min = 1e-14, max = 1e-2)
      )
    ),
    shiny::actionButton(ns("find"), "Find Steady State",
                        class = "btn btn-secondary btn-sm mb-2"),
    shiny::uiOutput(ns("status_ui")),
    shiny::tableOutput(ns("ss_table")),
    shiny::uiOutput(ns("use_ics_ui"))
  )
}

#' Steady-state module — server
#'
#' @param id Module namespace id
#' @param ir Reactive returning Canonical IR (or NULL)
#' @param param_state Return value of mod_params_server (list of reactives)
#' @return List with one reactive: \code{use_as_ics()} — named numeric vector
#'   of steady-state values when the user clicks "Use as ICs", else NULL
#' @export
mod_steadystate_server <- function(id, ir, param_state) {
  shiny::moduleServer(id, function(input, output, session) {

    ss_result <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$find, {
      shiny::req(ir())
      result <- solve_steady(
        ir     = ir(),
        params = param_state$params(),
        ics    = param_state$ics(),
        method = input$method %||% "runsteady",
        stol   = input$stol   %||% 1e-8
      )
      ss_result(result)
    })

    output$status_ui <- shiny::renderUI({
      res <- ss_result()
      if (is.null(res) || !inherits(res, "rhapsody_error")) return(NULL)
      shiny::div(class = "alert alert-danger py-1 px-2 small", res$message)
    })

    output$ss_table <- shiny::renderTable({
      res <- ss_result()
      if (is.null(res) || inherits(res, "rhapsody_error")) return(NULL)
      data.frame(
        State         = names(res),
        `Steady State` = as.numeric(res),
        check.names   = FALSE
      )
    }, digits = 6)

    output$use_ics_ui <- shiny::renderUI({
      res <- ss_result()
      if (is.null(res) || inherits(res, "rhapsody_error")) return(NULL)
      shiny::actionButton(
        session$ns("use_as_ics"), "Use as ICs",
        class = "btn btn-outline-primary btn-sm mt-1"
      )
    })

    use_as_ics_rv <- shiny::eventReactive(input$use_as_ics, {
      ss_result()
    }, ignoreNULL = TRUE)

    list(use_as_ics = use_as_ics_rv)
  })
}
