# STUB — full implementation in Task 3 (Phase 6)
#' @noRd
mod_report_ui <- function(id) {
  shiny::actionButton(
    shiny::NS(id, "btn"), "Report",
    class = "btn btn-outline-secondary btn-sm"
  )
}

# STUB — full implementation in Task 3 (Phase 6)
#' @noRd
mod_report_server <- function(id, ir, param_state, solve_result) {
  shiny::moduleServer(id, function(input, output, session) {
    # placeholder
  })
}
