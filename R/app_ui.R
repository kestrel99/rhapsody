#' Shiny application UI
#' @param request Shiny request object (required by golem)
#' @export
app_ui <- function(request) {
  shiny::tagList(
    golem_add_external_resources(),
    bslib::page_fluid(
      theme = bslib::bs_theme(version = 5),

      # ── Toolbar ──────────────────────────────────────────────
      shiny::div(
        class = "d-flex align-items-center gap-3 p-2 bg-light border-bottom",
        shiny::tags$strong("rhapsody", class = "fs-5 me-2"),
        shiny::actionButton("run", "Run", class = "btn btn-primary btn-sm")
      ),

      # ── Main split pane ──────────────────────────────────────
      bslib::layout_columns(
        col_widths = c(5, 7),
        shiny::div(class = "p-3", mod_editor_ui("editor")),
        shiny::div(class = "p-3", mod_plot_ui("plot"))
      )
    )
  )
}

golem_add_external_resources <- function() {
  shiny::tags$head(
    golem::favicon(),
    golem::bundle_resources(
      path      = app_sys("app/www"),
      app_title = "rhapsody"
    )
  )
}
