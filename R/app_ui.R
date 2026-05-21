#' @noRd
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

      # ── 4-pane workspace ─────────────────────────────────────
      bslib::layout_columns(
        col_widths = c(5, 7),

        # Left column: editor (fixed height) + scrollable params below
        shiny::div(
          style = paste(
            "display: flex;",
            "flex-direction: column;",
            "height: calc(100vh - 58px);"
          ),
          shiny::div(
            style = "flex: 0 0 auto; padding: 0.5rem 0.75rem;",
            mod_editor_ui("editor"),
            shiny::uiOutput("parse_error_ui")
          ),
          shiny::div(
            style = "flex: 1 1 auto; overflow-y: auto;",
            mod_params_ui("params")
          )
        ),

        # Right column: plot (full height for now; tab panel added in Phase 3)
        shiny::div(
          class = "p-3",
          mod_plot_ui("plot")
        )
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
