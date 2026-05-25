.default_model_code <- paste(
  "dX/dt = r * X * (1 - X / K)",
  "dY/dt = a * X * Y - b * Y",
  "X[0] = 10",
  "Y[0] = 2",
  "r = 1.2   # [0.1, 3]",
  "K = 100   # [10, 500]",
  "a = 0.01  # [0, 0.1]",
  "b = 0.5   # [0.01, 2]",
  "t0   = 0",
  "tmax = 100",
  "dt   = 0.1",
  sep = "\n"
)

#' Model code editor module — UI
#' @param id Module namespace id
#' @export
mod_editor_ui <- function(id) {
  ns <- shiny::NS(id)
  shinyAce::aceEditor(
    outputId = ns("code"),
    value    = .default_model_code,
    mode     = "text",
    theme    = "tomorrow",
    height   = "420px",
    fontSize = 14,
    debounce = 0L
  )
}

#' Model code editor module — server
#' @param id Module namespace id
#' @return Reactive string containing the current editor text
#' @export
mod_editor_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::reactive(input$code)
  })
}
