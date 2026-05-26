.default_model_code <- paste(
  "# One-compartment PK with oral absorption and Emax PD",
  "dA/dt = -ka * A",
  "dC/dt = ka * A / Vd - ke * C",
  "",
  "A[0] = 100",
  "C[0] = 0",
  "",
  "ka   = 1.0   # [0.1, 5]",
  "ke   = 0.2   # [0.05, 1]",
  "Vd   = 20    # [5, 100]",
  "Emax = 10    # [1, 20]",
  "EC50 = 2     # [0.1, 10]",
  "",
  "E = Emax * C / (EC50 + C)",
  "",
  "t0   = 0",
  "tmax = 24",
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
