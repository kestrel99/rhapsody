#' @noRd
mod_report_ui <- function(id) {
  shiny::actionButton(
    shiny::NS(id, "btn"), "Report",
    class = "btn btn-outline-secondary btn-sm"
  )
}

#' @noRd
mod_report_server <- function(id, ir, param_state, solve_result) {
  shiny::moduleServer(id, function(input, output, session) {

    shiny::observeEvent(input$btn, {
      r          <- shiny::isolate(solve_result())
      has_result <- !is.null(r) && !inherits(r, "rhapsody_error")
      default_sections <- if (has_result) {
        c("model", "params", "plot", "table")
      } else {
        c("model", "params")
      }
      shiny::showModal(shiny::modalDialog(
        title = "Generate Report",
        shiny::textInput(session$ns("title"),  "Title",  value = "rhapsody Report"),
        shiny::textInput(session$ns("author"), "Author", value = ""),
        shiny::checkboxGroupInput(
          session$ns("sections"), "Include sections:",
          choices = c(
            "Model source"       = "model",
            "Parameters"         = "params",
            "Time-series plot"   = "plot",
            "Simulation table"   = "table"
          ),
          selected = default_sections
        ),
        shiny::hr(),
        shiny::downloadButton(session$ns("html"),    "HTML",
                              class = "btn btn-primary btn-sm me-1"),
        shiny::downloadButton(session$ns("word"),    "Word",
                              class = "btn btn-outline-secondary btn-sm me-1"),
        shiny::downloadButton(session$ns("rscript"), "R Script",
                              class = "btn btn-outline-secondary btn-sm"),
        footer = shiny::modalButton("Close"),
        size   = "m"
      ))
    })

    make_render <- function(output_format) {
      function(file) {
        shiny::req(ir())
        template <- system.file("templates", "report.Rmd", package = "rhapsody")
        if (!file.exists(template)) {
          shiny::showNotification(
            "report.Rmd template not found in package.",
            type = "error", duration = 8
          )
          stop("report.Rmd template not found in package.")
        }

        ir_val <- ir()
        res    <- solve_result()
        sim_df <- if (!is.null(res) && !inherits(res, "rhapsody_error")) {
          as.data.frame(res)
        } else {
          NULL
        }

        if (length(ir_val$parameters) == 0L) {
          params_df <- data.frame(name = character(0), value = numeric(0),
                                   min  = numeric(0),   max   = numeric(0),
                                   stringsAsFactors = FALSE)
        } else {
          params_df <- data.frame(
            name  = names(ir_val$parameters),
            value = vapply(ir_val$parameters, `[[`, numeric(1L), "value"),
            min   = vapply(ir_val$parameters,
                           function(p) if (is.na(p$range_min)) NA_real_ else p$range_min,
                           numeric(1L)),
            max   = vapply(ir_val$parameters,
                           function(p) if (is.na(p$range_max)) NA_real_ else p$range_max,
                           numeric(1L)),
            stringsAsFactors = FALSE
          )
        }

        sections <- shiny::isolate(input$sections) %||% character(0L)
        title    <- shiny::isolate(input$title)
        author   <- shiny::isolate(input$author)

        rmarkdown::render(
          input         = template,
          output_format = output_format,
          output_file   = file,
          params        = list(
            report_title  = if (nzchar(trimws(title))) title else "rhapsody Report",
            report_author = author,
            report_date   = format(Sys.Date()),
            model_source  = ir_val$raw_source,
            params_df     = params_df,
            sim_data      = sim_df,
            sections      = sections
          ),
          envir = new.env(parent = baseenv()),
          quiet = TRUE
        )
      }
    }

    output$html <- shiny::downloadHandler(
      filename = function() {
        paste0("report-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".html")
      },
      content = make_render("html_document")
    )

    output$word <- shiny::downloadHandler(
      filename = function() {
        paste0("report-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".docx")
      },
      content = make_render("word_document")
    )

    output$rscript <- shiny::downloadHandler(
      filename = function() {
        paste0("model-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".R")
      },
      content = function(file) {
        shiny::req(ir())
        desolve_export(ir(), file)
      }
    )
  })
}
