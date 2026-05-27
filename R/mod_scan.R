#' Parameter scan module — UI
#' @param id Module namespace id
#' @export
mod_scan_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "p-2",
    shiny::fluidRow(
      shiny::column(3, shiny::uiOutput(ns("param_sel_ui"))),
      shiny::column(3, shiny::numericInput(ns("from"), "From", value = 0.5)),
      shiny::column(3, shiny::numericInput(ns("to"),   "To",   value = 2)),
      shiny::column(3,
        shiny::sliderInput(
          ns("steps"), "Steps", min = 2, max = 50, value = 8, step = 1
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(4,
        shiny::checkboxInput(ns("log_scale"), "Log scale", value = FALSE)
      ),
      shiny::column(4,
        shiny::selectInput(
          ns("mode"), "Display",
          choices = c(
            "Overlay curves" = "overlay",
            "Parameter plot" = "param_plot"
          )
        )
      ),
      shiny::column(4, shiny::uiOutput(ns("xaxis_ui")))
    ),
    shiny::uiOutput(ns("vars_ui")),
    shiny::div(
      class = "d-flex align-items-center gap-2 mb-2",
      shiny::actionButton(ns("run_scan"), "Run Scan",
                          class = "btn btn-secondary btn-sm"),
      shiny::uiOutput(ns("view_toggle_ui")),
      shiny::uiOutput(ns("csv_dl_ui"))
    ),
    shiny::uiOutput(ns("scan_error_ui")),
    shiny::uiOutput(ns("scan_output_ui"))
  )
}

#' Parameter scan module — server
#'
#' @param id Module namespace id
#' @param ir Reactive returning Canonical IR (or NULL)
#' @param param_state Return value of mod_params_server
#' @return Invisibly NULL (called for its side effects — renders scan plot)
#' @export
mod_scan_server <- function(id, ir, param_state) {
  shiny::moduleServer(id, function(input, output, session) {

    scan_result <- shiny::reactiveVal(NULL)
    scan_error  <- shiny::reactiveVal(NULL)
    show_data   <- shiny::reactiveVal(FALSE)

    output$param_sel_ui <- shiny::renderUI({
      shiny::req(ir())
      shiny::selectInput(session$ns("scan_param"), "Parameter",
                         choices = names(ir()$parameters))
    })

    shiny::observeEvent(input$scan_param, {
      shiny::req(ir(), nzchar(input$scan_param))
      p <- ir()$parameters[[input$scan_param]]
      if (is.null(p)) return()
      val      <- p$value %||% 1
      from_val <- if (!is.na(p$range_min)) p$range_min else val * 0.5
      to_val   <- if (!is.na(p$range_max)) p$range_max else val * 2
      shiny::updateNumericInput(session, "from", value = from_val)
      shiny::updateNumericInput(session, "to",   value = to_val)
      shiny::updateCheckboxInput(
        session, "log_scale", value = isTRUE(p$log_scale)
      )
    })

    output$vars_ui <- shiny::renderUI({
      shiny::req(ir())
      var_choices <- c(names(ir()$states), names(ir()$auxiliary))
      shiny::checkboxGroupInput(
        session$ns("scan_vars"), "Variables",
        choices  = var_choices,
        selected = var_choices[1L],
        inline   = TRUE
      )
    })

    output$xaxis_ui <- shiny::renderUI({
      res <- scan_result()
      if (is.null(input$mode) || input$mode != "overlay") return(NULL)
      if (is.null(res)) return(NULL)
      ok_mask  <- !vapply(res$results, inherits, logical(1L), "rhapsody_error")
      first_df <- res$results[ok_mask][[1L]]
      if (is.null(first_df)) return(NULL)
      choices <- c("time", setdiff(names(first_df), "time"))
      shiny::selectInput(
        session$ns("xvar"), "X axis",
        choices = choices, selected = "time", width = "100px"
      )
    })

    shiny::observeEvent(input$run_scan, {
      shiny::req(
        ir(), input$scan_param, input$from, input$to,
        input$steps, input$scan_vars
      )
      scan_error(NULL)
      spec <- list(
        parameter = input$scan_param,
        from      = input$from,
        to        = input$to,
        steps     = as.integer(input$steps),
        log_scale = isTRUE(input$log_scale)
      )
      result <- tryCatch(
        scan_params(
          ir        = ir(),
          params    = param_state$params(),
          ics       = param_state$ics(),
          scan_spec = spec
        ),
        error = function(e) {
          scan_error(conditionMessage(e))
          NULL
        }
      )
      scan_result(result)
      show_data(FALSE)
    })

    # ── Shared wide-format data.frame ─────────────────────────────
    scan_wide <- shiny::reactive({
      res  <- scan_result()
      vars <- input$scan_vars
      if (is.null(res) || is.null(vars) || length(vars) == 0L) return(NULL)
      ok_mask    <- !vapply(
        res$results, inherits, logical(1L), "rhapsody_error"
      )
      valid_dfs  <- res$results[ok_mask]
      valid_vals <- res$param_values[ok_mask]
      if (length(valid_dfs) == 0L) return(NULL)
      out <- data.frame(time = valid_dfs[[1L]]$time, check.names = FALSE)
      for (i in seq_along(valid_dfs)) {
        df  <- valid_dfs[[i]]
        pv  <- signif(valid_vals[i], 6L)
        lbl <- paste0(res$param_name, "=", pv)
        for (vr in vars) {
          if (!vr %in% names(df)) next
          out[[paste0(vr, "[", lbl, "]")]] <- df[[vr]]
        }
      }
      out
    })

    # ── Toggle ────────────────────────────────────────────────────
    output$view_toggle_ui <- shiny::renderUI({
      if (is.null(scan_result())) return(NULL)
      shiny::actionButton(
        session$ns("toggle_view"),
        label = if (show_data()) "Plot" else "Data",
        class = "btn btn-outline-secondary btn-sm"
      )
    })

    shiny::observeEvent(input$toggle_view, {
      show_data(!show_data())
    })

    # ── CSV download ──────────────────────────────────────────────
    output$csv_dl_ui <- shiny::renderUI({
      if (is.null(scan_result())) return(NULL)
      shiny::downloadButton(
        session$ns("scan_csv"), "Download CSV",
        class = "btn btn-outline-secondary btn-sm"
      )
    })

    output$scan_csv <- shiny::downloadHandler(
      filename = function() {
        res <- scan_result()
        paste0("scan-", res$param_name, "-",
               format(Sys.time(), "%Y%m%d-%H%M%S"), ".csv")
      },
      content = function(file) {
        out <- shiny::isolate(scan_wide())
        shiny::req(!is.null(out))
        utils::write.csv(out, file, row.names = FALSE)
      }
    )

    output$scan_error_ui <- shiny::renderUI({
      err <- scan_error()
      if (is.null(err)) return(NULL)
      shiny::div(class = "alert alert-danger py-1 px-2 small", err)
    })

    # ── Plot / Data output ────────────────────────────────────────
    output$scan_output_ui <- shiny::renderUI({
      if (isTRUE(show_data())) {
        shiny::div(
          style = "overflow: auto; max-height: 280px; font-size: 0.75rem;",
          shiny::tableOutput(session$ns("scan_table"))
        )
      } else {
        plotly::plotlyOutput(session$ns("scan_plot"), height = "280px")
      }
    })

    output$scan_table <- shiny::renderTable(
      scan_wide(),
      digits    = 4L,
      striped   = TRUE,
      hover     = TRUE,
      bordered  = FALSE,
      spacing   = "xs",
      width     = "100%",
      na        = ""
    )

    output$scan_plot <- plotly::renderPlotly({
      res  <- scan_result()
      vars <- input$scan_vars
      if (is.null(res) || is.null(vars) || length(vars) == 0L) {
        return(plotly::plotly_empty())
      }

      ok_mask    <- !vapply(
        res$results, inherits, logical(1L), "rhapsody_error"
      )
      valid_dfs  <- res$results[ok_mask]
      valid_vals <- res$param_values[ok_mask]
      if (length(valid_dfs) == 0L) return(plotly::plotly_empty())

      p <- plotly::plot_ly()

      if (input$mode == "overlay") {
        xvar <- if (!is.null(input$xvar) &&
                    input$xvar %in% names(valid_dfs[[1L]])) {
          input$xvar
        } else {
          "time"
        }
        for (i in seq_along(valid_dfs)) {
          df <- valid_dfs[[i]]
          pv <- signif(valid_vals[i], 4L)
          for (vr in vars) {
            if (!vr %in% names(df) || vr == xvar) next
            p <- plotly::add_lines(
              p, x = df[[xvar]], y = df[[vr]],
              name = paste0(vr, " (", res$param_name, "=", pv, ")")
            )
          }
        }
        p <- plotly::layout(p,
          xaxis = list(title = xvar),
          yaxis = list(title = "Value"))
      } else {
        for (vr in vars) {
          final_vals <- vapply(valid_dfs, function(df) {
            if (vr %in% names(df)) tail(df[[vr]], 1L) else NA_real_
          }, numeric(1L))
          p <- plotly::add_lines(
            p, x = valid_vals, y = final_vals, name = vr
          )
        }
        p <- plotly::layout(p,
          xaxis = list(title = res$param_name),
          yaxis = list(title = "Final value"))
      }
      p
    })
  })
}
