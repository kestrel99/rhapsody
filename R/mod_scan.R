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
      )
    ),
    shiny::uiOutput(ns("vars_ui")),
    shiny::div(
      class = "d-flex gap-2 mb-2",
      shiny::actionButton(ns("run_scan"), "Run Scan",
                          class = "btn btn-secondary btn-sm"),
      shiny::uiOutput(ns("csv_dl_ui"))
    ),
    shiny::uiOutput(ns("scan_error_ui")),
    plotly::plotlyOutput(ns("scan_plot"), height = "280px")
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
      shiny::updateNumericInput(session,  "from",      value = from_val)
      shiny::updateNumericInput(session,  "to",        value = to_val)
      shiny::updateCheckboxInput(session, "log_scale", value = isTRUE(p$log_scale))
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
    })

    output$csv_dl_ui <- shiny::renderUI({
      res <- scan_result()
      if (is.null(res)) return(NULL)
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
        res  <- scan_result()
        vars <- shiny::isolate(input$scan_vars)
        shiny::req(!is.null(res), length(vars) > 0L)
        ok_mask    <- !vapply(
          res$results, inherits, logical(1L), "rhapsody_error"
        )
        valid_dfs  <- res$results[ok_mask]
        valid_vals <- res$param_values[ok_mask]
        shiny::req(length(valid_dfs) > 0L)
        time_vec <- valid_dfs[[1L]]$time
        out <- data.frame(time = time_vec, check.names = FALSE)
        for (i in seq_along(valid_dfs)) {
          df  <- valid_dfs[[i]]
          pv  <- signif(valid_vals[i], 6L)
          lbl <- paste0(res$param_name, "=", pv)
          for (vr in vars) {
            if (!vr %in% names(df)) next
            out[[paste0(vr, "[", lbl, "]")]] <- df[[vr]]
          }
        }
        utils::write.csv(out, file, row.names = FALSE)
      }
    )

    output$scan_error_ui <- shiny::renderUI({
      err <- scan_error()
      if (is.null(err)) return(NULL)
      shiny::div(class = "alert alert-danger py-1 px-2 small", err)
    })

    output$scan_plot <- plotly::renderPlotly({
      res  <- scan_result()
      vars <- input$scan_vars
      if (is.null(res) || is.null(vars) || length(vars) == 0L) {
        return(plotly::plotly_empty())
      }

      ok_mask    <- !vapply(res$results, inherits, logical(1L), "rhapsody_error")
      valid_dfs  <- res$results[ok_mask]
      valid_vals <- res$param_values[ok_mask]
      if (length(valid_dfs) == 0L) return(plotly::plotly_empty())

      p <- plotly::plot_ly()

      if (input$mode == "overlay") {
        for (i in seq_along(valid_dfs)) {
          df <- valid_dfs[[i]]
          pv <- signif(valid_vals[i], 4L)
          for (vr in vars) {
            if (!vr %in% names(df)) next
            p <- plotly::add_lines(
              p, x = df$time, y = df[[vr]],
              name = paste0(vr, " (", res$param_name, "=", pv, ")")
            )
          }
        }
        p <- plotly::layout(p,
          xaxis = list(title = "Time"),
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
