#' Parameter panel module — UI
#'
#' Renders a live-mode toggle, then a slider+numeric pair for every
#' parameter in the parsed IR, followed by a numeric input for every
#' state initial condition.
#'
#' @param id Module namespace id
#' @export
mod_params_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    # Live-mode toggle
    shiny::div(
      class = "d-flex align-items-center gap-2 p-2 border-bottom mb-1",
      shiny::checkboxInput(ns("live"), "Live", value = TRUE),
      shiny::tags$span(class = "text-muted small", "(300 ms debounce)")
    ),
    # Parameter sliders — built server-side from IR
    shiny::tags$h6("Parameters", class = "px-2 mb-1 mt-2"),
    shiny::uiOutput(ns("param_ui")),
    # Initial condition inputs — built server-side from IR
    shiny::tags$h6("Initial Conditions", class = "px-2 mb-1 mt-3"),
    shiny::uiOutput(ns("ic_ui"))
  )
}

#' Parameter panel module — server
#'
#' @param id Module namespace id
#' @param ir Reactive returning the Canonical IR from parse_model(), or NULL
#' @return Named list of reactives:
#'   \describe{
#'     \item{params}{Named list of current parameter values (immediate)}
#'     \item{ics}{Named list of current IC values (immediate)}
#'     \item{live_params}{Debounced (300 ms) version of params}
#'     \item{live_ics}{Debounced (300 ms) version of ics}
#'     \item{live_mode}{Reactive logical — TRUE when live checkbox is checked}
#'   }
#' @export
mod_params_server <- function(id, ir) {
  shiny::moduleServer(id, function(input, output, session) {

    # ── Dynamic parameter UI ──────────────────────────────────
    output$param_ui <- shiny::renderUI({
      shiny::req(ir())
      lapply(names(ir()$parameters), function(nm) {
        p   <- ir()$parameters[[nm]]
        rng <- .param_range(p)
        shiny::tagList(
          shiny::tags$label(nm, class = "form-label small mb-0 px-2"),
          shiny::div(
            class = "d-flex align-items-center gap-1 px-2 mb-1",
            shiny::div(
              class = "flex-grow-1",
              shiny::sliderInput(
                session$ns(paste0("p_", nm)),
                label = NULL,
                min   = rng$min,
                max   = rng$max,
                value = p$value,
                width = "100%",
                ticks = FALSE
              )
            ),
            shiny::numericInput(
              session$ns(paste0("n_", nm)),
              label = NULL,
              value = p$value,
              width = "80px"
            )
          )
        )
      })
    })

    # ── Dynamic IC UI ─────────────────────────────────────────
    output$ic_ui <- shiny::renderUI({
      shiny::req(ir())
      lapply(names(ir()$states), function(nm) {
        init_val <- tryCatch(
          as.numeric(ir()$states[[nm]]$init_expr %||% "0"),
          warning = function(w) 0,
          error   = function(e) 0
        )
        shiny::div(
          class = "d-flex align-items-center gap-2 px-2 mb-1",
          shiny::tags$label(
            paste0(nm, "[0]"),
            class = "form-label small mb-0",
            style = "min-width: 60px"
          ),
          shiny::numericInput(
            session$ns(paste0("ic_", nm)),
            label = NULL,
            value = init_val,
            width = "90px"
          )
        )
      })
    })

    # ── Bidirectional slider ↔ numeric sync ───────────────────
    # Sets up two observers per parameter each time the IR changes.
    # Old observers from previous IR survive but their input IDs no
    # longer exist in the DOM, so they never fire — effectively inert.
    shiny::observe({
      shiny::req(ir())
      pnames <- names(ir()$parameters)
      lapply(pnames, function(nm) {
        local({
          nm_      <- nm
          slider_  <- paste0("p_", nm_)
          numeric_ <- paste0("n_", nm_)

          # Slider moved → update numeric display
          shiny::observeEvent(input[[slider_]], {
            if (!isTRUE(all.equal(input[[slider_]], input[[numeric_]]))) {
              shiny::updateNumericInput(session, numeric_, value = input[[slider_]])
            }
          }, ignoreInit = TRUE)

          # Numeric typed → clamp slider; retain exact value in numeric
          shiny::observeEvent(input[[numeric_]], {
            shiny::req(input[[numeric_]])
            p_   <- ir()$parameters[[nm_]]
            rng_ <- .param_range(p_)
            clamped <- max(rng_$min, min(rng_$max, input[[numeric_]]))
            if (!isTRUE(all.equal(input[[slider_]], clamped))) {
              shiny::updateSliderInput(session, slider_, value = clamped)
            }
          }, ignoreInit = TRUE)
        })
      })
    })

    # ── Collect current values (fallback to IR default if UI unready) ─
    current_params <- shiny::reactive({
      shiny::req(ir())
      pnames <- names(ir()$parameters)
      vals <- lapply(pnames, function(nm) {
        v <- input[[paste0("n_", nm)]]
        if (is.null(v)) ir()$parameters[[nm]]$value else v
      })
      setNames(vals, pnames)
    })

    current_ics <- shiny::reactive({
      shiny::req(ir())
      snames <- names(ir()$states)
      vals <- lapply(snames, function(nm) {
        v <- input[[paste0("ic_", nm)]]
        if (is.null(v)) {
          suppressWarnings(
            as.numeric(ir()$states[[nm]]$init_expr %||% "0")
          )
        } else {
          v
        }
      })
      setNames(vals, snames)
    })

    # ── Debounced versions for live mode ──────────────────────
    live_params <- shiny::debounce(current_params, 300)
    live_ics    <- shiny::debounce(current_ics,    300)

    list(
      params      = current_params,
      ics         = current_ics,
      live_params = live_params,
      live_ics    = live_ics,
      live_mode   = shiny::reactive(isTRUE(input$live))
    )
  })
}

# Internal helper — computes slider min/max from a parameter entry.
# Prefers inline range from model source; auto-infers [val/10, val*10]
# when no range was specified.
.param_range <- function(param) {
  if (!is.na(param$range_min) && !is.na(param$range_max)) {
    return(list(min = param$range_min, max = param$range_max))
  }
  val <- if (param$value == 0) 1 else abs(param$value)
  list(min = signif(val / 10, 2), max = signif(val * 10, 2))
}
