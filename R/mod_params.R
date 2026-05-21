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

    # ── Helper: evaluate an IC expression against current param values ──
    init_val_for <- function(nm) {
      ic_expr <- ir()$states[[nm]]$init_expr %||% "0"
      direct <- suppressWarnings(as.numeric(ic_expr))
      if (!is.na(direct)) return(direct)
      parms <- lapply(ir()$parameters, `[[`, "value")
      tryCatch(
        eval(parse(text = ic_expr), envir = list2env(parms, parent = baseenv())),
        error   = function(e) 0,
        warning = function(w) 0
      )
    }

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
        init_val <- init_val_for(nm)
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
    # Tracks observer handles so old observers are destroyed before
    # new ones are created, preventing unbounded accumulation across
    # model edits.
    sync_obs <- list()

    shiny::observe({
      shiny::req(ir())
      # Destroy previous observers before creating new ones
      for (obs in sync_obs) try(obs$destroy(), silent = TRUE)
      sync_obs <<- list()

      pnames <- names(ir()$parameters)
      new_obs <- lapply(pnames, function(nm) {
        local({
          nm_      <- nm
          slider_  <- paste0("p_", nm_)
          numeric_ <- paste0("n_", nm_)

          obs1 <- shiny::observeEvent(input[[slider_]], {
            if (!isTRUE(all.equal(input[[slider_]], input[[numeric_]]))) {
              shiny::updateNumericInput(session, numeric_, value = input[[slider_]])
            }
          }, ignoreInit = TRUE)

          obs2 <- shiny::observeEvent(input[[numeric_]], {
            shiny::req(!is.na(input[[numeric_]]))
            p_   <- ir()$parameters[[nm_]]
            rng_ <- .param_range(p_)
            clamped <- max(rng_$min, min(rng_$max, input[[numeric_]]))
            if (!isTRUE(all.equal(input[[slider_]], clamped))) {
              shiny::updateSliderInput(session, slider_, value = clamped)
            }
          }, ignoreInit = TRUE)

          list(obs1, obs2)
        })
      })
      sync_obs <<- unlist(new_obs, recursive = FALSE)
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
          init_val_for(nm)
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
  v <- param$value
  if (v == 0) return(list(min = -10, max = 10))
  lo <- signif(v * 10, 2)
  hi <- signif(v / 10, 2)
  list(min = min(lo, hi), max = max(lo, hi))
}
