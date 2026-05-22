#' Compute the one-sided amplitude spectrum of a signal
#'
#' Pure helper function — no Shiny dependencies, fully testable.
#'
#' @param x Numeric vector (the signal)
#' @param dt Sample interval (time between consecutive values)
#' @param window "None" | "Hann" | "Hamming" | "Blackman"
#' @param trim_frac Fraction of leading samples to drop before FFT (0–1)
#' @return data.frame with columns \code{freq} and \code{magnitude}
#' @export
compute_fft_spectrum <- function(x, dt, window = "None", trim_frac = 0) {
  n_orig <- length(x)
  trim_n <- floor(trim_frac * n_orig)
  x      <- x[seq(trim_n + 1L, n_orig)]
  n      <- length(x)
  if (n < 4L) stop("Insufficient data points for FFT after trimming.")

  w <- switch(window,
    Hann     = 0.5 * (1 - cos(2 * pi * (0:(n - 1L)) / (n - 1L))),
    Hamming  = 0.54 - 0.46 * cos(2 * pi * (0:(n - 1L)) / (n - 1L)),
    Blackman = 0.42 - 0.5  * cos(2 * pi * (0:(n - 1L)) / (n - 1L)) +
               0.08 * cos(4 * pi * (0:(n - 1L)) / (n - 1L)),
    rep(1, n)   # "None" — rectangular
  )

  fft_out   <- fft(x * w)
  n_half    <- floor(n / 2L) + 1L
  magnitude <- Mod(fft_out[seq_len(n_half)])
  freq      <- (seq_len(n_half) - 1L) / (n * dt)

  data.frame(freq = freq, magnitude = magnitude)
}

#' Find the top-N local maxima in an FFT spectrum
#'
#' @param spectrum_df data.frame returned by \code{compute_fft_spectrum}
#' @param n_peaks Maximum number of peaks to return
#' @return data.frame with columns Frequency, Magnitude, Period
#' @export
find_fft_peaks <- function(spectrum_df, n_peaks = 10L) {
  mag <- spectrum_df$magnitude
  n   <- length(mag)
  is_peak <- c(
    FALSE,
    mag[-c(1L, n)] > mag[-c(n - 1L, n)] & mag[-c(1L, n)] > mag[-c(1L, 2L)],
    FALSE
  )
  is_peak[1L] <- FALSE   # exclude DC component
  peak_idx <- which(is_peak)
  peak_idx <- peak_idx[order(mag[peak_idx], decreasing = TRUE)]
  peak_idx <- head(peak_idx, n_peaks)

  if (length(peak_idx) == 0L) {
    return(data.frame(Frequency = numeric(0L), Magnitude = numeric(0L),
                      Period = numeric(0L)))
  }
  f <- spectrum_df$freq[peak_idx]
  data.frame(
    Frequency = f,
    Magnitude = mag[peak_idx],
    Period    = ifelse(f > 0, 1 / f, Inf)
  )
}

#' FFT analysis module — UI
#' @param id Module namespace id
#' @export
mod_fft_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "p-2",
    shiny::fluidRow(
      shiny::column(4,
        shiny::uiOutput(ns("var_sel_ui"))
      ),
      shiny::column(4,
        shiny::selectInput(ns("window"), "Window",
          choices  = c("None", "Hann", "Hamming", "Blackman"),
          selected = "Hann"
        )
      ),
      shiny::column(4,
        shiny::numericInput(ns("trim_frac"), "Transient trim (0–1)",
                            value = 0.1, min = 0, max = 0.9, step = 0.05)
      )
    ),
    plotly::plotlyOutput(ns("spectrum_plot"), height = "250px"),
    shiny::tags$h6("Top Peaks", class = "mt-2 mb-1"),
    shiny::tableOutput(ns("peaks_table"))
  )
}

#' FFT analysis module — server
#'
#' @param id Module namespace id
#' @param solve_result Reactive returning a data.frame (solve output) or
#'   an object of class "rhapsody_error"
#' @export
mod_fft_server <- function(id, solve_result) {
  shiny::moduleServer(id, function(input, output, session) {

    output$var_sel_ui <- shiny::renderUI({
      res <- solve_result()
      if (is.null(res) || inherits(res, "rhapsody_error")) return(NULL)
      choices <- setdiff(names(res), "time")
      shiny::selectInput(session$ns("variable"), "Variable",
                         choices = choices, selected = choices[1L])
    })

    spectrum <- shiny::reactive({
      res <- solve_result()
      shiny::req(is.data.frame(res), input$variable %in% names(res))
      x  <- res[[input$variable]]
      dt <- if (nrow(res) > 1L) res$time[2L] - res$time[1L] else 1
      compute_fft_spectrum(
        x         = x,
        dt        = dt,
        window    = input$window    %||% "Hann",
        trim_frac = input$trim_frac %||% 0.1
      )
    })

    output$spectrum_plot <- plotly::renderPlotly({
      spec <- tryCatch(spectrum(), error = function(e) NULL)
      if (is.null(spec)) return(plotly::plotly_empty())
      plotly::plot_ly(spec, x = ~freq, y = ~magnitude, type = "scatter",
                      mode = "lines", name = "Amplitude") |>
        plotly::layout(
          xaxis = list(title = "Frequency"),
          yaxis = list(title = "Magnitude")
        )
    })

    output$peaks_table <- shiny::renderTable({
      spec <- tryCatch(spectrum(), error = function(e) NULL)
      if (is.null(spec)) return(NULL)
      find_fft_peaks(spec, n_peaks = 10L)
    }, digits = 4)
  })
}
