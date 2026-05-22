test_that("compute_fft_spectrum: sine wave peak at correct frequency", {
  # 1000 points, dt=0.1 s → sample rate 10 Hz, Nyquist 5 Hz
  # 2 Hz sine → peak at freq = 2
  dt <- 0.1
  t  <- seq(0, 99.9, by = dt)
  x  <- sin(2 * pi * 2 * t)
  spec <- compute_fft_spectrum(x, dt = dt)
  peak_freq <- spec$freq[which.max(spec$magnitude)]
  expect_equal(peak_freq, 2, tolerance = 0.05)
})

test_that("compute_fft_spectrum: trim_frac drops leading data", {
  # First 4 points are huge transients; 96 remaining ≈ 1 Hz sine
  dt <- 0.1
  t_trim <- seq(0, 9.5, by = dt)   # 96 points
  sine_part <- sin(2 * pi * 1 * t_trim)
  x <- c(1e6, 1e6, 1e6, 1e6, sine_part)   # 100 points total
  spec <- compute_fft_spectrum(x, dt = dt, trim_frac = 0.04)
  peak_freq <- spec$freq[which.max(spec$magnitude)]
  expect_equal(peak_freq, 1, tolerance = 0.2)
})

test_that("find_fft_peaks: detects two distinct frequency peaks", {
  dt <- 0.01
  t  <- seq(0, 99.99, by = dt)   # 10 000 points
  x  <- sin(2 * pi * 5 * t) + 0.5 * sin(2 * pi * 12 * t)
  spec  <- compute_fft_spectrum(x, dt = dt)
  peaks <- find_fft_peaks(spec, n_peaks = 5L)
  expect_true(any(abs(peaks$Frequency - 5)  < 0.5))
  expect_true(any(abs(peaks$Frequency - 12) < 0.5))
})
