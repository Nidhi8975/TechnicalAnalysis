# rsi(data, period)
# Returns a vector the same length as data. The first `period` values are NA
# because there is not enough history yet.

rsi <- function(data, period) {
  n <- length(data)

  # My own check: we need at least one value after the first `period` changes
  if (n <= period) {
    stop("Data length must be greater than the period")
  }

  # Calculate the differences between consecutive data points
  diff_values <- data[2:n] - data[1:(n - 1)]

  # Initialize two vectors to store the gains and losses (zeros to start)
  gains  <- numeric(length(diff_values))
  losses <- numeric(length(diff_values))

  # Calculate gains and losses
  for (i in seq_along(diff_values)) {
    if (diff_values[i] > 0) {
      gains[i] <- diff_values[i]
    } else {
      losses[i] <- abs(diff_values[i])
    }
  }

  # Calculate the average gains and average losses for the first 'period' data points
  avg_gain <- sum(gains[1:period]) / period
  avg_loss <- sum(losses[1:period]) / period

  # Initialize the RSI vector with NA values
  rsi_values <- rep(NA_real_, n)

  # Calculate RSI values using Wilder's smoothing method
  for (i in (period + 1):n) {
    avg_gain <- (avg_gain * (period - 1) + gains[i - 1]) / period
    avg_loss <- (avg_loss * (period - 1) + losses[i - 1]) / period

    if (avg_loss == 0) {
      # No losses: RSI is 100. If there were no gains either, call it neutral (50).
      rsi_values[i] <- ifelse(avg_gain == 0, 50, 100)
    } else {
      rs <- avg_gain / avg_loss
      rsi_values[i] <- 100 - (100 / (1 + rs))
    }
  }

  return(rsi_values)
}
