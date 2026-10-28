# stoch_rsi.R
# Stochastic RSI (StochRSI). Base R only.
# This file needs rsi() from rsi.R and sma() from sma.R.
#
# stoch_rsi(data, period, k_period, d_period)
# Returns a list with:
#   k_line = SMA of the scaled RSI (0 to 1)
#   d_line = SMA of the k_line
#
# Things to know:
#  - rsi() has NA values at the start, so the first k_line and d_line values are
#    NA too (a window that touches an NA gives NA).
#  - sma() returns a shorter vector, so k_line is (k_period - 1) shorter than data
#    and d_line is (d_period - 1) shorter than k_line.
#  - rsi() needs more data points than `period`. The example in the assignment
#    uses period = 14 with only 10 values, which cannot work, so my examples use 5.

stoch_rsi <- function(data, period, k_period, d_period) {
  # Load the helper functions if they are not loaded yet
  if (!exists("rsi", mode = "function")) source("rsi.R")
  if (!exists("sma", mode = "function")) source("sma.R")

  # Calculate the RSI
  rsi_values <- rsi(data, period)

  # Calculate the StochRSI (RSI scaled between its lowest and highest value)
  min_rsi <- min(rsi_values, na.rm = TRUE)  
  max_rsi <- max(rsi_values, na.rm = TRUE)

  if (max_rsi == min_rsi) {
    # If RSI never changes there is nothing to scale, so use 0 
    k_values <- ifelse(is.na(rsi_values), NA_real_, 0)
  } else {
    k_values <- (rsi_values - min_rsi) / (max_rsi - min_rsi)
  }

  # Calculate the %K line 
  k_line <- sma(k_values, k_period)

  # Calculate the %D line 
  d_line <- sma(k_line, d_period)

  # Return the %K and %D lines as a list
  result <- list(k_line = k_line, d_line = d_line)

  return(result)
}
