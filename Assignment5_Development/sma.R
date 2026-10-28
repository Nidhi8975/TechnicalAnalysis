# sma.R
# Simple Moving Average (SMA). Base R only, no libraries.
#
# sma(data, period)
#   data   = numeric vector (for example closing prices)
#   period = how many values go in each window
# Returns the average of each window. The first value is the average of
# data[1] to data[period], the second is data[2] to data[period + 1], and so on,
# so the result has (length(data) - period + 1) values.

sma <- function(data, period) {
  # Check if the length of data is less than the specified period
  if (length(data) < period) {
    stop("Data length should be greater than or equal to the period")
  }

  # Initialize a vector to store the SMA values
  sma_values <- numeric(length(data) - period + 1)

  # Calculate SMA for each window of 'period' data points
  for (i in 1:(length(data) - period + 1)) {
    current_window <- data[i:(i + period - 1)]      # the values in this window
    sma_values[i] <- sum(current_window) / period   # average of the window
  }

  return(sma_values)
}
