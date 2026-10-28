# stdev(data)
# This is the POPULATION standard deviation: squared differences are divided by n,
# not n - 1, which is what the formula in the assignment uses. R's built-in sd()
# divides by n - 1, so its answer is a little bigger.

stdev <- function(data) {
  # Calculate the mean of the data
  mean_value <- sum(data) / length(data)

  # Calculate the differences between the data points and the mean
  diff_values <- data - mean_value

  # Calculate the squared differences
  squared_diff <- diff_values * diff_values

  # Calculate the variance (mean of squared differences)
  variance <- sum(squared_diff) / length(squared_diff)

  # Calculate the standard deviation (square root of the variance)
  standard_deviation <- sqrt(variance)

  return(standard_deviation)
}
