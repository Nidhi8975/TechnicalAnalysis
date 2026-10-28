# crossover(arr1, arr2)
# Returns a character vector the same length as the inputs:
#   "Up"   = arr1 crossed ABOVE arr2 at this point
#   "Down" = arr1 crossed BELOW arr2 at this point
#   "None" = no cross
# The first value is always "None" because there is no earlier point to compare.

crossover <- function(arr1, arr2) {
  # Check if the length of both arrays is the same
  if (length(arr1) != length(arr2)) {
    stop("Both arrays should have the same length")
  }

  n <- length(arr1)
  if (n == 0) return(character(0))

  # Initialize a vector to store the crossover signals
  crossover_signals <- character(n)
  crossover_signals[1] <- "None"

  # Check for crossovers at each data point (starting from the second one)
  for (i in seq_len(n)[-1]) {
    # If any value is missing we cannot tell, so report "None"
    if (is.na(arr1[i]) || is.na(arr2[i]) || is.na(arr1[i - 1]) || is.na(arr2[i - 1])) {
      crossover_signals[i] <- "None"
    } else if (arr1[i] > arr2[i] && arr1[i - 1] <= arr2[i - 1]) {
      crossover_signals[i] <- "Up"
    } else if (arr1[i] < arr2[i] && arr1[i - 1] >= arr2[i - 1]) {
      crossover_signals[i] <- "Down"
    } else {
      crossover_signals[i] <- "None"
    }
  }

  return(crossover_signals)
}
