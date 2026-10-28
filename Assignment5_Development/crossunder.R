# crossunder(arr1, arr2)
# Returns a character vector the same length as the inputs:
#   "True"  = arr1 crossed UNDER arr2 at this point
#   "False" = no crossunder at this point
# The first value is "None" because there is no earlier point to compare.

crossunder <- function(arr1, arr2) {
  # Check if the length of both arrays is the same
  if (length(arr1) != length(arr2)) {
    stop("Both arrays should have the same length")
  }

  n <- length(arr1)
  if (n == 0) return(character(0))

  # Initialize a vector to store the crossunder signals
  crossunder_signals <- character(n)
  crossunder_signals[1] <- "None"

  # Check for crossunder signals at each data point (starting from the second one)
  for (i in seq_len(n)[-1]) {
    # If any value is missing we cannot tell, so report "False"
    if (is.na(arr1[i]) || is.na(arr2[i]) || is.na(arr1[i - 1]) || is.na(arr2[i - 1])) {
      crossunder_signals[i] <- "False"
    } else if (arr1[i] < arr2[i] && arr1[i - 1] >= arr2[i - 1]) {
      crossunder_signals[i] <- "True"
    } else {
      crossunder_signals[i] <- "False"
    }
  }

  return(crossunder_signals)
}
