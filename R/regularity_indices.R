#' Compute runs (see Ginsburg & Karpiuk, 1994)
#' @description Compute the variance of successive ascending or descending runs
#' @param x vector of random numbers
#' @return runs of \code{x}
runs_index <- function(x, asc = TRUE) {
  x <- to_numeric(x)

  runs <- vector(length = 1)

  # iterate over vector and count runs
  # make case distinction whether asc is 'TRUE' or 'FALSE'
  current_length <- 1
  runs_counter <- 1
  for (i in 2:length(x)) {
    if (asc == TRUE & x[i] > x[i - 1]) {
      current_length <- current_length + 1
    } else if (asc == FALSE & x[i] < x[i - 1]) {
      current_length <- current_length + 1
    } else {
      runs[runs_counter] <- current_length
      current_length <- 1
      runs_counter <- runs_counter + 1
    }
  }

  # store length of last run
  runs[runs_counter] <- current_length
  current_length <- 0

  # compute population variance of runs
  variance <- var(runs) * (length(runs) - 1) / length(runs)

  return(variance)
}
