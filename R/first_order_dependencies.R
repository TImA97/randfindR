#' Compute Digram Repetitions (see Ginsburg & Karpiuk, 1994)
#'
#' @param x vector of random numbers
#' @param possible_responses number of available options in sequence
#' @return digram repetition of \code{x}
digram_rep <- function(x, possible_responses) {

  x <- to_numeric(x)
  matr <- convert_to_matrix(x, possible_responses)

  # compute sum of (cell values - 1)
  sum <- 0
  for (i in 1:possible_responses) {
    for (j in 1:possible_responses) {
      sum <- sum + matr[i,j] - 1
    }
  }

  return(sum)
}

convert_to_matrix <- function(x, possible_responses) {
  matr <- matrix(data = 0, nrow = possible_responses, ncol = possible_responses)
  for (i in 1:(length(x) - 1)) {
    current_value <- x[i]
    next_value <- x[i + 1]
    matr[current_value, next_value] <- matr[current_value, next_value] + 1
  }
  return(matr)
}
