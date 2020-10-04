#' convert vector to matrix of first order dependencies
#' @description helper function to transform a given vector to a matrix of first
#' order dependencies, specifying the frequency of all possible 2-digit long
#' sequences
#' @param x vector of random numbers
#' @param options number of available options in sequence
#' @param order indicates displacement between response pairs (default is 1)
#'
#' @noRd
convert_to_matrix <- function(x, options, order = 1, circ = TRUE) {
  matr <- matrix(data = 0, nrow = options, ncol = options)

  # count response pairs depending on the specified order
  for (i in 1:(length(x) - order)) {
    current_value <- x[i]
    next_value <- x[i + order]
    matr[current_value, next_value] <- matr[current_value, next_value] + 1
  }

  # include transition from latest to first responses
  if (circ) {
    for (i in 1:options) {
      current_value <- x[length(x) - order + i]
      next_value <- x[i]
      matr[current_value, next_value] <- matr[current_value, next_value] + 1
    }
  }

  return(matr)
}


#' function transforms given vector to numeric
#'
#' @param x vector
#' @return x_transformed
#'
#' @noRd
to_numeric <- function(x) {
  as.numeric(factor(x))
}


#' function returns the number of unique response in a vector
#' @param x vector
#' @return number of unique responses in \code{x}
#' @noRd
get_number_unique_responses <- function(x) {
  length(unique(x))
}
