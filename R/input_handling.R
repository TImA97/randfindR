#' convert vector to matrix of first order dependencies
#' @description helper function to transform a given vector to a matrix of first
#' order dependencies, specifying the frequency of all possible 2-digit long
#' sequences
#' @param x vector of random numbers
#' @param options number of available options in sequence
#'
#' @noRd
convert_to_matrix <- function(x, options) {
  matr <- matrix(data = 0, nrow = options, ncol = options)
  for (i in 1:(length(x) - 1)) {
    current_value <- x[i]
    next_value <- x[i + 1]
    matr[current_value, next_value] <- matr[current_value, next_value] + 1
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


# TODO: Add function that returns the number of unqiue options in a given vector
#' function returns the number of unique response in a vector
#' @param x vector
#' @return number of unique responses in \code{x}
#' @noRd
get_number_unique_responses <- function(x) {
  length(unique(x))
}
