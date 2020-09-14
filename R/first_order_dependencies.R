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
      if (matr[i,j] > 0) {
        sum <- sum + matr[i,j] - 1
      }
    }
  }

  return(sum)
}

#' Compute RNG index by Evans (1978)
#'
#' @param x vector of random numbers
#' @param possible_responses number of available options in sequence
#' @return RNG index of \code{x}
rng_index <- function(x, possible_responses) {

  ## decide whether to include transition from last to first number

  x <- to_numeric(x)
  matr <- convert_to_matrix(x, possible_responses)

  dividend  <- 0
  for (i in 1:possible_responses) {
    for (j in 1:possible_responses) {
      if (matr[i,j] > 1) {
        dividend  <- dividend  + log10(matr[i,j]) *  matr[i,j]
      }
    }
  }

  divisor <- 0
  rowMarginals <- rowSums(matr)
  for (i in 1:possible_responses) {
    rowMarginal <- rowMarginals[i]
    if (rowMarginal > 1) {
      divisor <- divisor + log10(rowMarginal) * rowMarginal;
    }
  }

  result <- dividend / divisor
  return(result)
}

#' Compute repetitions as measure of randomness
#' @description Compute number of times a value is repeated in the next round
#' @param x vector of random numbers
#' @param oprions number of available options in sequence
#' @return repetitions of \code{x}
repetitions <- function(x, options) {
  x <- to_numeric(x)
  matr <- convert_to_matrix(x, options)
  sum <- sum(diag(matr))
  return(sum)
}


#' convert vector to matrix of first order dependencies
#' @description helper function to transform a given vector to a matrix of first order
#' dependencies, specifying the frequency of all possible 2-digit long sequences
#' @param x vector of random numbers
#' @param possible_responses number of available options in sequence
#' @keywords internal
convert_to_matrix <- function(x, possible_responses) {
  matr <- matrix(data = 0, nrow = possible_responses, ncol = possible_responses)
  for (i in 1:(length(x) - 1)) {
    current_value <- x[i]
    next_value <- x[i + 1]
    matr[current_value, next_value] <- matr[current_value, next_value] + 1
  }

  return(matr)
}
