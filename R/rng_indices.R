#' Compute RNG index
#'
#' @param x vector of random numbers
#' @param options number of available options in sequence
#' @return RNG index of \code{x}
#'
#' @details
#'
#' This function takes a vector \code{x} and computes an index that reflects
#' whether the distribution of response pairs in the sequence is uneven given
#' the number of possible \code{options}. The values of this index range between
#' 0 and 1. A value of '1' would indicate complete redundancy of response pairs,
#' whereas a value of '0' would indicate an even distribution of response pairs.
#'
#' @export
#'
#' @references
#'
#' Evans, F.J. Monitoring attention deployment by random number generation:
#' An index to measure subjective randomness.
#' Bull. Psychon. Soc. 12, 35–38 (1978).
#' \url{https://doi.org/10.3758/BF03329617}
rng_index <- function(x, options) {


  x <- to_numeric(x)
  min_options <- 2
  base_checks(x, options, min_options)
  matr <- convert_to_matrix(x, options)

  dividend  <- 0
  for (i in 1:options) {
    for (j in 1:options) {
      if (matr[i,j] > 1) {
        dividend  <- dividend  + log10(matr[i,j]) *  matr[i,j]
      }
    }
  }

  divisor <- 0
  row_marginals <- rowSums(matr)
  for (i in 1:options) {
    row_marginal <- row_marginals[i]
    if (row_marginal > 1) {
      divisor <- divisor + log10(row_marginal) * row_marginal;
    }
  }

  result <- dividend / divisor
  return(result)
}
