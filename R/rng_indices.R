#' Compute RNG index
#'
#' @param x vector of random numbers
#' @param options number of available options in sequence
#' @param circ indicate whether to include wrap around from end to the beginning
#' of the sequence when computing response pairs
#' @return RNG index of \code{x}
#'
#' @examples
#' rng_index(ginsburg1994, 10)
#' rng_index(ginsburg1994, 10, circ = FALSE)
#' rng_index(evans1978[, 1], 10)
#' rng_index(evans1978[, 2], 10)
#'
#' @details
#'
#' This function takes a vector \code{x} and computes an index that reflects
#' whether the distribution of response pairs in the sequence is uneven given
#' the number of possible \code{options}. The values of this index range between
#' 0 and 1. A value of 1 indicates complete redundancy of one response pair,
#' whereas a value of 0 indicates an even distribution of response pairs.
#'
#' @export
#'
#' @references
#'
#' Evans, F.J. Monitoring attention deployment by random number generation:
#' An index to measure subjective randomness.
#' Bull. Psychon. Soc. 12, 35–38 (1978).
#' \url{https://doi.org/10.3758/BF03329617}
rng_index <- function(x, options, circ = TRUE) {
  x <- to_numeric(x)
  min_options <- 2
  base_checks(x, options, min_options)
  matr <- convert_to_matrix(x, options, circ = circ)

  ## get dividend and divisor of rng_index
  dividend <- get_quotient_dividend(matr)
  divisor <- get_quotient_divisor(matr)

  ## compute and return quotient
  result <- get_quotient(dividend, divisor)
  return(result)
}

#' Compute RNG2 index
#'
#' @param x vector of random numbers
#' @param options number of available options in sequence
#' @param circ indicate whether to include wrap around from end to the beginning
#' of the sequence when computing response pairs
#' @return RNG2 index of \code{x}
#'
#' @examples
#' rng2_index(ginsburg1994, 10)
#' rng2_index(ginsburg1994, 10, circ = FALSE)
#' rng2_index(evans1978[, 1], 10)
#' rng2_index(evans1978[, 2], 10)
#'
#' @details
#'
#' This function takes a vector \code{x} and computes an index that reflects
#' whether the distribution of response pairs in the sequence is uneven given
#' the number of possible \code{options}. In this modified version of the
#' RNG index response pairs are constructed over the distance of two values,
#' e.g., in the sequence c(1,2,3) the first response pair is (1,3).
#' The values of this index range between 0 and 1. A value of 1  indicates
#' complete redundancy of response pairs,whereas a value of 0  indicates
#' an even distribution of response pairs.
#'
#' @export
#'
#' @references
#'
#' Towse, J.N., Neil, D. Analyzing human random generation behavior: A review of
#' methods used and a computer program for describing performance. Behavior
#' Research Methods, Instruments, & Computers 30, 583–591 (1998).
#' \url{https://doi.org/10.3758/BF03209475}
rng2_index <- function(x, options, circ = TRUE) {
  x <- to_numeric(x)
  min_options <- 2
  min_length <- 3
  base_checks(x, options, min_options, min_length)
  matr <- convert_to_matrix(x, options, order = 2, circ = circ)

  ## get dividend and divisor of rng_index
  dividend <- get_quotient_dividend(matr)
  divisor <- get_quotient_divisor(matr)

  ## compute and return quotient
  result <- get_quotient(dividend, divisor)
  return(result)
}

#' Compute dividend of rng_index, which reflects the sum of log values of
#' all matrix cell frequencies
#'
#' @param matr matrix of response pairs
#' @return rng_index dividend of \code{x}
#'
#' @noRd
get_quotient_dividend <- function(matr) {
  dividend  <- 0
  row_length <- nrow(matr)
  col_length <- row_length
  for (i in 1:row_length) {
    for (j in 1:col_length) {
      if (matr[i, j] > 1) {
        dividend  <- dividend  + log10(matr[i, j]) *  matr[i, j]
      }
    }
  }
  return(dividend)
}

#' Compute divisor of rng_index, which reflects the sum of log values of all
#' matrix row frequencies
#'
#' @param matr matrix of response pairs
#' @return rng_index divisor of \code{x}
#'
#' @noRd
get_quotient_divisor <- function(matr) {
  divisor <- 0
  row_length <- sqrt(length(matr))
  row_marginals <- rowSums(matr)
  for (i in 1:row_length) {
    row_marginal <- row_marginals[i]
    if (row_marginal > 1) {
      divisor <- divisor + log10(row_marginal) * row_marginal;
    }
  }
  return(divisor)
}

#' Compute result of rng_index
#'
#' @param dividend dividend of rng_index
#' @param divisor divisor of rng_index
#' @return rng_index of\code{x}
#'
#' @noRd
get_quotient <- function(dividend, divisor) {
  if (dividend == 0 & divisor == 0) {
    return(0)
  } else {
    quotient <- dividend / divisor
    return(quotient)
  }
}
