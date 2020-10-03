#' Turning Point Index
#'
#' @param x vector of random numbers
#' @param options number of available options in sequence
#' @return Turning Point Index of \code{x}
#'
#' @details
#'
#' This function takes a vector \code{x} and computes all turning points in the
#' given sequence. Turning points are defined as values int the given
#' sequence where the preceding and succeeding value are both either higher
#' or lower than the current value. The number of observed turning points is
#' weighted against the theoretically expected number of turning points given
#' the length of the sequence. The resulting quotient is multiplied with 100.
#' Values over 100 indicate too many turning points compared to the theoretical
#' distributions, values below 100 too few.
#'
#' @export
#'
#' @references
#'
#' Towse, J.N., Neil, D. Analyzing human random generation behavior: A review of
#' methods used and a computer program for describing performance. Behavior
#' Research Methods, Instruments, & Computers 30, 583–591 (1998).
#' \url{https://doi.org/10.3758/BF03209475}
tp_index <- function(x) {
  x <- to_numeric(x)
  min_length <- 3
  is_vector_long_enough(x, min_length)

  # compute number of turning points in x
  tp_observed <- 0
  for (i in 2:(length(x) - 1)) {
    if (
      (x[i] < x[i - 1] & x[i] < x[i + 1]) |
      (x[i] > x[i - 1] & x[i] > x[i + 1])) {
      tp_observed <- tp_observed + 1
    }
  }

  # compute number of expected turning points
  tp_expected <- (2 / 3) * (length(x) - 2)

  # compute quotient of observed and expected turning points
  tp_index <- 100 * (tp_observed / tp_expected)
  return(tp_index)
}
