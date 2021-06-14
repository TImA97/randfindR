#' Turning Point Index
#'
#' @param x vector of random numbers
#' @return Turning Point Index of \code{x}
#'
#' @examples
#' tp_index(ginsburg1994)
#' tp_index(evans1978[, 1])
#' tp_index(evans1978[, 2])
#'
#' @details
#'
#' This function takes a vector \code{x} and computes all turning points in the
#' given sequence. Turning points are defined as values in the given
#' sequence where the preceding and succeeding value are both either higher
#' or lower than the current value. The number of observed turning points is
#' weighted against the theoretically expected number of turning points given
#' the length of the sequence. The resulting quotient is multiplied with 100.
#' Values over 100 indicate a higher number of turning points than theoretically
#' expected; values below 100 indicate fewer turning points than theoretically
#' expected.
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

  ## compute number of turning points in x
  tp_observed <- get_number_tps(x)

  ## compute number of expected turning points
  tp_expected <- (2 / 3) * (length(x) - 2)

  ## compute quotient of observed and expected turning points
  tp_index <- 100 * (tp_observed / tp_expected)
  return(tp_index)
}

#' Computes number of turning points
#'
#' @param x vector of random numbers
#' @return number of turning points of \code{x}
#'
#' @noRd
get_number_tps <- function(x) {
  tp_observed <- 0
  level <- FALSE
  before_level <- x[1]
  level_value <- 0
  for (i in 2:(length(x) - 1)) {
    ## mark occurrence of a sequence of identical values (change level to 'TRUE')
    if (x[i] == x[i + 1] & level == FALSE) {
      level <- TRUE
      before_level <- x[i - 1]
      level_value <- x[i]
    } else if (level == TRUE & x[i] != level_value) {
      ## mark end of sequence of identical values (change level to 'FALSE')
      level = FALSE
    }

    ## increment turning points if there is a local peak or low point
    ## also, consider special case that the local peak or low point consists
    ## of several values of the same kind
    if ((x[i] < x[i - 1] & x[i] < x[i + 1]) |
        (x[i] > x[i - 1] & x[i] > x[i + 1])) {
      tp_observed <- tp_observed + 1
    } else if (((x[i] < x[i + 1] & x[i] < before_level) |
                (x[i] > x[i + 1] & x[i] > before_level)) &
               (level == TRUE)) {
      tp_observed <- tp_observed + 1
      level <- FALSE
    }
  }
  return(tp_observed)
}
