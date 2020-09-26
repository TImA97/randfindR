#' Compute redundancy index (see Towse & Neil, 1998)
#'
#' @param x vector of random numbers
#' @param options number of available options in sequence
#' @return redundancy index of \code{x}
#' @examples
#' redundancy_index(c(1,1,1,1),2)
#' redundancy_index(c(2,2,1,1),2)
#'
#' @export
redundancy_index <- function(x, options) {

  min_options <- 2
  x <- to_numeric(x)

  base_checks(x, options, min_options)

  distinct_responses <- length(unique(x))
  frequencies <- as.vector(table(x))

  # if there are more possible  than distinct options in the vector,
  # add the omitted options to the observed frequencies with value '0'
  if (options > distinct_responses) {
    options_to_be_added <- options - distinct_responses
    for (i in 1:options_to_be_added) {
      frequencies[distinct_responses + i] <- 0
    }
  }

  # compute information that is provided by the sequence
  log_sum <- 0
  for (i in 1:options) {
    if (frequencies[i] == 0) {
      log_sum <- log_sum
    } else {
      freq <-  frequencies[i]
      log_sum <- log_sum + freq * log2(freq)
    }
  }
  h_single <- log2(length(x)) - (1 / length(x)) * log_sum

  # compute maximum information of sequence given the number of possible options
  h_max <- log2(options)

  r_index <- 100 * (1 - (h_single / h_max))

  return(r_index)
}


#' Compute variance of digits (see Ginsburg & Karpiuk, 1994)
#' @description Computes the variance of marginal totals
#' @param x vector of random numbers
#' @param options number of available options in sequence
#' @return variance of digits of \code{x}
#'
#' @export
variance_of_digits <- function(x, options) {

  min_options <- 2
  x <- to_numeric(x)
  base_checks(x, options, min_options)

  matr <- convert_to_matrix(x, options)
  frequencies <- colSums(matr)

  # compute variance and correct result so that it corresponds to the
  # population variance
  variance <-
    var (frequencies) * (length(frequencies) - 1) / length(frequencies)
  return(variance)
}


#' function transforms given vector to numeric
#'
#' @param x vector
#' @return x_transformed
#' @keywords internal
#'
#' @noRd
to_numeric <- function(x) {
  as.numeric(factor(x))
}
