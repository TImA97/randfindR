#' Redundancy index
#'
#' @description
#' Computes index indicating the evenness of the response frequency
#' distribution.
#'
#' @details
#' This function takes a vector \code{x} and weighs the observed information
#' concerning the response frequencies against the highest observable
#' information given the number of possible \code{options}. The resulting
#' quotient is subtracted from 1 and then, multiplied with 100.
#' A value of 0 indicates an even distribution of response frequencies,
#' whereas a value of 100 indicates complete redundancy of one response.
#'
#' @param x vector of random numbers
#' @param options number of available options in sequence
#' @return redundancy index of \code{x}
#'
#' @examples
#' redundancy_index(c(1,1,1,1), 2)
#' redundancy_index(c(2,2,1,1), 2)
#' redundancy_index(ginsburg1994, 10)
#' redundancy_index(evans1978[, 1], 10)
#' redundancy_index(evans1978[, 2], 10)
#'
#' @importFrom stats chisq.test var median
#'
#' @export
#'
#' @references
#' Towse, J.N., Neil, D. Analyzing human random generation behavior: A review of
#' methods used and a computer program for describing performance. Behavior
#' Research Methods, Instruments, & Computers 30, 583–591 (1998).
#' \url{https://doi.org/10.3758/BF03209475}
redundancy_index <- function(x, options) {

  min_options <- 2
  x <- to_numeric(x)

  base_checks(x, options, min_options)

  unique_responses <- get_number_unique_responses(x)
  frequencies <- as.vector(table(x))

  # if there are more possible  than distinct options in the vector,
  # add the omitted options to the observed frequencies with value '0'
  if (options > unique_responses) {
    options_to_be_added <- options - unique_responses
    for (i in 1:options_to_be_added) {
      frequencies[unique_responses + i] <- 0
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


#' Variance of digits
#' @description Computes the variance of marginal totals
#' @param x vector of random numbers
#' @param options number of available options in sequence
#' @return variance of digits of \code{x}
#'
#' @examples
#' var_digits(ginsburg1994, 10)
#' var_digits(evans1978[, 1], 10)
#' var_digits(evans1978[, 2], 10)
#'
#' @details
#' This function takes a vector \code{x} and computes the population variance
#' of response frequencies given the number of possible \code{options}.
#' This index is conceptually closely related to the idea of the
#' \code{\link{redundancy_index}}.
#'
#' @export
#'
#' @references
#' Ginsburg N, Karpiuk P. Random Generation: Analysis of the Responses.
#' Perceptual and Motor Skills. 1994;79(3):1059-1067.
#' \url{doi:10.2466/pms.1994.79.3.1059}
var_digits <- function(x, options) {

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


