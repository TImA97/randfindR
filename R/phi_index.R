#' Phi Index
#'
#' @param x vector of random numbers
#' @param options number of available options in sequence
#' @param order order of analysis
#' @return Phi index of \code{x}
#'
#' @details
#' to be filled
#'
#' @export
#'
#' @references
#' Ginsburg N, Karpiuk P. Random Generation: Analysis of the Responses.
#' Perceptual and Motor Skills. 1994;79(3):1059-1067.
#' \url{doi:10.2466/pms.1994.79.3.1059}
#'
#' Wagenaar, W. A. 1970. “Subjective Randomness and the Capacity to
#' Generate Information.” Acta Psychologica 33:233–42.
#' doi: 10.1016/0001-6918(70)90135-6.
phi_index <- function(x, options, order = 2) {
  x <- to_numeric(x)
  min_options <- 2
  min_length <- order + 1
  base_checks(x, options, min_options, min_length)

  phi_index <- 0
  if (options == 2) {
    phi_index <- compute_phi_index(x, order)
  } else {
    phi_index <- compute_average_phi_index(x, options, order)
  }

  ## make case distinction between 1) options = 2 and 2) options > 2
  ### case 1) compute phi index (function)
  ### case 2) compute summed phi index (function)
  #### transform sequence (function)
  ## get observed frequencies (function)
  ## get expected frequencies (function)
  ## get chi-square value (function)
  ## get phi value (not necessarily function)
  ## apply normalization procedure (function)?
}

#' Compute phi index
#' @param x vector of random numbers
#' @param order order of analysis
#' @return Phi index of \code{x} with two options
#'
#' @noRd
compute_phi_index <- function(x, order) {

}
