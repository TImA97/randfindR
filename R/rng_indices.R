#' Compute RNG index by Evans (1978)
#'
#' @param x vector of random numbers
#' @param options number of available options in sequence
#' @return RNG index of \code{x}
#'
#' @export
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
