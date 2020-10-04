## decide whether to include transition from last to first number

#' Compute Digram Repetitions (see Ginsburg & Karpiuk, 1994)
#'
#' @param x vector of random numbers
#' @param options number of available options in sequence
#' @return digram repetition of \code{x}
#'
#' @export
digram_rep <- function(x, options) {

  x <- to_numeric(x)
  min_options <- 2
  base_checks(x, options, min_options)
  matr <- convert_to_matrix(x, options)

  # compute sum of (cell values - 1)
  sum <- 0
  for (i in 1:options) {
    for (j in 1:options) {
      if (matr[i,j] > 0) {
        sum <- sum + matr[i,j] - 1
      }
    }
  }

  return(sum)
}


#' Compute repetitions as measure of randomness (see Ginsburg & Karpiuk, 1994)
#' @description Compute frequency a value is repeated in the next round
#' @param x vector of random numbers
#' @param options number of available options in sequence
#' @return repetitions of \code{x}
#'
#' @export
repetitions <- function(x, options) {
  x <- to_numeric(x)
  min_options <- 2
  base_checks(x, options, min_options)
  matr <- convert_to_matrix(x, options)
  sum <- sum(diag(matr))
  return(sum)
}

#' Compute series as measure of randomness (see Ginsburg & Karpiuk, 1994)
#' @description Compute frequency with which values are followed by their most
#' adjacent predecessors and successors in the vector
#' @param x vector of random numbers
#' @param options number of available options in sequence
#' @return series of \code{x}
#'
#' @export
series <- function(x, options) {
  x <- to_numeric(x)
  min_options <- 2
  base_checks(x, options, min_options)
  matr <- convert_to_matrix(x, options)

  sum <- 0

  # add values below or above the matrix diagonal to sum
  for (i in (1:options)) {
    if (i != options && i != 1) {
      sum <- sum + sum(matr[i, c(i + 1, i - 1)])
    }
    else if (i == options) {
      sum <- sum + matr[i, i - 1]
    } else {
      sum <- sum + matr[i, i + 1]
    }
  }

  # add top right and bottom left cell of matrix to sum
  sum <- sum + matr[1, options]
  sum <- sum + matr[options, 1]

  return(sum)
}

#' Compute cluster ratio as a measure of randomness (see Ginsburg & Karpiuk, 1994)
#' @description compute the variance of all frequencies in table of
#' first-order dependencies
#' @param x vector of random numbers
#' @param options number of available options in sequence
#' @return cluster ratio of \code{x}
#'
#' @export
cluster_ratio <- function(x, options) {
  x <- to_numeric(x)
  min_options <- 2
  base_checks(x, options, min_options)
  matr <- convert_to_matrix(x, options)

  # transform first-order dependency table to vector
  cells <- as.vector(matr)

  # compute variance and correct result so that it corresponds to the
  # population variance
  var_cells <- var(cells) *  (length(cells) - 1) / (length(cells))

  return(var_cells)
}

#' Compute Guttmann's Null-Score Quotient
#'
#' @param x vector of random numbers
#' @param options number of available options in sequence
#' @return Guttmann Null-Score Quotient of \code{x}
#'
#' @details
#'
#' This function takes a vector \code{x} and computes the number of all response
#' pairs that do not occur in this sequence given the number of possible
#' \code{options}. The result of this computation is weighted with the highest
#' possible number of unused response pairs for this sequence.
#' The final result is attained by multiplying this quotient with 100.
#' The result of this quotient ranges from 0 to 100.
#' High values indicate an uneven distribution of response pairs, whereas low
#' values indicate an even distribution of response pairs.
#'
#' @export
#'
#' @references
#'
#' Towse, J.N., Neil, D. Analyzing human random generation behavior: A review of
#' methods used and a computer program for describing performance. Behavior
#' Research Methods, Instruments, & Computers 30, 583–591 (1998).
#' \url{https://doi.org/10.3758/BF03209475}
null_score <- function(x, options) {
  x <- to_numeric(x)
  min_options <- 2
  base_checks(x, options, min_options)
  matr <- convert_to_matrix(x, options)

  # compute number of 0-value-cells and compute quotient
  null_score <- sum(matr == 0)
  max_score <- (options ^ 2) - 1
  null_score_quotient <- 100 * (null_score / max_score)
  return(null_score_quotient)
}

