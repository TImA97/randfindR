#' Digram Repetitions
#'
#' @description
#' Compute sum of all response pairs minus one
#'
#' @param x vector of distinct options of numbers or characters
#' @param options number of available options in sequence
#' @return digram repetition of \code{x}
#'
#' @examples
#' digram_rep(ginsburg1994, 10)
#' digram_rep(evans1978[, 1], 10)
#' digram_rep(evans1978[, 2], 10)
#'
#' @details
#' This function takes a vector \code{x} and sums up the frequency of all
#' response pairs given the number of possible \code{options}. For each response
#' pair that occurs at least once the total sum is reduced by one. A lower score
#' indicates an even distribution of response pairs, i.e., most or all of them occurred.
#' A higher score indicates an uneven distribution of response pairs, i.e., many
#' response pairs did not occur.
#'
#' @export
#'
#' @references
#' Ginsburg N, Karpiuk P. Random Generation: Analysis of the Responses.
#' Perceptual and Motor Skills. 1994;79(3):1059-1067.
#' \doi{doi:10.2466/pms.1994.79.3.1059}
digram_rep <- function(x, options) {
  x <- to_numeric(x)
  min_options <- 2
  base_checks(x, options, min_options)
  matr <- convert_to_matrix(x, options)

  ## compute sum of (cell values - 1)
  sum <- 0
  for (i in 1:options) {
    for (j in 1:options) {
      if (matr[i, j] > 0) {
        sum <- sum + matr[i, j] - 1
      }
    }
  }

  return(sum)
}


#' Repetitions index of randomness
#' @description Compute the frequency a value is repeated in the next round
#' @param x vector of distinct options of numbers or characters
#' @return repetitions of \code{x}
#'
#' @examples
#' repetitions(ginsburg1994)
#' repetitions(evans1978[, 1])
#' repetitions(evans1978[, 2])
#'
#' @details
#' This function takes a vector \code{x} and counts how often values are
#' followed by the same value in the next position of the sequence. A score of 0
#' indicates that there are no repetitions of any value in the next position.
#' A higher score indicates an increased degree of repetition.
#'
#' @export
#'
#' @references
#' Ginsburg N, Karpiuk P. Random Generation: Analysis of the Responses.
#' Perceptual and Motor Skills. 1994;79(3):1059-1067.
#' \doi{doi:10.2466/pms.1994.79.3.1059}
repetitions <- function(x) {
  x <- to_numeric(x)
  is_vector_long_enough(x)
  options <- get_number_unique_responses(x)
  matr <- convert_to_matrix(x, options)
  sum <- sum(diag(matr))
  return(sum)
}

#' Series index of randomness
#' @description Compute frequency with which values are followed by their most
#' adjacent predecessors and successors
#' @param x vector of distinct options of numbers or characters
#' @param options number of available options in sequence
#' @return series of \code{x}
#'
#' @examples
#' series(ginsburg1994, 10)
#' series(evans1978[, 1], 10)
#' series(evans1978[, 2], 10)
#'
#' @details
#' This function takes a vector \code{x} and counts how often values are
#' followed by their most adjacent predecessor and successors given the number
#' of possible \code{options}. This function includes series from the lowest to
#' the highest value and vice versa.
#' An example of a series would be '1,2,3' (options = 3).
#' The resulting score of this sequence is 3 (from 1 to 2, from 2 to 3,
#' and from 3 to 1).
#' The highest possible score equals the number of values in a sequence when
#' each value is followed by its most adjacent predecessor or successor. The
#' lowest possible score is 0 when each value is followed by another value than
#' its predecessor or successor.
#'
#' @export
#'
#' @references
#' Ginsburg N, Karpiuk P. Random Generation: Analysis of the Responses.
#' Perceptual and Motor Skills. 1994;79(3):1059-1067.
#' \doi{doi:10.2466/pms.1994.79.3.1059}
series <- function(x, options) {
  x <- to_numeric(x)
  min_options <- 2
  base_checks(x, options, min_options)
  matr <- convert_to_matrix(x, options)

  sum <- 0

  ## add values below or above the matrix diagonal to sum
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

  ## add top right and bottom left cell of matrix to sum
  sum <- sum + matr[1, options]
  sum <- sum + matr[options, 1]

  return(sum)
}

#' Cluster ratio index
#' @description compute the population variance of all response pair frequencies
#' @param x vector of distinct options of numbers or characters
#' @param options number of available options in sequence
#' @return cluster ratio of \code{x}
#'
#' @examples
#' cluster_ratio(ginsburg1994, 10)
#' cluster_ratio(evans1978[, 1], 10)
#' cluster_ratio(evans1978[, 2], 10)
#'
#' @details
#'
#' This function takes a vector \code{x} and computes the population variance
#' of all response pair frequencies given the number of possible \code{options}.
#' A score of 0 indicates no variance among response pair frequencies, i.e.,
#' all response pairs occur equally often. An example of this is the sequence
#' '1-1-2-2' (response pairs: '1-1', '1-2', '2-2', and '2-1' from the last to
#' the first value) as each response pair occurs exactly once. Higher scores
#' indicate an increased degree of variance in the distribution of response pair
#' frequencies.
#'
#' @export
#'
#' @references
#' Ginsburg N, Karpiuk P. Random Generation: Analysis of the Responses.
#' Perceptual and Motor Skills. 1994;79(3):1059-1067.
#' \doi{doi:10.2466/pms.1994.79.3.1059}
cluster_ratio <- function(x, options) {
  x <- to_numeric(x)
  min_options <- 2
  base_checks(x, options, min_options)
  matr <- convert_to_matrix(x, options)

  ## transform first-order dependency table to vector
  cells <- as.vector(matr)

  ## compute variance and correct result so that it corresponds to the
  ## population variance
  var_cells <- var(cells) * (length(cells) - 1) / (length(cells))

  return(var_cells)
}

#' Guttmann's Null-Score Quotient
#'
#' @param x vector of distinct options of numbers or characters
#' @param options number of available options in sequence
#' @return Guttmann Null-Score Quotient of \code{x}
#'
#' @examples
#' null_score(ginsburg1994, 10)
#' null_score(evans1978[, 1], 10)
#' null_score(evans1978[, 2], 10)
#'
#' @details
#'
#' This function takes a vector \code{x} and computes the number of all response
#' pairs that do not occur in this sequence given the number of possible
#' \code{options}. The result of this computation is weighted with the highest
#' possible number of unused response pairs for this sequence.
#' The final result is attained by multiplying this quotient with 100.
#' The result of this quotient ranges from 0 to 100.
#' High scores indicate an uneven distribution of response pairs, whereas low
#' scores indicate an even distribution of response pairs.
#'
#' @export
#'
#' @references
#'
#' Towse, J.N., Neil, D. Analyzing human random generation behavior: A review of
#' methods used and a computer program for describing performance. Behavior
#' Research Methods, Instruments, & Computers 30, 583–591 (1998).
#' \doi{doi:10.3758/BF03209475}
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

