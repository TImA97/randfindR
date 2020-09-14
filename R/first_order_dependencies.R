#' Compute Digram Repetitions (see Ginsburg & Karpiuk, 1994)
#'
#' @param x vector of random numbers
#' @param options number of available options in sequence
#' @return digram repetition of \code{x}
digram_rep <- function(x, options) {

  x <- to_numeric(x)
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

#' Compute RNG index by Evans (1978)
#'
#' @param x vector of random numbers
#' @param options number of available options in sequence
#' @return RNG index of \code{x}
rng_index <- function(x, options) {

  ## decide whether to include transition from last to first number

  x <- to_numeric(x)
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
  rowMarginals <- rowSums(matr)
  for (i in 1:options) {
    rowMarginal <- rowMarginals[i]
    if (rowMarginal > 1) {
      divisor <- divisor + log10(rowMarginal) * rowMarginal;
    }
  }

  result <- dividend / divisor
  return(result)
}

#' Compute repetitions as measure of randomness (see Ginsburg & Karpiuk, 1994)
#' @description Compute frequency a value is repeated in the next round
#' @param x vector of random numbers
#' @param options number of available options in sequence
#' @return repetitions of \code{x}
repetitions <- function(x, options) {
  x <- to_numeric(x)
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
series <- function(x, options) {
  x <- to_numeric(x)
  matr <- convert_to_matrix(x, options)

  sum <- 0

  # add values below or above the matrix diagonal to sum
  for (i in (1:options)) {
    if (i != options && i != 1) {
      sum <- sum + matr[i, i - 1]
      sum <- sum + matr[i, i + 1]
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
cluster_ratio <- function(x, options) {
  x <- to_numeric(x)
  matr <- convert_to_matrix(x, options)

  # transform first-order dependency table to vector
  cells <- as.vector(matr)

  # compute variance and correct result so that it corresponds to the
  # population variance
  var_cells <- var(cells) *  (length(cells) - 1) / (length(cells))



  return(var_cells)
}



#' convert vector to matrix of first order dependencies
#' @description helper function to transform a given vector to a matrix of first
#' order dependencies, specifying the frequency of all possible 2-digit long
#' sequences
#' @param x vector of random numbers
#' @param options number of available options in sequence
#' @keywords internal
convert_to_matrix <- function(x, options) {
  matr <- matrix(data = 0, nrow = options, ncol = options)
  for (i in 1:(length(x) - 1)) {
    current_value <- x[i]
    next_value <- x[i + 1]
    matr[current_value, next_value] <- matr[current_value, next_value] + 1
  }

  return(matr)
}
