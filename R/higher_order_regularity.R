#' Compute regularity index
#' @param x vector of distinct options of numbers or characters
#' @param options number of available options in sequence
#' @return regularity index of \code{x}
#'
#' @examples
#' reg_index(ginsburg1994, 10)
#' reg_index(evans1978[, 1], 10)
#' reg_index(evans1978[, 2], 10)
#'
#' @details
#' This function takes a vector \code{x} as a sequence and the number of
#' possible \code{options} and computes the regularity of the given sequence.
#' The score ranges from 0 and 1. A score of 0 (which is barely ever obtained)
#' indicates no regularity, whereas a score of 1 indicates maximum regularity
#' (always the same option in the sequence).
#' The regularity index is obtained by identifying the the maximum regularity
#' occurring in the sequence.
#'
#' @export
#'
#' @references
#'
#' Skliar, Osvaldo, Ricardo E. Monge, Guillermo Oviedo, and Víctor Medina. 2009.
#' “Indices of regularity and indices of randomness for m-ary strings.”
#' \doi{doi:10.15517/rmta.v16i1.1418}.
reg_index <- function(x, options) {
  x <- to_numeric(x)
  min_length <- (options ^ 2)
  min_options <- 2
  base_checks(x, options, min_options, min_length)

  ## if length of vector x is dividable by the number of options squared,
  ## compute index as usual
  ## else, compute average reg_index over sub-strings
  if (length(x) %% (options ^ 2) == 0) {
    reg_index <- compute_index(x, options)
  } else {
    reg_index <- compute_average_index(x, options)
  }

  return(reg_index)
}

#' Compute index for cases in which the sequence length is dividable by the
#' number of dyads
#'
#' @param x vector of distinct options of numbers or characters
#' @param options number of available options in sequence
#' @return regularity index of \code{x}
#'
#' @noRd
compute_index <- function(x, options) {
  components <- numeric(length = length(x) * 2 - 1)
  components_counter <- 1
  number_dyads <- (options ^ 2)


  ## displace input sequence (number of options -1) times and save them in list
  sequences <- get_all_displacements(x)

  ## reverse input sequence and then, get all displacements into a list
  x_rev <- rev(x)
  reversed_sequences <- get_all_displacements(x_rev)

  ## combine all displaced sequences to one list
  all_sequences <- c(sequences, list(x_rev), reversed_sequences)

  ## compare displaced with original sequence and compute components
  for (i in 1:length(all_sequences)) {
    components[components_counter] <-
      get_sum_of_squares(x, all_sequences[[i]], options)
    components_counter <- components_counter + 1
  }

  ## compute index with max sum of squares
  max_component <- max(components)
  divisor_sum_one <- (length(x) - (length(x) / options ^ 2)) ^ 2
  divisor_sume_two <-
    (options ^ 2 - 1) * (length(x) / options ^ 2) ^ 2
  divisor <- sqrt(divisor_sum_one + divisor_sume_two)

  reg_index <- max_component / divisor

  ## optional: interchange reg_index with rnd_index
  rnd_index <- 1 - reg_index
  return(reg_index)
}

#' Compute average index for cases in which the sequence length is not dividable
#' by the number of dyads
#'
#' @param x vector of distinct options of numbers or characters
#' @param options number of available options in sequence
#' @return regularity index of \code{x}
#'
#' @noRd
compute_average_index <- function(x, options) {
  ## determine number of sub-strings for which the index has to be computed
  additional_length <- length(x) %% (options ^ 2)
  number_indizes <- additional_length + 1
  substring_length <- length(x) - additional_length
  indizes <- numeric(length = number_indizes)

  ## compute reg_index for each sub-string with a length dividable by the
  ## number of options squared
  for (i in 1:number_indizes) {
    indizes[i] <-
      compute_index(x[i:(substring_length + i - 1)], options)
  }

  ## compute and return average of regularity measures
  average_index <- mean(indizes)
  return(average_index)
}

#' Compute regularity statistic for each comparison of the original sequence
#' with a newly transformed sequence
#'
#' @param x vector of distinct options of numbers or characters
#' @param y vector of transformed sequence against which \code{x} is compared
#' @param options number of available options in sequence
#' @return regularity statistic for further computations
#'
#' @noRd
get_sum_of_squares <- function(x, y, options) {
  dyads <- numeric(length = (options ^ 2))
  names(dyads) <-
    paste0(rep(1:options, each = options), ",", 1:options)

  ## count occurrences of all dyads
  for (i in 1:length(x)) {
    original_value <- x[i]
    displaced_value <- y[i]
    dyad_name <- paste0(original_value, ",", displaced_value)
    dyads[dyad_name] <- dyads[dyad_name] + 1
  }

  ## compute score for distribution of dyads
  score <- 0
  for (i in 1:length(dyads)) {
    score <- score + (dyads[i] - (length(x) / options ^ 2)) ^ 2
  }

  ## compute square root of result score
  score <- as.vector(sqrt(score))

  return(score)
}

#' Get all displacements of the entered sequence
#'
#' @param x vector of random numbers
#' @return list of all possible displacements of original sequence \code{x}
#'
#' @noRd
get_all_displacements <- function(x) {
  all_sequences <- list()
  sequence_counter <- 1
  to_be_displaced <- x
  for (i in 1:(length(x) - 1)) {
    displaced_sequence <- displace_sequence(to_be_displaced)
    all_sequences[[sequence_counter]] <- displaced_sequence
    to_be_displaced <- displaced_sequence
    sequence_counter <- sequence_counter + 1
  }
  return(all_sequences)
}

#' Displace entered sequence by one step to the left
#'
#' @param  x vector of random numbers
#' @return displaced sequence of \code{x} by one step
#'
#' @noRd
displace_sequence <- function(x) {
  result <- x
  result[length(x)] <- x[1]
  for (i in 1:(length(x) - 1)) {
    result[i] <- x[i + 1]
  }
  return(result)
}
