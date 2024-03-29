#' Compute runs index of randomness
#'
#' @param x vector of distinct options of numbers or characters
#' @param asc Indicate whether to compute variance of ascending or descending
#' runs. Default value is set to ascending.
#' @return runs of \code{x}
#'
#' @examples
#' runs_index(ginsburg1994)
#' runs_index(ginsburg1994, FALSE)
#' runs_index(evans1978[, 1])
#' runs_index(evans1978[, 2])
#'
#' @details
#' This function takes a vector \code{x} and computes the population variance
#' of all ascending or descending run lengths, depending on \code{asc}.
#' An ascending run is defined by a value in a sequence being followed by
#' a larger value in the next position of the sequence. A score of 0 indicates
#' no variance in run length, i.e., runs always have the same length. Higher
#' scores indicate an increased degree of variation in run length.
#' The default version of this function computes the population variance of
#' ascending run lengths.
#'
#' @export
#'
#' @references
#' Ginsburg N, Karpiuk P. Random Generation: Analysis of the Responses.
#' Perceptual and Motor Skills. 1994;79(3):1059-1067.
#' \doi{doi:10.2466/pms.1994.79.3.1059}
runs_index <- function(x, asc = TRUE) {
  x <- to_numeric(x)
  is_vector_long_enough(x)

  runs <- numeric(length = 1)

  ## iterate over vector and count runs
  ## make case distinction whether 'asc' is 'TRUE' or 'FALSE'
  current_length <- 1
  runs_counter <- 1
  for (i in 2:length(x)) {
    if (asc == TRUE & x[i] > x[i - 1]) {
      current_length <- current_length + 1
    } else if (asc == FALSE & x[i] < x[i - 1]) {
      current_length <- current_length + 1
    } else {
      runs[runs_counter] <- current_length
      current_length <- 1
      runs_counter <- runs_counter + 1
    }
  }

  ## store length of last run
  runs[runs_counter] <- current_length

  ## print warning if just one run occurred
  if (length(runs) == 1) {
    warning("runs index cannot be computed because only one run occurred in the sequence.
            Consequently, NA was returned.")
  }

  ## compute population variance of runs
  variance <- var(runs) * (length(runs) - 1) / length(runs)

  return(variance)
}

#' Coupon Score
#'
#' @description Compute average digit length required for all responses to occur
#' @param x vector of distinct options of numbers or characters
#' @param options number of available options in sequence
#' @return Coupon Score of \code{x}
#'
#' @examples
#' runs_index(ginsburg1994, 10)
#' runs_index(evans1978[, 1], 10)
#' runs_index(evans1978[, 2], 10)
#'
#' @details
#' This function takes a vector \code{x} and computes the mean number of values
#' that is required for all possible \code{options} to occur. This means that the algorithm
#' starts to count at the beginning of the sequence until all \code{options} have occurred.
#' Having observed all \code{options},
#' the counting process starts again at zero at the next value of the sequence.
#' Finally, the mean over the length of these
#' complete sets of values (containing all options) is computed.
#' Incomplete sets of responses at the end of a sequence with at least one
#' option missing are not used for the computation of this index.
#' Consequently, this index cannot be computed for vectors that do not
#' contain all possible \code{options} and therefore, NA is returned. The lowest
#' possible score equals the number of \code{options}, indicating that
#' each complete set contains every possible option only once. A higher score
#' indicates that at least one complete set contains values of the same kind at least twice.
#'
#' @export
#'
#' @references
#' Ginsburg N, Karpiuk P. Random Generation: Analysis of the Responses.
#' Perceptual and Motor Skills. 1994;79(3):1059-1067.
#' \doi{doi:10.2466/pms.1994.79.3.1059}
#'
#' Towse, J.N., Neil, D. Analyzing human random generation behavior: A review of
#' methods used and a computer program for describing performance. Behavior
#' Research Methods, Instruments, & Computers 30, 583–591 (1998).
#' \doi{doi:10.3758/BF03209475}
coupon_score <- function(x, options) {
  x <- to_numeric(x)
  min_options <- 2
  base_checks(x, options, min_options)

  ## check whether all possible options are included in the provided sequence
  distinct_options <- get_number_unique_responses(x)
  if (distinct_options < options) {
    warning(
      "The provided sequence does not contain all possible options.
        Consequently, the Coupon Score cannot be computed and 'NA' is
        returned."
    )
    return(NA)
  }

  ## logical vector to check whether digit has occurred in sequence
  occurred_options <- vector(length = options)

  sequence_lengths <- vector(length = 1)
  current_length <- 0
  sequence_counter <- 1

  ## iterate over vector and update occurred_options
  for (i in 1:length(x)) {

    ## if all digits were emitted in a sequence, store sequence length
    ## if the last sequence does not contain all possible values, it will not be
    ## added to sequence_lengths
    if (all(occurred_options)) {
      sequence_lengths[sequence_counter] <- current_length
      current_length <- 0
      sequence_counter <- sequence_counter + 1
      occurred_options <- occurred_options & FALSE
    }

    value <- x[i]
    occurred_options[value] <- TRUE
    current_length <- current_length + 1
  }

  ## store length of last sequence if its complete
  if (all(occurred_options)) {
    sequence_lengths[sequence_counter] <- current_length
  }

  ## compute mean of all complete sets of digits
  result <- mean(sequence_lengths)
  return(result)
}


#' Gap Score
#'
#' @description Compute median gap between identical values
#' @param x vector of distinct options of numbers or characters
#' @return gap score of \code{x}
#'
#' @examples
#' gap_score(ginsburg1994)
#' gap_score(evans1978[, 1])
#' gap_score(evans1978[, 2])
#'
#' @details
#' This function takes a vector \code{x} and computes the median gap between
#' the most adjacent identical values. For each value encountered in the sequence
#' it is computed whether the same value occurs again in the sequence. If so,
#' the length of the gap between them is stored. Finally, the mean over these
#' gap lengths is computed and returned.
#' The lowest score of 1 indicates that most or all values of the same kind are
#' right next to each other. A higher scores indicates that options of the
#' same kind often have a gap between them containing options of a different kind.
#'
#' @export
#'
#' @references
#' #' Ginsburg N, Karpiuk P. Random Generation: Analysis of the Responses.
#' Perceptual and Motor Skills. 1994;79(3):1059-1067.
#' \doi{doi:10.2466/pms.1994.79.3.1059}
gap_score <- function(x) {
  x <- to_numeric(x)
  is_vector_long_enough(x)

  current_value <- x[1]
  gaps <- 0
  gap_counter <- 1
  occurred_values <- vector(length = 1)
  occurred_counter <- 1
  last_position <- 1

  for (i in 1:length(x)) {

    ## check whether current value has not occurred yet
    ## if so, update current value and position
    ## otherwise, go to the next value in vector x
    if (!any(x[i] %in% occurred_values)) {
      current_value <- x[i]
      last_position <- i
      occurred_values[occurred_counter] <- current_value
      occurred_counter <- occurred_counter + 1
    } else {
      next
    }

    ## break loop if last value in vector x is reached
    if (i == length(x)) {
      break
    }

    ## compute and store gaps
    for (j in (i + 1):length(x)) {
      if (x[j] == current_value) {
        gap <- j - last_position
        gaps[gap_counter] <- gap
        last_position <- j
        gap_counter <- gap_counter + 1
      }
    }
  }

  ## compute median of gaps
  result <- median(gaps)
  return(result)
}


#' Poker Score
#' @description Compute number of times exactly two responses of the same value
#' occur after division of the complete sequence into 5-digit-long sub-sequences
#' @param x vector of distinct options of numbers or characters
#' @return poker score of \code{x}
#'
#' @examples
#' poker_score(ginsburg1994)
#' poker_score(evans1978[, 1])
#' poker_score(evans1978[, 2])
#'
#' @details
#' This function takes a vector \code{x} and computes the frequency with
#' which exactly two identical values occur in 5-digit-long sub-sequences of the
#' original vector (2 in 5). If the vector length is not dividable by 5, the last 1-4
#' values will not be used for the computation. This index is a measure of repetition
#' in a sequence. The resulting score ranges from 0 (no 2 in 5) to (length of \code{x}) / 5
#' (always a 2 in 5 for each sub-sequence).
#'
#' @export
#'
#' @references
#' Ginsburg N, Karpiuk P. Random Generation: Analysis of the Responses.
#' Perceptual and Motor Skills. 1994;79(3):1059-1067.
#' \doi{doi:10.2466/pms.1994.79.3.1059}
poker_score <- function(x) {
  x <- to_numeric(x)
  is_vector_long_enough(x, min_length = 5)

  last_five <- numeric(length = 5)
  counter <- 1
  result <- 0

  ## iterate over vector x, store last 5 responses and check if an option
  ## occurred exactly 2 times
  for (i in 1:length(x)) {
    last_five[counter] <- x[i]

    ## check whether to begin new 5-digit-long sequence
    ## if so, check whether previous sequence contains two-of-a-kind
    if (counter %% 5 == 0) {
      values <- as.vector(table(last_five))
      two_of_a_kind <- values == 2
      ## increment result if a two-of-a-kind occurs exactly one time
      if (sum(two_of_a_kind == 1)) {
        result <- result + 1
      }
      counter <- 0
    }
    counter <- counter + 1
  }
  return(result)
}


