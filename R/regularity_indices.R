#' Compute runs (see Ginsburg & Karpiuk, 1994)
#' @description Compute the variance of successive ascending or descending runs
#' @param x vector of random numbers
#' @return runs of \code{x}
runs_index <- function(x, asc = TRUE) {
  x <- to_numeric(x)

  runs <- vector(length = 1)

  # iterate over vector and count runs
  # make case distinction whether asc is 'TRUE' or 'FALSE'
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

  # store length of last run
  runs[runs_counter] <- current_length
  current_length <- 0

  # compute population variance of runs
  variance <- var(runs) * (length(runs) - 1) / length(runs)

  return(variance)
}

#' Compute Coupon Score (see Ginsburg & Karpiuk)
#' @description Compute average digit length required for all responses to occur
#' @param x vector of random numbers
#' @param options number of available options in sequence
#' @return Coupon Score of \code{x}
coupon_score <- function(x, options) {
  x <- to_numeric(x)

  # logical vector to check whether digit has occurred in sequence
  occurred_options <- vector(length = options)

  sequence_lengths <- vector(length = 1)
  current_length <- 0
  sequence_counter <- 1

  # iterate over vector and update occurred_options
  for (i in 1:length(x)) {

    # if all digits were emitted in a sequence, store sequence length
    # if the last sequence does not contain all possible values, it will not be
    # added to sequence_lengths
    if (all(occurred_options)) {
      sequence_lengths[sequence_counter] <- current_length
      current_length <- 0
      sequence_counter <- sequence_counter + 1
      occurred_options <- occurred_options & FALSE
    }

    value <- x[i]
    occurred_options[value] <- occurred_options[value] | TRUE
    current_length <- current_length + 1

  }

  # store length of last sequence if its complete
  if (all(occurred_options)) {
    sequence_lengths[sequence_counter] <- current_length
  }

  # check whether all possible options are included in the provided sequence
  # if this is not the case, sequence_lengths is still in its original logical
  # state
  if (is.logical(sequence_lengths)) {
    warning(
      "The provided sequence does not contain all possible options.
        Consequently, the Coupon Score cannot be computed and 'NA' is
        returned."
    )
    return(NA)
  }

  # compute mean of all complete sets of digits
  result <- mean(sequence_lengths)
  return(result)

}

