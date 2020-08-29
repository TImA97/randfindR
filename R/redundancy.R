#' Compute redundancy index (see Towse & Neil, 1998)
#'
#' @param x vector
#' @param possible_responses number of options in sequence
#' @return redundancy index of \code{x}
redundancy_index <- function(x, possible_responses) {
  if (possible_responses < 2) {
    stop('sequence must include at least 2 distinct responses')
  }

  x <- to_numeric(x)
  distinct_responses <- length(unique(x))
  frequencies <- as.vector(table(x))

  # check whether there are more distinct options in the vector than declared
  # if so, return error message.
  # if there are more possible  than distinct options in the vector,
  # add the omitted options to the observed frequencies with value '0'
  if (possible_responses < distinct_responses) {
    stop('vector contains more unique responses than declared in function call')
  } else if (possible_responses > distinct_responses) {
    options_to_be_added <- possible_responses - distinct_responses
    for (i in 1:options_to_be_added) {
      frequencies[distinct_responses + i] <- 0
    }
  }

  log_sum <- 0
  for (i in 1:possible_responses) {
    if (frequencies[i] == 0) {
      log_sum <- log_sum
    } else {
      freq <-  frequencies[i]
      log_sum <- log_sum + freq * log2(freq)
    }
  }

  h_single <- log2(length(x)) - (1 / length(x)) * log_sum
  h_max <- log2(possible_responses)
  r_index <- 100 * (1 - (h_single / h_max))

  return(r_index)
}

#' function transforms given vector to numeric
#'
#' @param x vector
#' @return x_transformed
#' @keywords internal
to_numeric <- function(x) {
  as.numeric(factor(x))
}
