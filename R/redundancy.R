#' Compute redundancy index (see Towse & Neil, 1998)
#'
#' @param x vector
redundancy <- function(x, possible_responses) {
  if (possible_responses < 2) {
    stop('sequence must include at least 2 responses')
  }

  x <- to_numeric(x)
  unique_responses <- length(unique(x))
  freq_table <- table(x)
  frequencies <- as.vector(freq_table)

  if (possible_responses < unique_responses) {
    stop('vector includes more unique responses than declared in function call')
  } else if (possible_responses > unique_responses) {
    options_to_be_added <- possible_responses - unique_responses
    for (i in 1:options_to_be_added) {
      frequencies[unique_responses + i] <- 0
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

to_numeric <- function(x) {
  as.numeric(factor(x))
}
