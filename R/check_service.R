

#' Aggregate function that bundles main checks together
#' @param x vector of random numbers
#' @param options number of provided options
#' @param min_options number of minimum options for function to work
#' @param min_length minimum vector length for function to work
#'
#' @noRd
base_checks <- function(x, options, min_options, min_length = 2) {
  is_vector_long_enough(x, min_length);
  sufficient_options_provided(options, min_options)
  is_number_of_distinct_options_too_high(x, options)
}

#' Check whether provided vector has a length of at least 2
#'
#' @param x vector of random numbers
#' @param min_length minimum vector length for function to work
#'
#' @noRd
is_vector_long_enough <- function(x, min_length = 2) {
  if (length(x) < min_length) {
    error_message <- paste("the sequence should contain at least",
                           min_length,
                           "digits")
    stop(error_message)
  }
}

#' Check whether the provided number of options is high enough for the function
#' to work
#'
#' @param options number of provided options
#' @param min_options number of minimum options for function to work
#'
#' @noRd
sufficient_options_provided <- function(options, min_options) {

  if (options < min_options) {
    error_message <- paste("Number of options must be at least",
                           min_options)
    stop(error_message)
  }
}

#' Check whether there are more unique options in the provided vector than
#' specified in options
#'
#' @param x vector of random numbers
#' @param options number of provided options
#'
#' @noRd
is_number_of_distinct_options_too_high <- function(x, options) {
  distinct_options <- get_number_unique_responses(x)

  if (distinct_options > options) {
    stop(paste("Vector contains", distinct_options, "responses but only",
               options, "were declared in the 'options' argument")
    )
  }
}

#' Check whether correct a list of correct vector of indices was provided
#'
#' @param indices character vector of wanted indices
#' @param available_indices character vector of allowed indices
#'
#' @noRd
correct_indices_provided <- function(indices, indices_names) {
  if (is.character(indices)) {
    correct_index <- indices %in% indices_names

    if (sum(correct_index) != length(indices_names)) {
      stop("The provided vector of randomness indices names contains at least
           one invalid name")
    }
  }

  stop("The 'indices' argument must be a character vector of valid randomness
       indices in the randseq package.")
}
