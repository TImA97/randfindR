# this class contains checks that are conducted before the main code is executed

# aggregate function that conducts basic checks for functions
base_checks <- function(x, options, min_options) {
  vector_length_at_least_two(x);
  sufficient_options_provided(options, min_options)
  is_number_of_distinct_options_too_high(x, options)
}

# check whether vector length is at least 2
vector_length_at_least_two <- function(x) {
  if (length(x) < 2) {
    stop("sequence length should be at least 2")
  }
}

# check whether the provided number of options exceeds the minimum number of
# options for the function to work
sufficient_options_provided <- function(options, min_options) {

  if (options < min_options) {
    error_message <- paste("Number of options must be at least",
                           min_options)
    stop(error_message)
  }
}

# check whether vector contains more unique options than provided in the
# 'options' argument
is_number_of_distinct_options_too_high <- function(x, options) {
  distinct_options <- length(unique(x))

  if (distinct_options > options) {
    stop(
      "vector contains more distinct responses than declared in 'function call
      'options' argument"
    )
  }
}
