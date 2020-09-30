# this class contains checks that are conducted before the main code is executed

# aggregate function that conducts basic checks for functions
# set default of min_length to 2
base_checks <- function(x, options, min_options, min_length = 2) {
  is_vector_long_enough(x, min_length);
  sufficient_options_provided(options, min_options)
  is_number_of_distinct_options_too_high(x, options)
}

# check whether vector length is at least 2
is_vector_long_enough <- function(x, min_length = 2) {
  if (length(x) < min_length) {
    error_message <- paste("the sequence should contain at least",
                           min_length,
                           "digits")
    stop(error_message)
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
  distinct_options <- get_number_unique_responses(x)

  if (distinct_options > options) {
    stop(
      "vector contains more distinct responses than declared in
      'options' argument"
    )
  }
}
