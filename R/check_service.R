# this class contains checks that are conducted before the main code is executed

# check whether the provided number of options exceeds the minimum number of
# options for the function to work
sufficient_options_provided <- function(options, min_options) {

  if (options < min_options) {
    error_message <- paste("sequence must include at least",
                           min_options,
                           "options")
    stop(error_message)
  }
}

# check whether vector contains more unique options than provided in the
# 'options' argument
is_number_of_distinct_options_too_high <- function(x, options) {
  distinct_options <- length(unique(x))

  if (distinct_options > options) {
    stop("vector contains more distinct responses than declared in function call")
  }
}
