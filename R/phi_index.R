#' Phi Index
#'
#' @param x vector of random numbers
#' @param options number of available options in sequence
#' @param order order of analysis
#' @return Phi index of \code{x}
#'
#' @details
#' to be filled
#'
#' @references
#' Ginsburg N, Karpiuk P. Random Generation: Analysis of the Responses.
#' Perceptual and Motor Skills. 1994;79(3):1059-1067.
#' \url{doi:10.2466/pms.1994.79.3.1059}
#'
#' Wagenaar, W. A. 1970. “Subjective Randomness and the Capacity to
#' Generate Information.” Acta Psychologica 33:233–42.
#' doi: 10.1016/0001-6918(70)90135-6.
phi_index <- function(x, options, order = 2) {
  x <- to_numeric(x)
  min_options <- 2
  min_length <- order + 1
  base_checks(x, options, min_options, min_length)

  phi_index <- 0
  if (options == 2) {
    phi_index <- compute_phi_index(x, order)
  } else {
    phi_index <- compute_average_phi_index(x, options, order)
  }

  ## make case distinction between 1) options = 2 and 2) options > 2
  ### case 1) compute phi index (function)
  ### case 2) compute summed phi index (function)
  #### transform sequence (function)
  ## get observed frequencies (function)
  ## get expected frequencies (function)
  ## get chi-square value (function)
  ## get phi value (not necessarily function)
  ## apply normalization procedure (function)?
}

#' Compute phi index
#' @param x vector of random numbers
#' @param order order of analysis
#' @return Phi index of \code{x} with two options
#'
#' @noRd
compute_phi_index <- function(x, order) {
  observed_frequencies <- get_observed_frequencies(x, order)
  expected_frequencies <- get_expected_frequencies(x, order)

  contingency_table <-
    get_contingency_table(observed_frequencies, expected_frequencies)
 # print(contingency_table)

  # compute chi-square goodness-of-fit statistic
  chi_squared_test <-
    suppressWarnings(chisq.test(as.vector(contingency_table[, 1]),
                                p = contingency_table[, 2]))
  chi_squared <- chi_squared_test$statistic
  chi_squared <- unname(chi_squared)

  ## manually change value to 100 if 'NaN' was returned by the chisq-test
  if (is.nan(chi_squared)) {
    chi_squared <- 100
  }

  # compute phi index
  phi <- sqrt(chi_squared / length(x)) * 100

  # reverse sign if there are more alternating than repetitive pairs
  if (contingency_table[1, 1] < contingency_table[2, 1]) {
    phi <- phi * (-1)
  }

  print(phi)
  return(phi)
}


#' Compute observed frequencies
#' @param x vector of random numbers
#' @param order order of analysis
#' @return observed frequencies of \code{x}
#'
#' @noRd
get_observed_frequencies <- function(x, order) {
  frequencies <- numeric(length = 2)
  names(frequencies) <- c("repetitive", "alternating")
  distance <- order - 1
  number_grams <- length(x) - distance

  # compute how often the first and last value of a sequence with length of a
  # specified order are alternating or identical
  frequencies["repetitive"] <-
    sum(x[1:(number_grams)] == x[(1 + distance):length(x)])
  frequencies["alternating"] <-
    sum(x[1:(number_grams)] != x[(1 + distance):length(x)])

  return(frequencies)
}


get_expected_frequencies <- function(x, order) {
  all_frequencies <- get_all_expected_frequencies(x, order)
  frequencies <- all_frequencies

  # only keep frequencies of required order
#  number_values <- 2 ^ order
 # required_values <-
#    (length(all_frequencies) - number_values + 1):length(all_frequencies)
 # frequencies <-
  #  all_frequencies[required_values]

  # separate frequencies into cases where first and last value are identical
  # or alternating
  reduced_frequencies <- numeric(length = 2)
  names(reduced_frequencies) <- c("repetitive", "alternating")

  for (i in 1:length(frequencies)) {
    gram <- names(frequencies[i])
    first_value <- substring(gram, first = 1, last = 1)
    last_value <- substring(gram, first = nchar(gram), last = nchar(gram))

    if (first_value == last_value) {
      reduced_frequencies["repetitive"] <-
        reduced_frequencies["repetitive"] + frequencies[i]
    } else {
      reduced_frequencies["alternating"] <-
        reduced_frequencies["alternating"] + frequencies[i]
    }
  }

  return(reduced_frequencies)
}

#' Compute expected frequencies
#' @param x vector of random numbers
#' @param order order of analysis
#' @return expected frequencies of \code{x}
#'
#' @noRd
get_all_expected_frequencies <- function(x, order) {

  frequencies <- numeric()
  distance <- order - 1

  # compute response frequencies of current order
  permutations <- expand.grid(rep(list(1:2), order))

  #compute expected frequencies for all permutations
  for (j in 1:nrow(permutations)) {
    permutation <- as.vector(permutations[j, ])
    #print(permutation)

    # compute dividend for expected frequencies TODO hier läuft etwas schief!
    dividend_factor_one_name <- as.numeric(permutation[1:distance])
    dividend_factor_one <- get_underlying_observed_frequency(x, dividend_factor_one_name)
    print(dividend_factor_one)
    dividend_factor_two_name <- as.numeric(permutation[2:order])
    dividend_factor_two <- get_underlying_observed_frequency(x, dividend_factor_two_name)

    dividend <- dividend_factor_one * dividend_factor_two

    # compute divisor for expected frequencies
    divisor <- 0
    if (order == 2) {
      divisor <- length(x)
    } else {
      divisor_factor_name <- as.numeric(permutation[2:distance])
      divisor <- get_underlying_observed_frequency(x, divisor_factor_name)
    }

    # compute expected frequencies
    expected <- 0
    expected <- dividend / divisor

    # generate name under which to store this frequency
    freq_name <- paste(permutation, collapse = "")

    frequencies[freq_name] <- expected

  }
  # replace NaNs with 0
  frequencies[frequencies == "NaN"] <- 0
  return(frequencies)
}

get_underlying_observed_frequency <- function(x, freq) {
  distance <- length(freq) - 1
  counter <- 0

  if (length(freq) == 1) {
    return(sum(x == freq))
  }

  ## count equal occurrences of substrings
  for (i in 1:(length(x) - length(freq) + 1)) {
    if (identical(x[i:(i + distance)], freq)) {
      counter <- counter + 1
    }
  }

  return(counter)
}

#' Compute expected frequencies
#' @param x vector of random numbers
#' @param order order of analysis
#' @return expected frequencies of \code{x}
#'
#' @noRd
get_all_expected_frequencies_from_scratch <- function(x, order) {

  ## escape condition for recursive call
  if (order == 1) {
    frequencies <- numeric(length = 2)

    ## include base rates as first values in frequencies
    names(frequencies) <- c("1", "2")
    frequencies["1"] <- sum(x == 1)
    frequencies["2"] <- sum(x == 2)

    return(frequencies)
  }

  # recursive call to compute the expected frequencies of previous orders
  frequencies <- get_all_expected_frequencies(x, (order - 1))

  # compute response frequencies of current order
  distance <- order - 1
  for (i in 1:order) {
    permutations <- expand.grid(rep(list(1:2), order))

    #compute expected frequencies for all permutations
    for (j in 1:nrow(permutations)) {
      permutation <- as.vector(permutations[j, ])

      # compute dividend for expected frequencies
      dividend_factor_one_name <- paste(permutation[1:distance], collapse = "")
      dividend_factor_one <- frequencies[dividend_factor_one_name]
      dividend_factor_two_name <- paste(permutation[2:order], collapse = "")
      dividend_factor_two <- frequencies[dividend_factor_two_name]

      dividend <- dividend_factor_one * dividend_factor_two

      # compute divisor for expected frequencies
      divisor <- 0
      if (i == 2) {
        divisor <- length(x)
      } else {
        divisor <- paste(permutation[2:distance], collapse = "")
        divisor <- frequencies[divisor]
      }

      # compute expected frequencies
      expected <- 0
      expected <- dividend / divisor

      # generate name under which to store this frequency
      freq_name <- paste(permutation, collapse = "")

      frequencies[freq_name] <- expected
    }
  }
  # replace NaNs with 0
  frequencies[frequencies == "NaN"] <- 0
  return(frequencies)
}

get_contingency_table <- function(observed, expected) {
  matr <-
    cbind(observed = observed,
          expected = proportions(expected))
  return(matr)
}
