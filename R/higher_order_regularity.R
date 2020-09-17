#' Compute regularity index by Skliar, Monge & Oviedo (2009)
#' @param x vector of random numbers
#' @param options number of available options in sequence
#' @return regularity index of \code{x}
reg_index <- function(x, options) {

  x <- to_numeric(x)

  # make case distinction depending on length of input vector (TODO)

  components <- numeric(length = length(x) * 2 - 1)
  number_dyads <- (options^2)


  # displace input sequence (number of options -1) times and save them in list
  all_sequences <- get_all_displacements(x)

}

get_all_displacements <- function(x) {
  all_sequences <- list(x)
  sequence_counter <- 2
  to_be_displaced <- x
  for (i in 1:(length(x) - 1)) {
    displaced_sequence <- displace_sequence(to_be_displaced)
    all_sequences[[sequence_counter]] <- displaced_sequence
    to_be_displaced <- displaced_sequence
    sequence_counter <- sequence_counter + 1
  }
  return(all_sequences)
}

displace_sequence <- function(x) {
  result <- x
  result[length(x)] <- x[1]
  for (i in 1:(length(x) -1)) {
    result[i] <- x[i + 1]
  }
  return(result)
}
