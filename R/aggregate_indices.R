#' Compute variety of randomness indices
#'
#' @param df data frame containing sequences of options in row-wise format
#' @param options number of available options in sequence
#' @param columns columns which will be interpreted as sequence
#' @param indices_names indices of randomness to be computed as character vector
#' @param arguments indicates the wanted options for the computation of indices
#'
#' @details
#'
#' This function allows to enter a data frame of sequence data and computes a
#' variety of randomness indices. It is assumed that the data is provided in
#' row-wise, i.e., each row represents one sequences. The output of this function
#' is also a data frame. If the 'columns' argument is not provided, it is assumed
#' that the whole data frame should be used for computing the indices.
#' The 'indices_names' argument indicates the selection of randomness indices
#' you want to have. By default all indices are computed.
#'
#' @export
all_rand <- function(df, options, columns = NULL, indices = NULL, arguments = NULL) {

  ## check whether 'df' is a data frame and not a list
  df_has_correct_format(df)

  all_indices <-
    c(
      "digram_rep",
      "repetitions",
      "series",
      "cluster_ratio",
      "null_score",
      "reg_index",
      "runs_index",
      "coupon_score",
      "gap_score",
      "poker_score",
      "rng_index",
      "rng2_index",
      "tp_index",
      "redundancy_index",
      "var_digits"
    )

  indices_names <- all_indices

  ## check if provided indices are valid
  if (!is.null(indices)) {
    correct_indices_provided(indices, all_indices)
    indices_names <- indices
  }

  ## prepare default arguments for the computation of randomness indices
  default_arguments <- character(length = length(indices_names))

  without_options_argument <-
    c("repetitions",
      "runs_index",
      "gap_score",
      "poker_score",
      "tp_index")

  ## take by default all columns as arguments
  col_names <- names(df)

  if (!is.na(columns)) {
    col_names <- columns
  }

  for (i in indices_names) {
    new_index <- numeric(length = nrow(df))

    for (p in 1:nrow(df)) {
      arguments <- list(df[p, col_names], options)
      if (i %in% without_options_argument) {
        arguments <- list(df[p, col_names])
      }
      tryCatch(
        {
          new_index[p] <- do.call(i, arguments)
        },
        error = function(e) {
          print(paste0("An error occurred. It was called from ", i, ": ", e))
        }
      )
    }
    col_name <- i
    df[, col_name] <- new_index
  }
  print(df)
  return(df)
}

