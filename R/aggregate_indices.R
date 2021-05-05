#' Compute variety of randomness indices
#'
#' @param df data frame containing sequences of options in row-wise format
#' @param columns columns which will be interpreted as sequence
#' @param indices_names indices of randomness to be computed as character vector
#' @param options indicates the wanted options for the ocmputation of indices
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
all_rand <- function(df, options, columns = NULL, indices = NULL, new_col_name) {

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

  ## check if provided indices are valid
  correct_indices_provided(indices, all_indices)


  for (i in indices_names) {
    new_var <- numeric(length = nrow(df))

    for (p in 1:nrow(df)) {
      arguments <- list(df[p, columns], 3)
      if (i == "runs_index" | i == "repetitions" | i == "gap_score" | i == "tp_index") {
        arguments <- list(df[p, columns])
      }
      new_var[p] <- do.call(i, arguments)
    }
    col_name <- paste0(new_col_name, i)
    df[, col_name] <- new_var
  }
  return(df)
}

