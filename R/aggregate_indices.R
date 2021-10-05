#' Compute collection of randomness indices
#'
#' @param x data frame or vector containing sequences of options in row-wise format
#' @param options number of available options in sequence
#' @param circ indicate whether to include wrap around from end to the beginning
#' @param asc Indicate whether to compute variance of ascending or descending
#' runs. Default value is set to ascending.
#' @param indices indices of randomness to be computed as character vector
#' @param combine indicates whether the computed indices should be combined with
#' the original data frame.
#' @return vector or data frame containing the selection of randomness indices
#' @examples
#' all_rand(ginsburg1994, 10)
#' all_rand(evans1978[, 1], 10)
#' all_rand(evans1978[, 2], 10)
#'
#' @details
#'
#' This function allows to enter a data frame or vector \code{x} of sequence data and
#' computes a variety of randomness indices. If \code{x} is a data frame, it is
#' assumed that the sequences are provided in a row-wise format, i.e., each row
#' represents one sequence. All columns are included in the analysis.
#' In this case, the output of the function is also a data frame. If you want
#' the indices to be appended to your input data frame, you can set
#' \code{combine} to TRUE.
#' If \code{x} is a vector all indices are computed normally over said vector.
#' In this case, the output of the function is also a vector with one value for
#' each computed index.
#' The \code{circ} arguments determines whether a wrap around of the last digits
#' in a sequence to the first digits of a sequence should be included for indices
#' that are based on computing response pairs. The \code{asc} arguments
#' determines whether ascending or descending runs should be computed for the
#' runs index.
#' The \code{indices} argument lets you specify the randomness indices that you
#' want to have. By default all indices are computed.
#'
#' @export
all_randicators <- function(x,
                     options,
                     circ = TRUE,
                     asc = TRUE,
                     indices = NULL,
                     combine = FALSE) {
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

  if (is.data.frame(x)) {
    output_df <-
      all_rand_df(x, options, circ, asc, indices_names, combine)
    return(output_df)
  } else {
    output_vector <-
      all_rand_vector(x, options, circ, asc, indices_names)
    return(output_vector)
  }
}

#' Compute randomness indices when input is a data frame
#' (arguments are the same as in the main function)
#'
#' @noRd
all_rand_df <-
  function(x,
           options,
           circ,
           asc,
           indices_names,
           combine) {
    ## prepare output data frame (can be the input data frame if 'combine' equals
    ## true)
    new_df <- data.frame(nr = vector(length = nrow(x)))
    if (combine == TRUE) {
      new_df <- x
    }

    error_messages <- "There were errors during the analysis:\n"

    ## compute randomness indices for each row
    for (i in indices_names) {
      new_index <- numeric(length = nrow(x))
      arguments <- get_function_arguments(i, options, circ, asc)

      for (p in 1:nrow(x)) {
        row <- list(x = x[p, ])
        arguments["x"] <- row

        tryCatch({
          new_index[p] <- do.call(i, arguments)
        },
        error = function(e) {
          new_error <- paste0("An error was called from ", i, ": ", e)
          error_messages <<- append(error_messages, new_error)
        })
      }
      col_name <- i
      new_df[, col_name] <- new_index
    }

    ## only keep and print unique error messages
    error_messages <- unique(error_messages)
    if (length(error_messages) > 1) {
      warning(error_messages)
    }

    ## remove first placeholder column if entirely new data frame was created
    if (combine == FALSE) {
      new_df <- new_df[-1]
    }

    return(new_df)
  }

#' Compute randomness indices when input is a vector
#' (arguments are the same as in the main function except for \code{combine}
#' as this argument is only required for working with data frames)
#'
#' @noRd
all_rand_vector <- function(x, options, circ, asc, indices_names) {
  result <- numeric(length = length(indices_names))
  names(result) <- indices_names
  error_messages <- "There were errors during the analysis:\n"

  for (i in indices_names) {
    arguments <- get_function_arguments(i, options, circ, asc)
    #print(x)
    arguments["x"] <- list(x = x)

    tryCatch({
      result[i] <- do.call(i, arguments)
    },
    error = function(e) {
      new_error <- paste0("An error was called from ", i, ": ", e)
      error_messages <<- append(error_messages, new_error)
    })
  }

  ## only keep and print unique error messages
  error_messages <- unique(error_messages)
  if (length(error_messages) > 1) {
    warning(error_messages)
  }

  return(result)
}

#' Prepare arguments for computation of indices
#'
#' @param index name of the randomness index to be computed
#' @param options
#' @param circ
#' @param asc
#'
#' @noRd
get_function_arguments <- function(index, options, circ, asc) {
  without_options_argument <-
    c("repetitions",
      "runs_index",
      "gap_score",
      "poker_score",
      "tp_index")

  with_circ_argument <-
    c("rng_index",
      "rng2_index")

  with_asc_argument <- c("runs_index")

  arguments <- list()
  arguments["x"] <- 0

  if (!index %in% without_options_argument) {
    arguments["options"] <- options
  }

  if (index %in% with_circ_argument) {
    arguments["circ"] <- circ
  }

  if (index %in% with_asc_argument) {
    arguments["asc"] <- asc
  }

  return(arguments)
}
