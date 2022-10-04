#' Compute Block Entropy
#'
#' @param x vector of distinct options of numbers or characters
#' @param block_size length of blocks in which the original sequence should be
#' divided for analysis
#' @return block entropy of \code{x} for the specified \code{block_size}
#'
#' @examples
#' block_entropy(c(1, 1, 1, 2, 2, 2))
#' block_entropy(c(1, 1, 1, 2, 2, 2), block_size = 2)
#'
#' @details
#'
#' This function takes a vector \code{x} and uses it to compute its entropy for
#' the specified \code{block_size}, often referred to as \emph{k}. The default
#' implementation sets the \code{block_size} to 1 which is equivalent to
#' Shannon Entropy.
#'
#' @export
#'
#' @references
#'
#' Shannon, C. E. (n.d.). A Mathematical Theory of Communication. 55.
block_entropy <- function(x, block_size = 1){
  x <- to_numeric(x)

  ## compute entropy directly from observed frequencies for block_size= 1
  if (block_size== 1) {
    p_xi <- table(x) / length(x)
    result <- sum(p_xi * log2(p_xi)) * -1
    return(result)
  }

  ## extract all blocks of size k from sequence
  max_index <- length(x) - block_size+ 1
  blocks <- character(length = max_index)
  for (i in 1:max_index) {
    block <- x[i:(i + block_size- 1)]
    block_string <- paste0(block, collapse = "")
    blocks[i] <- block_string
  }

  ## compute higher-order block entropy over empirically observed block frequencies
  p_xi <- table(blocks) / length(blocks)
  result <- sum(p_xi * log2(p_xi)) * -1
  return(result)
}
