#' Compute Autocorrelation
#'
#' @param x vector of distinct options of numbers or characters
#' @param tau numeric indicating the gap between subsequent numbers for which
#' the autocorrelation will be computed
#' @return autocorrelation of \code{x} for the gap between subsequent numbers \code{tau}
#'
#' @examples
#' max_auto_corr(c(1, 1, 1, 2, 2, 2, 1, 2, 1, 2, 2, 2), tau = 2)
#' max_auto_corr(c(1, 1, 1, 2, 2, 2, 1, 2, 1, 2, 2, 2), tau = 3)
#'
#' @details
#'
#' This function takes a vector \code{x} and uses it to compute the autocorrelation
#' for a specified gap tau \code{tau}. This is done by correlating the original vector \code{x}
#' with the same sequence starting \code{tau} values later. A high autocorrelation
#' for a specified value \code{tau} indicates that there is a high correlation
#' between numbers in a sequence that are \code{tau} values apart from each other.
#'
#' @export
auto_corr <- function(x, tau) {
  n <- length(x)
  m <- mean(x)
  variance <- sum((x-m)^2) / n
  if (variance == 0) return(1)
  s1 <- x[1:(n-tau)]
  s2 <- x[(1+tau):n]
  covariance <- sum((s1-m)*(s2-m)) / (n-tau)
  covariance / variance
}

#' Compute the Maximum Autocorrelation
#'
#' @param x vector of distinct options of numbers or characters
#' @param min_tau numeric indicating the smallest gap between subsequent numbers for which
#' the autocorrelation will be computed
#' @param max_tau numeric indicating the largest gap between subsequent numbers for which
#' the autocorrelation will be computed
#' @return maximum autocorrelation of \code{x} for the all gaps \code{tau} in
#' the interval from \code{min_tau} to \code{max_tau} between subsequent numbers
#'
#' @examples
#' max_auto_corr(c(1, 1, 1, 2, 2, 2, 1, 2, 1, 2, 2, 2), tau = 2)
#' max_auto_corr(c(1, 1, 1, 2, 2, 2, 1, 2, 1, 2, 2, 2), tau = 3)
#'
#' @details
#'
#' This function takes a vector \code{x} and uses it to compute the maximum autocorrelation
#' for a specified interval of gaps tau from \code{min_tau} to \code{max_tau}.
#' The resulting autocorrelation is the largest autocorrelation that could be observed
#' in the interval for values of \code{tau}. A high autocorrelation
#' indicates that there there is a high correlation between numbers in a
#' sequence that are set apart from each other \code{min_tau} to \code{max_tau} values.
#'
#' @export
max_auto_corr <- function(x, min_tau, max_tau) {
  auto_corrs <- numeric(length = length(min_tau:max_tau))
  counter <- 1
  for (i in min_tau:max_tau) {
    auto_corrs[counter] <- auto_corr(x, tau = i)
    counter <- counter + 1
  }
  max(abs(auto_corrs))
}
