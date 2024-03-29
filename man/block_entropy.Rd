% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/block_entropy.R
\name{block_entropy}
\alias{block_entropy}
\title{Compute Block Entropy}
\usage{
block_entropy(x, block_size = 1)
}
\arguments{
\item{x}{vector of distinct options of numbers or characters}

\item{block_size}{length of blocks in which the original sequence should be
divided for analysis}
}
\value{
block entropy of \code{x} for the specified \code{block_size}
}
\description{
Compute Block Entropy
}
\details{
This function takes a vector \code{x} and uses it to compute its entropy for
the specified \code{block_size}, often referred to as \emph{k}. The default
implementation sets the \code{block_size} to 1 which is equivalent to
Shannon Entropy.
}
\examples{
block_entropy(c(1, 1, 1, 2, 2, 2))
block_entropy(c(1, 1, 1, 2, 2, 2), block_size = 2)

}
\references{
Shannon, C. E. (n.d.). A Mathematical Theory of Communication. 55.
\url{https://people.math.harvard.edu/~ctm/home/text/others/shannon/entropy/entropy.pdf}
}
