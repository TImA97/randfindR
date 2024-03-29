% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rng_indices.R
\name{rng2_index}
\alias{rng2_index}
\title{Compute RNG2 index}
\usage{
rng2_index(x, options, circ = TRUE)
}
\arguments{
\item{x}{vector of distinct options of numbers or characters}

\item{options}{number of available options in sequence}

\item{circ}{indicate whether to include wrap around from end to the beginning
of the sequence when computing response pairs}
}
\value{
RNG2 index of \code{x}
}
\description{
Compute RNG2 index
}
\details{
This function takes a vector \code{x} and computes an index that reflects
whether the distribution of response pairs in the sequence is uneven given
the number of possible \code{options}. In this modified version of the
RNG index response pairs are constructed over the distance of two values,
e.g., in the sequence c(1,2,3) the first response pair is (1,3).
The scores of this index range between 0 and 1. A score of 1  indicates
complete redundancy of response pairs (e.g., '1-2' in '1-1-2'),
whereas a score of 0  indicates an even distribution of response pairs.
}
\examples{
rng2_index(ginsburg1994, 10)
rng2_index(ginsburg1994, 10, circ = FALSE)
rng2_index(evans1978[, 1], 10)
rng2_index(evans1978[, 2], 10)

}
\references{
Towse, J.N., Neil, D. Analyzing human random generation behavior: A review of
methods used and a computer program for describing performance. Behavior
Research Methods, Instruments, & Computers 30, 583–591 (1998).
\doi{doi:10.3758/BF03209475}
}
