% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/checker.R
\name{checker}
\alias{checker}
\title{Test for optimal rotation vectors}
\usage{
checker(g3, rotation_vector)
}
\arguments{
\item{g3}{A matrix. This is the coded psuedo-design matrix with each element being 0, 1, or 2. Each row corresponds to one generator.}

\item{rotation_vector}{This is the rotation vector that is to be evaluated.}
}
\value{
A List. 
\item{rotation vector}{This is the vector you put in from the parameters}
\item{output}{A data frame with one column being the effect and the second col being the level}
}
\description{
For some set of generators, this function will show what the corresponding effect the rotation vector will have on it. Most importantly if the effects level come out to be 0 then it is rotationally tolerant.
}
\examples{
\dontrun{

g3 <- c(1, 2, 1, 0, 0, 0,
      0, 0, 1, 0, 2, 2,
      0, 0, 1, 1, 0, 1,
       0, 0, 1, 2, 1, 1,
       0, 0, 0, 1, 1, 2)
g3 <- matrix(q, nrow = 5, byrow = TRUE)
checker(g3, c(2,1,1,0,1,1))
}



}
