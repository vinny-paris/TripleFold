% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fraction_creator.R
\name{part_single}
\alias{part_single}
\title{Creates a 3^(f-1) Regular Fraction}
\usage{
part_single(x, l, tree)
}
\arguments{
\item{x}{This is the identifiers for the design factors. It should be a vector with each element being a character string or reducable to a character string. Needs to be of f length.}

\item{l}{This is the single equation that the new design is to be generated from. It is a vector of f length with each element being 0, 1 or 2}

\item{tree}{A single number that is 0, 1, or 2 which will be the solution to equation from l (i.e. 1*x + 2*y + 0*z mod 3 = tree). This dictates the fraction you want.}
}
\value{
A list will be returned. 
\item{Short Design}{A matix. This will return all relevant factors and levels as solutions to the design discluding those factors which simply "cycle through" the design matrix.}
\item{Expansion Variables}{A vector. The short design should be copied three times, with a new column for the expansion variable. For each copy increase the expansion variables level by one starting at 0. Continue to grow as needed for as many variables as there are in this category.}
}
\description{
This will create a regular fractional factorial design by solving a single generating equation.
}
