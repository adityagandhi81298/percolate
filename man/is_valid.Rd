% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{is_valid}
\alias{is_valid}
\title{is_valid function; checks whether a matrix is a valid board matrix}
\usage{
is_valid(mat)
}
\arguments{
\item{mat}{a matrix, if valid, a square matrix that contains only 0s, 1s, and 2s}
}
\value{
error if input is not a matrix, or if the input matrix is not square, or if the the input sqaure matrix does not contain only 0s and 1s; returns TRUE otherwise.
}
\description{
is_valid function; checks whether a matrix is a valid board matrix
}
\examples{
is_valid(generate_board_mat()), is_valid(generate_board_mat(n=1))
}
