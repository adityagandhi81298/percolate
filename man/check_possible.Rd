% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/percolate.R
\name{check_possible}
\alias{check_possible}
\title{check_possible function, a helper function which checks whether an open & dry board square is surrounded by an open & flood board square}
\usage{
check_possible(x)
}
\arguments{
\item{x}{a "board" object (superclass - matrix, subclass - board)}
}
\value{
TRUE if n open & dry board square is surrounded by an open & flood board square; returns FALSE otherwise
}
\description{
check_possible function, a helper function which checks whether an open & dry board square is surrounded by an open & flood board square
}
\examples{
check_possible(board(generate_board_mat()))
}
