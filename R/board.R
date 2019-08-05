# Q1d

#' An object with superclass "matrix" and subclass "board"
#'
#' @param mat a matrix (if valid, a square matrix that contains only 0s, 1s, and 2s)
#' @param n a positive integer denote the size of the board
#' @param p a number between 0 and 1 that denotes the fraction of the n^2 squares are blocked
#'
#' @return 'board' sub - class (matrix primary class) object; if matrix input paramter is incorrect, return an error
#' @export
#'
#' @examples board(mat = NULL, n = 5, p = 0.25), board(generate_board_mat())

board <- function(mat = NULL, n = 5, p = 0.25) {
  if (is.null(mat) == TRUE) {
    object <- generate_board_mat(n, p)
    class(object) <- c("board", "matrix")
    attr(object, "n") = n
    attr(object, "p") = p
  } else {
    is_valid(mat)
    object <- mat
    class(object) <- c("board", "matrix")
    attr(object, "n") = dim(mat)[1]
    attr(object, "p") = sum(mat == 0) / (dim(mat)[1]^2)
  }
  object
}
