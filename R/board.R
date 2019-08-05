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
#' @examples board(generate_board_mat())

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


# Q1d

#' plot.board method; plots a "board" class object
#'
#' @param x a "board" object (superclass - matrix, subclass - board)
#'
#' @return a tile plot containing squares that represent the elements of "board" (superclass matrix); black for 0s, white for 1s, and light blue for 2s
#' @export
#'
#' @examples plot(board_example, grid = TRUE), plot(board_example2, grid = TRUE), plot(board_example3, grid = TRUE), plot(board_example4, grid = TRUE)

plot.board <- function(x){
  is_valid(x)
  n <- attr(x, "n")
  df <- tidyr::gather(data.frame(row = 1:n, x),
                      key = "column", value = "value", -row)
  df$column <- as.numeric(substr(df$column, 2, nchar(df$column)))
  df$value <- factor(df$value, levels = c(0, 1, 2))
  ggplot(data = df, aes(x = column, y = (max(row)-row+1))) +
    geom_tile(aes(fill = value)) +
    scale_fill_manual(values = c("0" = "black", "1" = "white", "2" = "lightblue3")) +
    theme(legend.position = "none") +
    theme_void() +
    labs(x = "Row", y = "Column", title = paste("Size:", n))
}


# Q1e

#' plot.board method updated; includes a grid which if TRUE is represneted visually by gray dashed lines
#'
#' @param x a "board" object (superclass - matrix, subclass - board)
#' @param grid a boolean, to decide whether the plot should contain dashed grid lines or not
#'
#' @return a tile plot containing squares that represent the elements of "board" (superclass matrix); black for 0s, white for 1s, and light blue for 2s; if grid = TRUE, adds dashed gray lines to geom_tile
#' @export
#'
#' @examples plot(board_example, grid = TRUE), plot(board_example2, grid = TRUE), plot(board_example3, grid = TRUE), plot(board_example4, grid = TRUE)

plot.board <- function(x, grid = TRUE){
  is_valid(x)
  n <- attr(x, "n")
  df <- tidyr::gather(data.frame(row = 1:n, x),
                      key = "column", value = "value", -row)
  df$column <- as.numeric(substr(df$column, 2, nchar(df$column)))
  df$value <- factor(df$value, levels = c(0, 1, 2))
  plot_board <- ggplot(data = df, aes(x = column, y = (max(row)-row+1)))

  if (grid == TRUE) {
    plot_board <- plot_board + geom_tile(aes(fill = value), color = "gray", linetype = "dashed")
  } else {
    plot_board <- plot_board + geom_tile(aes(fill = value))
  }
  plot_board +
    scale_fill_manual(values = c("0" = "black", "1" = "white", "2" = "lightblue3")) +
    theme(legend.position = "none") +
    theme_void() +
    labs(x = "Row", y = "Column", title = paste("Size:", n))
}
