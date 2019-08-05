# Q2a

#' check_possible function, a helper function which checks whether an open & dry board square is surrounded by an open & flood board square
#'
#' @param x a "board" object (superclass - matrix, subclass - board)
#'
#' @return TRUE if n open & dry board square is surrounded by an open & flood board square; returns FALSE otherwise
#' @export
#'
#' @examples check_possible(board(generate_board_mat()))

check_possible <- function(x) {
  n <- attr(x, "n")
  dirs <- list(list(0, -1), list(0, 1), list(1, 0), list(-1, 0))
  for (i in 1:n) {
    for (j in 1:n) {
      if (x[i, j] == 1) {
        for (d in 1:4) {
          d1 <- dirs[[d]][[1]]
          d2 <- dirs[[d]][[2]]
          if ((1 <= (i + d1) && (i + d1) <= n && 1 <= (j + d2) && (j + d2) <= n) && (x[i + d1, j + d2] == 2)) {
            return(TRUE)
          }
        }
      }
    }
  }
  return(FALSE)
}


#' percolate.board function; determines result_board given input "board" object and returns the boolean result of percolation
#'
#' @param x a "board" object (superclass - matrix, subclass - board)
#'
#' @return a list of the result_board and the result i.e. whether the "board" percolates (TRUE) or not (FALSE)
#' @export
#'
#' @examples percolate(board_example_list[[1]]), percolate(board_example_list[[2]]), percolate(board_example_list[[3]])

percolate.board <- function(x) {
  is_valid(x)
  n <- attr(x, "n")
  assert_that(all(x %in% c(0,1)), msg = "not a valid matrix")
  # fill first row
  for (j in 1:n) {
    # print(x[1, j])
    if (x[1, j] == 1) {
      # print (x[1, j])
      x[1, j] = 2
    }
  }
  # fill remaining board
  dirs <- list(list(0, -1), list(0, 1), list(1, 0), list(-1, 0))
  while(check_possible(x)) {
    for (i in 1:n) {
      for (j in 1:n) {
        if (x[i, j] == 2) {
          for (d in 1:4) {
            d1 <- dirs[[d]][[1]]
            d2 <- dirs[[d]][[2]]
            if ((1 <= (i + d1) && (i + d1) <= n && 1 <= (j + d2) && (j + d2) <= n) && (x[i + d1, j + d2] == 1)) {
              x[i + d1, j + d2] = 2
            }
          }
        }
      }
    }
  }
  return(list(result_board = x, result = any(x[nrow(x), ] == 2)))
}

#' UseMethod for percolate; indicates that for a "board" class object, percolate is the method used for percolate.board
#'
#' @param x a "board" object (superclass - matrix, subclass - board)
#'
#' @return a list of the result_board and the result i.e. whether the "board" percolates (TRUE) or not (FALSE)
#' @export
#'
percolate <- function(x) {
  UseMethod("percolate", x)
}
