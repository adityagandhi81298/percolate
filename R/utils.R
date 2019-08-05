# Q1a

#' generate_board_mat function; generates a board matrix given 'n' and 'p' input
#'
#' @param n a positive integer denote the size of the board
#' @param p a number between 0 and 1 that denotes the fraction of the n^2 squares are blocked
#'
#' @return a matrix with random (0, 1) (n^2 size with ((n^2) * p) number of blocked (0), and the rest non blocked = (n^2) - blocked(0)
#' @export
#'
#' @examples generate_board_mat(), generate_board_mat(n = 5, p = 0.25), generate_board_mat(n = 8, p = 0.75)

generate_board_mat <- function(n = 5, p = 0.25) {
  assert_that(is.numeric(n)) # n is numeric
  assert_that(n > 0) # n is a positive integer
  assert_that(n %% 1 == 0) # n is an integer, not a floating point number
  assert_that(is.numeric(p)) # p is numeric
  assert_that(p >= 0 & p <= 1) # p is between 0 and 1 (both included)

  blocked <- floor((n^2) * p)
  non_blocked <- (n^2) - blocked
  gen_blocks <- sample(c(rep(0, blocked), rep(1, non_blocked)))
  return(matrix(gen_blocks, nrow = n, ncol = n))
}

# Q1c

#' is_valid function; checks whether a matrix is a valid board matrix
#'
#' @param mat a matrix, if valid, a square matrix that contains only 0s, 1s, and 2s
#'
#' @return error if input is not a matrix, or if the input matrix is not square, or if the the input sqaure matrix does not contain only 0s and 1s; returns TRUE otherwise.
#' @export
#'
#' @import assertthat
#'
#' @examples is_valid(generate_board_mat()), is_valid(generate_board_mat(n=1))

is_valid <- function(mat) {
  assert_that(is.matrix(mat))
  # check that mat is indeed a matrix
  assert_that(dim(mat)[1] == dim(mat)[2])
  # check that mat is indeed a square matrix
  assert_that(sum(mat %in% c(0, 1, 2)) == (dim(mat)[1]^2))
  # check that mat contains only 0s, 1s, and 2s
  return(TRUE)
}

# Q4

# Q4a

#' transform_one function; transforms a string of "*" to 0 and "." to 1
#'
#' @param s a string, containing "*" and "."
#'
#' @return vector of 0 and 1
#' @export
#'
#' @examples transform_one("***.")

transform_one <- function(s) {
  res_vec <- vector()
  for (i in 1:nchar(s)) {
    if (substr(s, i, i) == "*") {
      res_vec <- c(res_vec, 0)
    }
    else if (substr(s, i, i) == ".") {
      res_vec <- c(res_vec, 1)
    }
  }
  res_vec
}


#' transform_vec function; transforms a vector of strings containing "*" and "." to a "board" object
#'
#' @param v vector of strings of "*" and "."
#' @param d integer, dimensions of matrix
#'
#' @return "board" object, superclass "matrix" containing only 0s and 1s
#' @export
#'
#' @examples transform_vec(c("."), 1), transform_vec(c("*"), 1)

transform_vec <- function(v, d) {
  res_vec <- vector()
  for (i in 1:length(v)) {
    vec <- transform_one(v[i])
    res_vec <- c(res_vec, vec)
  }
  mat <- matrix(res_vec, d, d, byrow = TRUE)
  board(mat)
}


#' check_contain function; checks whether the vector contains only "*" and "."
#'
#' @param v a vector to be check, if TRUE contains only "*" and "."; returns FALSE otherwise
#'
#' @return returns TRUE if the vector contains on;y "*" and "."; returns FALSE otherwise
#' @export
#'
#' @examples

check_contain <- function(v) {
  for (i in 1:nchar(v)) {
    if (substr(v, i, i) != "*" && substr(v, i, i ) != "." && substr(v, i, i ) != " ") {
      return(FALSE)
    }
  }
  return(TRUE)
}


#' read_boards() function; takes in a filepath to a text file and outputs a list of matrices that the text file represents
#'
#' @param file text file that contains boards;
#'
#' @return list of matrices that the text file represents
#' @export
#'
#' @examples read_boards("https://raw.githubusercontent.com/benjaminleroy/36-350-summer-data/master/Week5/percolation_write_example.txt")

read_boards <- function(file) {
  txt <- readLines(file)
  txt <- txt[txt != ""]
  txt <- trimws(txt)

  res_lst <- vector(mode = "list")
  bool <- FALSE
  for (i in 1:length(txt)) {
    if (txt[i] == "----" && i != length(txt)) {
      if (!is.na(as.numeric(txt[i+1])) && as.numeric(txt[i+1]) > 0) {
        dim <- as.numeric(txt[i+1])
        for (j in ((i+2):(i+1+dim))) {
          if ((i+2+dim > length(txt)) | (txt[i+2+dim] != "----") | (nchar(txt[j]) != (2*dim-1)) | (check_contain(txt[j]) == FALSE)) {
            bool <- TRUE
          }
        }
        if (bool == FALSE) {
          ve <- txt[(i+2):(i+1+dim)]
          #print(ve)
          lst <- transform_vec(ve, dim)
          #print(lst)
          res_lst <- c(res_lst, list(lst))
        }
        else {
          res_lst <- c(res_lst, list(NA))
        }
      }
      else {
        res_lst <- c(res_lst, list(NA))
      }
    }
  }
  assert_that((sum(txt == "----") - 1) == length(res_lst), msg = "file is not properly formatted")
  res_lst
}
