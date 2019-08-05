test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

# Q1b

test_that("default matrix is 5 by 5 with only 0s, 1s, and 2s", {
  def_mat <- generate_board_mat()
  expect_equal(dim(def_mat)[1], 5)
  expect_equal(dim(def_mat)[2], 5)
  expect_equal(sum(def_mat %in% c(0, 1, 2)), 25)
})

test_that("matrix is 7 by 7 with only 0s, 1s, and 2s", {
  mat <- generate_board_mat(n = 7)
  expect_equal(dim(mat)[1], 7)
  expect_equal(dim(mat)[2], 7)
  expect_equal(sum(mat %in% c(0, 1, 2)), 49)
})

test_that("p = 0 gives a board with all 1’s", {
  mat <- generate_board_mat(p = 0)
  expect_equal(sum(mat == 1), 25)
})

test_that("p = 1 gives a board with all 0’s", {
  mat <- generate_board_mat(p = 1)
  expect_equal(sum(mat == 0), 25)
})


test_that("throws reasonable errors", {
  expect_error(generate_board_mat(n = c(1, 2)))
  expect_error(generate_board_mat(n = "asdf"))
  expect_error(generate_board_mat(n = 5.4))
  expect_error(generate_board_mat(n = -5))
})


# Q1c

# Invalidity: Input parameter is a vector, not a matrix
test_that("input paramter is not a matrix", {
  expect_error(is_valid(generate_board_mat(n = c(1, 2))))
})


test_that("check if is_valid functions correctly", {
  expect_error(is_valid(generate_board_mat(n = c(1, 2, 3)))) # Input paramter is a vector, not a matrix
  expect_error(is_valid(mat = matrix(nrow = 7, ncol = 5))) # Input paramter is a matrix, but not a square matrix
  expect_error(is_valid(mat = matrix(1:25, nrow = 5, ncol = 5))) # Input parameter is  square matrix, but contains values other than 0s, 1s, and 2s
})

test_that("Input paramter is a vector, not a matrix", {
  expect_error(is_valid(generate_board_mat(n = c(1, 2, 3))))
})

test_that("Input paramter is a matrix, but not a square matrix", {
  expect_error(is_valid(mat = matrix(nrow = 7, ncol = 5)))
})

test_that("Input parameter is  square matrix, but contains values other than 0s, 1s, and 2s", {
  expect_error(is_valid(mat = matrix(1:25, nrow = 5, ncol = 5)))
})


# Q4

# Q4b

test_that("read_board() works" , {
  load(url("https://raw.githubusercontent.com/benjaminleroy/36-350-summer-data/master/Week5/percolate_test.Rdata"))
  board_list_read <- read_boards("https://raw.githubusercontent.com/benjaminleroy/36-350-summer-data/master/Week5/percolation_write_test.txt")
  expect_identical(board_list, board_list_read)
})

# Q4c

# Test 1

test_that("read_board() returns a list containing NA if a non-square matrix is input" ,{
  t1 <- read_boards("https://raw.githubusercontent.com/benjaminleroy/36-350-summer-data/master/Week5/percolation_write_test1.txt")
  expect_equal(t1, list(NA))
})

# Test 2

test_that("read_board() returns a list containing NA if the matrix texts contains anything except * and ." ,{
  t2 <- read_boards("https://raw.githubusercontent.com/benjaminleroy/36-350-summer-data/master/Week5/percolation_write_test2.txt")
  expect_equal(t2, list(NA))
})

# Test 3

test_that("read_board() returns a list containing NA if the dimension of square matrix represented in the text file is missing" ,{
  t3 <- read_boards("https://raw.githubusercontent.com/benjaminleroy/36-350-summer-data/master/Week5/percolation_write_test3.txt")
  expect_equal(t3, list(NA))
})

# Test 4

test_that("read_board() returns a list containing NA if a matrix in the text file doesn't end with four dashes" ,{
  t4 <- read_boards("https://raw.githubusercontent.com/benjaminleroy/36-350-summer-data/master/Week5/percolation_write_test4.txt")
  expect_equal(t4, list(NA))
})

# Test 5

test_that("read_board() returns a list containing NA if a matrix contains any empty spaces" ,{
  t5 <- read_boards("https://raw.githubusercontent.com/benjaminleroy/36-350-summer-data/master/Week5/percolation_write_test5.txt")
  expect_equal(t5, list(NA))
})

# Test 6

test_that("read_board() returns a list containing NA if the dimension of square matrix represented in the text file is negative" ,{
  t6 <- read_boards("https://raw.githubusercontent.com/benjaminleroy/36-350-summer-data/master/Week5/percolation_write_test6.txt")
  expect_equal(t6, list(NA))
})
