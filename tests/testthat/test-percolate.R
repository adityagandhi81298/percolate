test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})


# Q2b

# (1)

test_that("percolate() works on a matrix with all open sites", {
  my_board <- board(matrix(1, nrow = 10, ncol = 10))
  lst <- percolate(my_board)
  expect_true(lst$result_board == board(matrix(2, nrow = 10, ncol = 10)) && lst$result == TRUE)
})


# (2)

test_that("percolate() works on a matrix with all blocked sites", {
  my_board <- board(matrix(0, nrow = 10, ncol = 10))
  lst <- percolate(my_board)
  expect_true(lst$result_board == board(matrix(0, nrow = 10, ncol = 10)) && lst$result == FALSE)
})


# (3)

test_that("percolate() works on any valid matrix with all the bottom row squares blocked", {
  my_board <- board(matrix(c(0,0,0,0,0,
                             1,1,1,1,1,
                             1,1,1,1,1,
                             1,1,1,1,1,
                             1,1,1,1,1), 5, 5, byrow = TRUE))
  lst <- percolate(my_board)
  expect_true(lst$result_board == matrix(c(0,0,0,0,0,
                                           1,1,1,1,1,
                                           1,1,1,1,1,
                                           1,1,1,1,1,
                                           1,1,1,1,1), 5, 5, byrow = TRUE) && lst$result == FALSE)
})



# (4)

test_that("percolate() works on any valid matrix with all the top row squares blocked", {
  my_board <- board(matrix(c(1,1,1,1,1,
                             1,1,1,1,1,
                             1,1,1,1,1,
                             1,1,1,1,1,
                             0,0,0,0,0), 5, 5, byrow = TRUE))
  lst <- percolate(my_board)
  expect_true(lst$result_board == matrix(c(2,2,2,2,2,
                                           2,2,2,2,2,
                                           2,2,2,2,2,
                                           2,2,2,2,2,
                                           0,0,0,0,0), 5, 5, byrow = TRUE) && lst$result == FALSE)
})


# Q2c

test_that("percolate.board() works with all the test cases",{
  load(url("https://raw.githubusercontent.com/benjaminleroy/36-350-summer-data/master/Week5/percolate_test.Rdata"))

  your_result_list <- lapply(board_list, percolate)

  bool_vec <- sapply(1:length(result_list), function(x){
    your_board <- your_result_list[[x]]$result_board
    result_board <- result_list[[x]]$result_board

    identical(your_board, result_board) *
      (your_result_list[[x]]$result == result_list[[x]]$result)
  })

  expect_true(all(bool_vec))
})
