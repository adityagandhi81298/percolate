test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})


# Q1d

test_that("basic structure equivalence", {
  t_mat <- matrix(0, 5, 5)
  t_board <- board()
  expect_equivalent((class(unclass(t_board))), class(t_mat))
  expect_equivalent(attr(t_board, "n"), nrow(t_mat))
  expect_equivalent(attr(t_board, "p"), 0.25)
})

test_that("equivalent matrix structure", {
  t_board <- board()
  expect_equivalent(unclass(t_board), unclass(board(t_board)))
})

test_that("empirical values are correct", {
  t_mat <- generate_board_mat()
  t_board <- board(mat = t_mat)
  expect_equal(attr(t_board, "n"), 5)
  expect_equal(((attr(t_board, "n")^2) * attr(t_board, "p")), floor(0.25 * 25))
})

test_that("check incorrect matrix errors", {
  # input is a vector, not a matrix
  expect_error(board(mat = c(1, 2, 3, 4)))
  # not a square matrix
  expect_error(board(mat = matrix(nrow = 7, ncol = 5)))
  # values not only 0s, 1s, and 2s
  e_mat <- matrix(1:36, nrow = 6, ncol = 6)
  expect_error(board(mat = e_mat))
})


test_that("check board class with some n, p", {
  t_mat <- generate_board_mat(n = 6)
  t_board <- board(mat = t_mat)
  expect_equal(attr(t_board, "n"), 6)
  expect_equal(((attr(t_board, "n")^2) * attr(t_board, "p")), floor(0.25 * 36))

  t_mat2 <- generate_board_mat(n = 6, p = 0.35)
  t_board2 <- board(mat = t_mat2)
  expect_equal(attr(t_board2, "n"), 6)
  expect_equal(((attr(t_board2, "n")^2) * attr(t_board2, "p")), floor(0.35 * 36))
})
