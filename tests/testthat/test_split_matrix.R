library(testthat)

test_that("Test splitting a matrix into 3 equal ones", {
  mat <- matrix(1:60, nrow = 10, ncol = 6, byrow = T)
  mat1 <- matrix(1:12, nrow = 2, ncol = 6, byrow = T)
  mat2 <- matrix(13:42, nrow = 5, ncol = 6, byrow = T)
  mat3 <- matrix(43:60, nrow = 3, ncol = 6, byrow = T)

  list <- list(mat1, mat2, mat3)
  vec <- c(2, 5, 3)

  expect_equal(split_matrix(mat, vec), list)
})
