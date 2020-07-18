library(testthat)

test_that("log transformation test", {
  row1 <- c(log(1), log(2), log(3))
  row2 <- c(log(4), log(5), log(6))
  row3 <- c(log(7), log(8), log(9))

  df <- data.frame(row1, row2, row3)
  row1 <- c(0, 1, 2)
  row2 <- c(3, 4, 5)
  row3 <- c(6, 7, 8)

  df2 <- data.frame(row1, row2, row3)
  df2 <- log_transformation(df2)
  expect_equal(df, df2)
})
