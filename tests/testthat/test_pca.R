library(testthat)

test_that("pca test: dataframe with < 50 columns/features", {
  df <- data.frame("V1" = c(0, 3, 6),
                          "V2" = c(1, 4, 7),
                          "V3" = c(2, 5, 8))
  df_result <- data.frame("V1" = c(0, 3, 6),
                          "V2" = c(1, 4, 7),
                          "V3" = c(2, 5, 8))

  df_result <- pca(df_result)
  expect_equal(df, df_result)
})

#test_that("pca test: dataframe with 50 columns/features", {
#  df <- a.data.frame(matrix(1:1650, ncol = 55, nrow = 30, byrow = T))
#
#  df_result <- pca(df_result)
#  expect_equal(df, df_result)
#})


