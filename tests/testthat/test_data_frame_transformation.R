library(testthat)

# Test log(x + 1) transformation
test_that("log transformation test", {
  df <- data.frame("V1" = c(log(1), log(4), log(7)), "V2" = c(log(2), log(5), log(8)), "V3" = c(log(3), log(6), log(9)))
  df_result <- data.frame("V1" = c(0, 3, 6), "V2" = c(1, 4, 7), "V3" = c(2, 5, 8))

  df_result <- log_transformation(as.data.frame(df_result))
  expect_equal(df, df_result)
})

# Test arcsinh(x / cofactor) transformation, default
test_that("arcsinh (cof = default) transfromation test", {
  df <- data.frame("V1" = c(asinh(1), asinh(2), asinh(3)), "V2" = c(asinh(4), asinh(5), asinh(6)), "V3" = c(asinh(7), asinh(8), asinh(9)))
  df_result <- data.frame("V1" = c(5, 10, 15), "V2" = c(20, 25, 30), "V3" = c(35, 40, 45))

  df_result <- asinh_transformation(df_result)
  expect_equal(df, df_result)
})


test_that("arcsinh (cof = 2) transformation test", {
  df <- data.frame("V1" = c(asinh(0), asinh(6), asinh(8)), "V2" = c(asinh(10), asinh(12), asinh(14)), "V3" = c(asinh(16), asinh(18), asinh(20)))
  df_result <- data.frame("V1" = c(0, 12, 16), "V2" = c(20, 24, 28), "V3" = c(32, 36, 40))

  df_result <- asinh_transformation(df_result, cof = 2)
  expect_equal(df, df_result)

})

test_that("log_t test", {
  a <- 2
  b <- log(3)
  expect_equal(log_t(a), b)
})
