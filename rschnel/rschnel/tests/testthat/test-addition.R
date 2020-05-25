

testing_data <- 5

test_that("test simple addition", {
  expect_equal(dodawanie(2, 3), testing_data)
})

test_that("addition, passing test", {
  expect_equal(dodawanie(1, testing_data), 6)
})
