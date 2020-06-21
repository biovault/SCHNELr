# Split the input matrix into matrices holding number of records specified in the input vector
split_matrix <- function(matrix, vec) {
  start <- 1
  end <- 0
  to_ret <- list()
  lapply(vec, function(x) {
    end <<- start + x - 1
    to_ret[[length(to_ret) + 1]] <<- as.matrix(matrix[start : end, ])
    start <<- start + vec[[length(to_ret)]]
  })
  to_ret
}

