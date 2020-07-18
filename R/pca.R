#' PCA on a data frame
#'
#' Perform PCA on a data frame and return a specified number of principal components.
#'
#' @param df Data frame on which PCA is performed.
#' @param n_components The desired number of principal components to keep
#' @return Data frame with PCA transformation
pca <- function(df, n_components = 50) {
  if (ncol(df) > 50) {n
    message("Applying PCA")
    n_comp = min(ncol(df), n_components)
    res = prcomp(df, rank. = n_comp)
    to_ret <- predict(res, df)
    return(as.data.frame(to_ret))
  } else {
    message("Number of features < 50. PCA not applied")
    return(as.data.frame(df))
  }
}
