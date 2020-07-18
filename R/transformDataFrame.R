library(dplyr)

#' ln(val + 1) function.
#'
#' Add 1 to the value in case it is 0.
#' @param val a numerical value
#' @return natural logarithm of (val + 1)
log_t <- function(val) {
  return(log(val + 1))

}

#' log(x + 1) transformation on a data frame
#'
#' Perform a logarithmic transformation on every element X of the data frame where, x = ln(x+1).
#'
#' @param df A data frame with elements to be transformed.
#' @return A data frame with applied transformation.
log_transformation <- function(df) {
  df %>% dplyr::mutate_each(dplyr::funs(log_t))
}

#' asinh(x / cofactor) transformation on a data frame
#'
#' Perform a arcsinh transformation on every element X of the data frame where, x = asinh(x/cofactor).
#'
#' @param df A data frame with elements to be transformed.
#' @param cof cofactor
#' @return A data frame with applied transformation.
asinh_transformation <- function(df, cof) {
  temp = df/cof
  temp %>% dplyr::mutate_each(dplyr::funs(asinh))
}
