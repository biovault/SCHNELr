# import parser and clustering of python implementation to R.
# Used over R implementation for speed in reading binary file.
reticulate::source_python("./py/src_clustering_HSNE_parser.py", envir = parent.env(parent.frame()))

#' Parses and clusters a HSNE file.
#'
#' @param filepath Path to HSNE file.
#' @param scale_num Scale number to cluster.
#' @param prop_method Leiden algorithm prop method. Either "cluster" or "label"
#' @return A vector of labels.
parse_and_cluster <- function(filepath, scale_num, prop_method = "cluster") {
  #hsne <- pyClus.read_HSNE_binary(filepath)
  hsne <- reticulate::py$HSNE_parser$read_HSNE_binary(filepath)
  #hsne <- reticulate::py$read_HSNE_binary(filepath)
  result <- hsne$cluster_scale(scale_num, prop_method)
  return(result)
}

#' Get number of scales in HSNE.
#'
#' @param filepath Path to HSNE file.
#' @return An integer of number of scales.
get_num_scales <- function(filepath) {
  #hsne <- pyClus.read_HSNE_binary(filepath)
  hsne <- reticulate::py$HSNE_parser$read_HSNE_binary(filepath)
  #hsne <- reticulate::py$read_HSNE_binary(filepath)
  return(hsne$num_scales)
}
