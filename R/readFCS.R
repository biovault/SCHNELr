#' FCS to data frame
#'
#' Parse data from a .fcs file into a data frame.
#'
#' The function makes use of a non-Cran\-Bioconductor package ParkerICI/grappolo/
#' that needs to be installed from GitHub.
#' @param file_path The Location of the file to read in as data frame.
#' @return Data frame containing data from the .fcs file
fcs_to_dataframe <- function(file_path) {
  grappolo::convert_fcs(flowCore::read.FCS(file_path, transformation = FALSE))
}

