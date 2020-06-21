#' CSV to data frame
#'
#' Parse csv file into a data frame.
#'
#' @param file_path Location of the file to be read in.
#' @param header Boolean specifying whether to keep the header from the csv file.
#' @return Data frame containing values from the .csv file.
csv_to_dataframe <- function(file_path, header = F) {
  read.csv(file_path, header)
}







