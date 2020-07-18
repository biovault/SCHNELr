library(tools)
library(dplyr)
library(purrr)

#' Read data into a matrix
#'
#' Parse the data from the file specified by the file_path into a NumericMatrix.
#' Takes in .csv, .fcs files as well as objects of type Seurat, h5ad, SingleCellExperiment.
#' Applies log or asinh transformation with cofactor to data if specified.
#'
#' @param file_path Location of the file to be read in
#' @param transformation Transformation applied to data prior to clustering: "log"/"arcsinh"/"FALSE, default = FALSE
#' @param cofactor Optional. Cofactor of the arcsinh(x / cofactor) transformation, default = 5
#' @param csv_header Boolean specifying if the file contains a header (csv only), default = T
#' @param features_after_pca Number of principal components to keep after performing PCA on the data, default = 50
#' @return A NumericMatrix representation of data in the file
#' @examples
#' parse_to_matrix("../../data/file_to_read.csv", transformation = "log", csv_header = F)
#' parse_to_matrix("../../data/file_to_read.fcs", "arcsinh", cofactor = 10)
#' parse_to_matrix("../../data/file_to_read.rds")
#' @export
parse_to_matrix <- function(file_path, transformation = F, cofactor = 5, csv_header = T, features_after_pca = 50, sce = FALSE) {
  # Check if cofactor is a valid number
  if (cofactor == 0)
    stop("The cofactor cannot be 0!")

  # Check if input is an object
  if (is.matrix(file_path)) {
    tdata <- file_path

  } else {
  ext = tools::file_ext(file_path)
  if (ext == "csv") {
    tdata <- csv_to_dataframe(file_path, csv_header)
    tdata <- pca(tdata, features_after_pca)
  } else if (ext == "fcs") {
    tdata <- fcs_to_dataframe(file_path)
    tdata <- pca(tdata, features_after_pca)
  } else if (ext == 'rds' && sce == TRUE) {
    tdata <- sce_to_dataframe(file_path)
    tdata <- pca(tdata, features_after_pca)
  } else if (ext == "rds") {
    s_obj <- readRDS(file_path)
    tdata <- seurat_to_dataframe_pca(s_obj, features_after_pca)
  } else if (ext == "h5ad") {
    tdata <- h5ad_to_dataframe(file_path)
    tdata <- pca(tdata, features_after_pca)
  } else {
    stop("Unsupported file extension!")
  }}

  if (transformation == "log") {
    tdata = log_transformation(tdata)
  } else if (transformation == "arcsinh") {
    tdata <- as.data.frame(tdata)
    tdata = asinh_transformation(tdata, cofactor)
  }

  to_ret = as.matrix(tdata)
  rownames(to_ret) <- c()

  return(to_ret)
}


#' Parse data from files in a specified directory into a NumericalMatrix.
#' @param dir_path Path to the directory with files to be parsed.
#' @param transformation Transformation applied to data prior to clsutering: "log"/"arcsinh"/"FALSE, default = FALSE
#' @param cofactor Cofactor used in the arcsinh(x / cofactor) transformation
parse_multiple_to_matrix <- function(dir_path, transformation = F, cofactor = 5, csv_header = T, features_after_pca = 50) {
  # Create a list of names of the files inside the given directory
  files_list <- list.files(dir_path)

  # Parse a single file to get the number of features of the objects
  tmp <- parse_to_matrix(paste(dir_path, files_list[1], sep=""))

  # Initialize an empty matrix to hold data from all the files
  matrix_to_return <-matrix(ncol = ncol(tmp))

  # Delete the dummy row
  matrix_to_return <- matrix_to_return[-1,]

  # Create a vector to store the number of records in each file
  m_row_count <- vector(mode = "integer")

  lapply(files_list, function(x) {
    matrix <- parse_to_matrix(paste(dir_path, x, sep=""))
    matrix_to_return <<- rbind(matrix_to_return, matrix)
    m_row_count <<- append(m_row_count, nrow(matrix))
  })

  return(list(matrix_to_return, m_row_count))
}

