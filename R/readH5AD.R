#' H5AD to data frame
#'
#' Parse data from a h5ad object into a data frame.
#'
#' The function makes use of two non-Cran/-Bioconductor packages:
#' satijalab/seurat-data and mojaveazure/seurat-disk that need to be installed from GitHub.
#' The packages at the time of the development are under experimental stage if the development.
#'
#' @param file_path Path to the file to be parsed.
#' @return A dataframe containing data from the h5ad object.
h5ad_to_dataframe <- function(file_path) {
  dir <- dirname(file_path)
  file <- basename(file_path)
  file_name <- tools::file_path_sans_ext(file)
  SeuratDisk::Convert(file_path, dest = "h5seurat", overwrite = TRUE)
  file_name <- paste(dir, file_name, sep = "/")
  new_file <- paste(file_name, "h5seurat", sep = ".")
  seu <- SeuratDisk::LoadH5Seurat(new_file)
  return(seurat_to_dataframe(seu))
}
