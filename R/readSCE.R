#' Parse a SingleCellExperiment object into a data frame.
#'
#' @param file_path File path to the SingleCellExperiment to be parsed into a data frame.
#' @return A data frame containing data from the SingleCellExperiment object.
sce_to_dataframe <- function(file_path) {
  sce <- readRDS(file_path)
  seu <- Seurat::as.Seurat(sce)
  return(seurat_to_dataframe(seu))
}
