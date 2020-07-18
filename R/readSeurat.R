#' Seurat object to dataframe
#'
#' Extract data from the Seurat object into a dataframe.
#'
#' @param obj A seurat object from which data will be extracted into a dataframe.
#' @parm npcs Number of principal components to return. Principal Component Analysis is performed if the number of features is greater than 50
#' @return A dataframe containing the assay data from the Seurat object. Rows correspond to different measurements,
#' columns to different features
#' @examples
#' ## Read in a Seurat object
#' seu <- readRDS("pbmc3k_final.rds")
#'
#' > seu
#' An object of class Seurat
#' 13714 features accross 2638 samples within 1 assay
#' Active assay: RNA (13714 features, 2000 variable features)
#'  2 dimensional reductions calculated: pca, umap
#'
#' ## Parse the object into a dataframe
#' seuM <- seurat_to_dataframe(seu)
#'
#' > dim(seuM)
#' [1] 2638 13714
#'
seurat_to_dataframe_pca <- function(obj, npcs) {
  s_obj <- Seurat::ScaleData(obj)
  # Run PCA on the seurat object, the result is stored in the reduction slot.
  ifelse(ncol(s_obj) > 50,
  {s_obj <- Seurat::RunPCA(s_obj, verbose = FALSE, npcs = npcs, features = Seurat::VariableFeatures(s_obj))
  # Return the cell embeddings with top 50 PCs
  return(s_obj@reductions$pca@cell.embeddings)}, {
    return(as.matrix(Seurat::GetAssayData(obj)))
  })
}

seurat_to_dataframe <- function(obj) {
  return(as.matrix(Seurat::GetAssayData(obj)))
}


