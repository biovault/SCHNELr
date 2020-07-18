## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- eval = F----------------------------------------------------------------
#  install.packages("devtools")

## ---- eval = F----------------------------------------------------------------
#  devtools::install_github(biovault/rschnel)

## ---- eval=FALSE--------------------------------------------------------------
#  if (!requireNamespace("BiocManager", quietly = TRUE))
#      install.packages("BiocManager")
#  BiocManager::install("flowCore")

## ---- eval=FALSE--------------------------------------------------------------
#  devtools::install_github("ParkerICI/grappolo")

## ---- eval=FALSE--------------------------------------------------------------
#  clusterSingleFile <- cluster(“../../data/fcs_data.fcs”)
#  clusterDirectory <- cluster(“../../data/fileFCS/”, dir = T)

## ---- eval = FALSE------------------------------------------------------------
#  install.packages("Seurat")

## ---- eval=FALSE--------------------------------------------------------------
#  # Extract the data from the Seurat object into a matrix
#  seurat_to_matrix <- parse_to_matrix("../../data/seuratObj.rds")
#  
#  # Perform clustering on a seurat object
#  cluster_seurat <- cluster("../../data/seuratObj.rds")

## ---- eval=FALSE--------------------------------------------------------------
#  devtools::install_github("mojaveazure/seurat-disk")
#  devtools::install_github('satijalab/seurat-data')

## ---- eval = FALSE------------------------------------------------------------
#  if (!requireNamespace("BiocManager", quietly = TRUE))
#      install.packages("BiocManager")
#  
#  BiocManager::install("SingleCellExperiment")

