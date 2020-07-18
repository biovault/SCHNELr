#' SCHNEL clustering
#'
#' Function that runs the SCHNEL algorithm on the specified file(s) and returns a matrix/matrices which rows correspond to datapoints provided
#' for clustering; columns correspond to scales on which the clustering was performed. The value of a cell represents the cluster to which a given
#' point belongs to.
#'
#' @param file_path Location of the file(s) to read in.
#' @param dir Boolean specifying if the file_path is a directory
#' @param sce Boolean specifying whether the input file is a SingleCellExperiment object
#' @param n_scales Number of scales in the HSNE hierarchy.
#' @param n_neighbors Number of neighbors for kNN graph clustering performed during HSNE hierarchy creation.
#' @param transformation Transformation function to be performed on data prior to clustering.
#' @param cofactor Optional. In case of an arcsinh transformation on data, the value by which the value of a measurement will be divided by.
#' @param seeds Seed parameter for the HSNE dimensionality reduction.
#' @param landmark_treshold Minimum value of the connectivity score at which points at a particular scales will be selected as future landmarks.
#' @param num_trees Number of trees parameter for the HSNE dimensionality reduction
#' @param num_checks Number of checks parameter for the HSNE dimensionality reduction
#' @param trans_matrix_prune_threshold The beta threshold
#' @param num_walks Number of random walks performed on transition matrices during the creation of the HSNE heirarchy creation.
#' @param num_walks_per_landmark Number of random walks performed from every single landmark during the HSNE hierarchy creation.
#' @param monte_carlo_sampling Boolean specifying whether to perform Monte Carlo Method during the HSNE hierarchy creation.
#' @param out_of_core_computation Boolean for the out_of_core parameter for the HSNE dimensionality reduction
#' @param prop_method Default: cluster; other option: label
#' @examples
#' cluster_single_fcs <- cluster("../data/fcs_file.fcs", transformation = "log")
#' cluster_multiple_files <- cluster("../data/folder_name", dir = T)
#' @export
cluster <- function(
  file_path,
  dir = F,
  sce = F,
  p_comps = NULL,
  n_scales = 0,
  n_neighbors = 50,
  transformation = "NULL",
  cofactor = 5,
  seeds = -1,
  landmark_treshold = 1.5,
  num_trees = 6,
  num_checks = 1024,
  trans_matrix_prune_treshold = 1.5,
  num_walks = 200,
  num_walks_per_landmark = 200,
  monte_carlo_sampling = TRUE,
  out_of_core_computation = TRUE,
  prop_method = "cluster") {

  # If a directory is loaded, the result of parsing the data will be a list with the matrix on the first position
  # and a vector with the numbers of elements in each file
  if (dir) {
    pars_res <- parse_multiple_to_matrix(file_path, transformation = transformation, cofactor = cofactor)
    data_matrix <- pars_res[[1]]
    matrices_info <- pars_res[[2]]
  } else {
    data_matrix <- parse_to_matrix(file_path, transformation = transformation, cofactor = cofactor, sce = sce)
  }

  # Determine the number of scales of the HSNE hierarchy
  check_num <- round(log10(nrow(data_matrix) / 100))
  if (n_scales < 2 && check_num > 1)
    n_scales <- check_num
  else if (n_scales < 2 && check_num < 2) {
    warning("The specified number of scales is below 2. The default value (2) will be used")
    n_scales <- 2
  }

  # Create the HSNE hierarchy
  matrix_to_hsne <- run(data_matrix, n_scales, seeds, landmark_treshold,
                        n_neighbors, num_trees, num_checks, trans_matrix_prune_treshold,
                        num_walks, num_walks_per_landmark, monte_carlo_sampling, out_of_core_computation)

  # Read in the hsne hierarchy
  #message("should not stop here")

  # Initialize a matrix to store the result of the algorithm
  final_matrix <- matrix(nrow = nrow(data_matrix))
  # Get rid of the empty column
  final_matrix <- final_matrix[,-1]

  #message("also should not stop here")

  # Create a list with subscale numbers
  num_scales <- get_num_scales("test.hsne")

  #message("if it stops here its python importing")

  #n_scales <- c(2:hier$num_scales)
  n_scales <- c(1:num_scales - 1)

  # Cluster all the subscales
  lapply(n_scales, function(x) {
    #final_matrix <<- cbind2(final_matrix,  as.matrix(hier$cluster_scale(x)))
    final_matrix <<- cbind2(final_matrix,  as.matrix(parse_and_cluster("test.hsne", x, prop_method)))
    #message("lapply working")
  })

  #message("if it stops here its the size kinda shits idk")

  # Create the output form
  if (dir) {
    return(split_matrix(final_matrix, matrices_info))
  }
  #message("you are here.")
  return(final_matrix)

}
