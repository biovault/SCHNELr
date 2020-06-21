## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- eval=FALSE--------------------------------------------------------------
#  data <- load_mnist_images("../../data/t10k-images.idx3-ubyte")

## ---- eval=FALSE--------------------------------------------------------------
#  clustered <- cluster(data)

## ---- eval = FALSE------------------------------------------------------------
#  Applying PCA
#  Initializing Hierarchical-SNE...
#  Number of data points:	10000
#  Initializing the first scale...
#  Computing the neighborhood graph...
#  	Building the trees...
#  	AKNN queries...
#  	FMC computation...
#  Creating transition matrix...
#  Min memory requirements (MB):	3.92914
#  Initialization complete!
#  Add a new scale with out-of-core implementation ...
#  Landmark selection...
#  Monte Carlo Approximation...
#  Selection...
#  	#landmarks:	2211
#  	Computing area of influence...
#  	Caching weights...
#  	Inverting the AoI matrix...
#  	Computing similarities...
#  	Computing finite markov chain...
#  
#  --------------- Hierarchical-SNE Statistics ------------------
#  Total time:	2.69811
#  	Markov Chain Monte Carlo sampling time:	1.63543
#  	Landmark selection time:		0.000137951
#  	Lndks Slct #walks:			2e+06
#  	Area of Influence computation time:	1.06185
#  	FMC computation time:			0.000547095
#  	AoI #walks:				2e+06
#  	Is sparsity (%):			97.1955
#  	Ts sparsity (%):			96.7932
#  	Ts effective sparsity (%):		96.7932
#  --------------------------------------------------------------

## ---- eval = FALSE------------------------------------------------------------
#  Number of scales 2
#  Start reading first scale of size 10000
#  Done reading first scale..
#  Next scale: 1
#  Scale size: 2211
#  Reading transmatrix...
#  Reading landmarks of scale to original data...
#  Reading landmarks to previous scale...
#  Reading landmark weights...
#  Reading previous scale to current scale...
#  Reading area of influence...
#  Total time spent parsing hierarchy and building objects: 1.0225149790446 seconds
#  Applying Leiden algorithm.
#  Warning: You are about to cluster the full dataset, this might take a very long time
#  Applying Leiden algorithm.

## ---- eval = FALSE------------------------------------------------------------
#  head(clustered)
#      [,1] [,2]
#  [1,]    0    4
#  [2,]    2   10
#  [3,]    8    1
#  [4,]    3    2
#  [5,]    7    9
#  [6,]    8    1

