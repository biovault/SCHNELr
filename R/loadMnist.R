#' Load the MNIST images dataset
#'
#' Parse MNIST images from .idx-ubyte format into a NumericMatrix.
#' @param file_path Location of the file with the image samples
#' @return NumericMatrix representation of the dataset
#' @example load_image_file("../../../data/t10k-images.idx3-ubyte")
#' @export
load_mnist_images <- function(file_path) {
  f = file(file_path,'rb')
  readBin(f,'integer',n=1,size=4,endian='big')
  n = readBin(f,'integer',n=1,size=4,endian='big')
  nrow = readBin(f,'integer',n=1,size=4,endian='big')
  ncol = readBin(f,'integer',n=1,size=4,endian='big')
  x = readBin(f,'integer',n=n*nrow*ncol,size=1,signed=F)
  x = matrix(x, ncol=nrow*ncol, byrow=T)
  close(f)
  x
}

#' Load MNIST labels dataset
#'
#' Parse MNIST labels from .idx-ubyte format into a NumericMatrix.
#' @param file_path Location of the file with the image labels.
#' @return A NumericMatrix with one column
#' @example load_label_file("../../../data/train-labels.idx1-ubyte")
#' @export
load_mnist_labels <- function(file_path) {
  f = file(file_path,'rb')
  readBin(f,'integer',n=1,size=4,endian='big')
  n = readBin(f,'integer',n=1,size=4,endian='big')
  y = readBin(f,'integer',n=n,size=1,signed=F)
  y = matrix(y, ncol = 1, byrow = T)
  close(f)
  y
}
