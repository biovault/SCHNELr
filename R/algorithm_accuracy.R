num_rep <<- 20

ari_accuracy_1 <<- vector(length = 1)
ari_accuracy_2 <<- vector(length = 1)
ari_accuracy_3 <<- vector(length = 1)

ari_accuracy_1 <<- ari_accuracy_1[-1]
ari_accuracy_2 <<- ari_accuracy_2[-1]
ari_accuracy_3 <<- ari_accuracy_3[-1]

scale_1_result <<- matrix(ncol = 1, nrow = 60000)
scale_2_result <<- matrix(ncol = 1, nrow = 60000)
scale_3_result <<- matrix(ncol = 1, nrow = 60000)

get_accuracy <- function() {
  imgs <- load_mnist_images("../../data/train-images.idx3-ubyte")
  mnist_result <- load_mnist_labels("../../data/train-labels.idx1-ubyte")

  scale_results <- matrix(ncol = 1, nrow = 60000)

  for (i in c(1:num_rep)) { ## HERE
    message(paste("YOU ARE AT REPETITION NUMBER:", i))
    our_result <- cluster(imgs, transformation = "arcsinh")

    scale_1_result <<- cbind(scale_1_result, our_result[, 1])
    scale_2_result <<- cbind(scale_2_result, our_result[, 2])
    scale_3_result <<- cbind(scale_3_result, our_result[, 3])

    #scale_1_result[,i] <- our_result[,1]
    #scale_2_result[,i] <- our_result[,2]
    #scale_3_result[,i] <- our_result[,3]

    scale_results <- cbind(scale_results, scale_1_result)
    scale_results <- cbind(scale_results, scale_2_result)
    scale_results <- cbind(scale_results, scale_3_result)

    ari_accuracy_1 <<- append(ari_accuracy_1, ari_metric(our_result[,1], mnist_result))
    ari_accuracy_2 <<- append(ari_accuracy_2, ari_metric(our_result[,2], mnist_result))
    ari_accuracy_3 <<- append(ari_accuracy_3, ari_metric(our_result[,3], mnist_result))

    #ari_accuracy_1[i] <- ari_metric(our_result[,1], mnist_result)
    #ari_accuracy_2[i] <- ari_metric(our_result[,2], mnist_result)
    #ari_accuracy_3[i] <- ari_metric(our_result[,3], mnist_result)
  }

  scale_1_result <<- scale_1_result[,-1]
  scale_2_result <<- scale_2_result[,-1]
  scale_3_result <<- scale_3_result[,-1]

  scale_results <- scale_results[, -1]

  message(paste("ARI accuracy for scale 1:", sum(ari_accuracy_1)/num_rep))
  message(paste("ARI accuracy for scale 2:", sum(ari_accuracy_2)/num_rep)) ## HERE
  message(paste("ARI accuracy for scale 3:", sum(ari_accuracy_3)/num_rep))

  #scale_1_result <- scale_1_result[,-1]
  #scale_2_result <- scale_2_result[,-1]
  #scale_3_result <- scale_3_result[,-1]

  return(scale_results)
  #return(c(scale_1_result, scale_2_result, scale_3_result))
}

ari_metric <- function(our_result, mnist_result) {
  return(pdfCluster::adj.rand.index(our_result, mnist_result))
}

#hs_metric <- function(our_result, mnist_result) {
#  MVT::homogeneity.test()
#}

#cs_metric <- function(our_result, mnist_result) {
#  optiSel::completeness()
#}
