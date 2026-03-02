# spectral clustering


gaussKernelWeights <- function(data,gamma){
  base::exp(- gamma * base::as.matrix(stats::dist(data)))
}
gaussKernel <- function(x,y,gamma){
  base::exp(- gamma * sqrt(sum((x-y)^2)))
}
gaussKernelByCustomMetric <- function(metric,gamma)
  function(x,y) base::exp(- gamma * metric(x,y))


#' Spectral Reduction
#'
#' Algorithm to decrease the dimension of a dataset to use for spectral clustering.
#'
#' @param data a tibble or atomic vector with n rows representing d-dimensional
#' points
#' @param gamma reduction faktor for the kernel function
#' @param k dimensional reduction (output will be of dimension k)
#' @param kernel kernel function (see algorithm description)
#' @param metric metric used in kernel function
#' @param epsilon minimal distance for reduction
#'
#' @returns tibble with eigenvectors of reduced dimension k
#'
#' @export
spectralReduction <- function(data,
                              k,
                              mercian_kernel = 'gauss',
                              gamma = NULL,
                              custom_mercian_kernel = NULL,
                              metric = NULL,
                              p = NULL,
                              custom_metric = NULL,
                              epsilon = Inf){
  n <- nrow(data)


  if(all(c(is.null(custom_mercian_kernel),
           is.null(metric),
           is.null(custom_metric),
           is.null(epsilon)))){
    if(mercian_kernel == 'gauss') {
      stopifnot('Please provide a non negative value for gamma'= !is.null(gamma) && gamma >= 0)
      W <- gaussKernelWeights(data,gamma)
      K <- gaussKernel
    }
    else stop('Unknown mercian kernel and no custom mercian kernel provided')
  }
  else {
    if(!is.null(custom_mercian_kernel)){
      if(any(c(!is.null(metric),
               !is.null(custom_metric),
               !is.null(epsilon))))
        warning('Custom Kernel provided, other parameters metric, custom_metric or epsilon discarded')
      W <- dissimilarityMatrix(data,custom_mercian_kernel)
      K <- custom_mercian_kernel}
    else{
      almost_metric <- getDistanceFunction(metric,p,custom_metric,epsilon)
      if(epsilon < Inf) metric <- function(x,y) almost_metric(x,y)* (epsilon >= almost_metric(x,y))
      else metric <- almost_metric

      almost_kernel <- gaussKernelByCustomMetric(metric,gamma)
      if(epsilon < Inf) K <- function(x,y) almost_kernel(x,y)* (epsilon >= almost_metric(x,y))
      else K <- almost_kernel

      W <- dissimilarityMatrix(data,K)
    }
  }


  D <- apply(W,1,sum) |> diag()

  L <- D - W

  D_sqrt <- 1/sqrt(D)    #D^(-1/2)
  D_sqrt[D_sqrt == Inf] <- 0

  # calculate eigenvectors
  b <- eigen(D_sqrt %*% L %*% D_sqrt, symmetric=TRUE)$vectors[,n:1]

  # transform to be solutions of our optiization problem
  beta <- apply(b,c(2),function(x) n^(-1/2)*D_sqrt %*% x)

  # retrieve the new transformed data points (columns of a).
  # The eigenvector belonging to the lowest eigenvalue gets left out, as it is
  # linearly dependent to (1,1, ... ,1) and thus bears no information.
  a <- t(beta)[2:(k+1), ]


  # prevent conversion to atomic vector, if k = 1
  dim(a) <- c(k,n)

  # save the data points in tibble as usual
  reduced_data <- tibble::as_tibble(t(a))

  # useful names
  colnames(reduced_data) <- paste0('X_',1:k)

  # To calculate the projection function. but wtf no clue
  #
  #volume <- sum(D) - n
  #
  #K_hat <- function(x,y) K(x,y) - sum(apply(1:n,function(i) K(data[i,],x)))*sum(apply(1:n,function(i) K(y,data[i,])))*1/volume
  #
  #lambda <- eigen(D_sqrt %*% L %*% D_sqrt, symmetric=TRUE)
  #
  #projection_function <- function(x)

  return(list(reduced_data = reduced_data, projection_function = function(...) stop('not implemented yet')))

}


spectralClustering <- function(data,
                               k,
                               mercian_kernel = 'gauss',
                               gamma = NULL,
                               custom_mercian_kernel = NULL,
                               metric = NULL,
                               p = NULL,
                               custom_metric = NULL,
                               epsilon = NULL,
                               cluster_algorithm = 'K-Means',
                               ...){

  spectral_reduction <- spectralReduction(data=data,
                                          k=k,
                                          mercian_kernel = mercian_kernel,
                                          gamma=gamma,
                                          custom_mercian_kernel = custom_mercian_kernel,
                                          metric = metric,
                                          p = p,
                                          custom_metric = custom_metric,
                                          epsilon = epsilon)




  if(cluster_algorithm == 'K-Means'){
    clustering <- kMeans(spectral_reduction$reduced_data, ... )
  }
  else if(cluster_algorithm == 'K-Medioids'){
    clustering <- kMeans(spectral_reduction$reduced_data, ...)
    # do this when implemented
    #clustered_data <- data |> tibble::add_column(cluster = clustering$clustered_data$cluster)

  }
  else if(cluster_algorithm == 'hierachicalClustering'){
    clustering <- hierarchical_clustering(spectral_reduction$reduced_data, ...)
  }
  else if(cluster_algorithm == 'DBSCAN'){
    clustering <- DBSCAN(spectral_reduction$reduced_data, ...)
  }
  else if(cluster_algorithm == 'OPTICS'){
    clustering <- OPTICS(spectral_reduction$reduced_data, ...)
  }
  else{
    stop('Unknown clustering algorithm')
  }

  return(clustering)
}

data <- generateClusterTestDataSimple(n=100,dim=3,cluster_amount = 3)

viewClusters(data)
viewClusters(spectralReduction(data,k=1,gamma=1)$reduced_data)
viewClusters(reduced_data)


