# spectral clustering

#' Gauss Kernel Weights
#'
#' Function to determine the weights using euclidean metric. (shortcut)
#'
#' @param data a tibble or atomic vector with n rows representing d-dimensional
#' points
#' @param gamma reduction factor for Kernel (large value means large reduction of
#' far distances)
#'
#' @returns n x n matrix with values between 0 and 1
#' @export
gaussKernelWeights <- function(data,gamma){
  base::exp(- gamma * base::as.matrix(stats::dist(data)))
}


#' Gauss Kernel
#'
#' Kernel function using the euclidean norm
#'
#' @param x tibble with one row or atomic vector
#' @param y tibble with one row or atomic vector
#'
#' @returns numeric bewteen 0 and 1
#' @export
gaussKernel <- function(x,y,gamma){
  base::exp(- gamma * sqrt(sum((x-y)^2)))
}


#' Kernel Generator (Custom Metric)
#'
#' Function operator generating a kernel with a custom metric
#'
#' @param metric a metric function taking two arguments
#' @param gamma reduction factor for Kernel (large value means large reduction of
#' far distances)
#'
#' @returns kernel function with this custom metric
#' @export
kernelByCustomMetric <- function(metric,gamma)
  function(x,y) base::exp(- gamma * metric(x,y))


#' Spectral projection
#'
#' Algorithm to project a dataset to k dimensional space to use for spectral clustering.
#'
#' @param data a tibble or atomic vector with n rows representing d-dimensional
#' points
#' @param k output dimension
#' @param mercer_kernel kernel function (see algorithm description)
#' @param gamma projection factor for the kernel function
#' @param custom_mercer_kernel kernel function (see algorithm description)
#' @param metric metric used in kernel function
#' @param p if metric is 'Lp', this value will be used for p
#' @param custom_metric a custom metric used in kernel function
#' @param kernel_epsilon maximal distance at which data points are still considered neighbored.
#'
#' @returns tibble with eigenvectors of projected dimension k
#'
#' @export
spectralProjection <- function(data,
                              k,
                              mercer_kernel = 'gauss',
                              gamma = NULL,
                              custom_mercer_kernel = NULL,
                              metric = NULL,
                              p = NULL,
                              custom_metric = NULL,
                              kernel_epsilon = Inf,
                              .print_info = FALSE){
  n <- nrow(data)

  if(.print_info) print(all(c(is.null(custom_mercer_kernel),
                              is.null(metric),
                              is.null(custom_metric),
                              kernel_epsilon==Inf)))
  if(.print_info) print(c(is.null(custom_mercer_kernel),
                              is.null(metric),
                              is.null(custom_metric),
                              kernel_epsilon==Inf))

  if(all(c(is.null(custom_mercer_kernel),
           is.null(metric),
           is.null(custom_metric),
           kernel_epsilon==Inf))){
    if(mercer_kernel == 'gauss') {
      stopifnot('Please provide a non negative value for gamma'= !is.null(gamma) && gamma >= 0)
      W <- gaussKernelWeights(data,gamma)
      K <- gaussKernel
    }
    else stop('Unknown mercer kernel and no custom mercer kernel provided')
  }
  else {
    if(!is.null(custom_mercer_kernel)){
      if(any(c(!is.null(metric),
               !is.null(custom_metric),
               !is.null(kernel_epsilon))))
        warning('Custom Kernel provided, other parameters metric, custom_metric or kernel_epsilon discarded')
      W <- dissimilarityMatrix(data,custom_mercer_kernel)
      K <- custom_mercer_kernel
    }else{
      almost_metric <- getDistanceFunction(metric,p,custom_metric)
      if(kernel_epsilon < Inf) {
        metric <- function(x,y) almost_metric(x,y)* (kernel_epsilon >= almost_metric(x,y))
      }else metric <- almost_metric

      almost_kernel <- kernelByCustomMetric(metric,gamma)
      if(kernel_epsilon < Inf) {
        K <- function(x,y) almost_kernel(x,y)* (kernel_epsilon >= almost_metric(x,y))
      }else K <- almost_kernel

      W <- dissimilarityMatrix(data,K)
    }
  }


  D <- apply(W,1,sum) |> diag()

  L <- D - W

  D_sqrt <- 1/sqrt(D)    #D^(-1/2)
  D_sqrt[D_sqrt == Inf] <- 0

  # calculate eigenvectors from smallest to biggest
  b <- eigen(D_sqrt %*% L %*% D_sqrt, symmetric=TRUE)$vectors[,n:1]

  # transform to be solutions of our optimization problem
  beta <- apply(b,c(2),function(x) n^(-1/2)*D_sqrt %*% x)

  # retrieve the new transformed data points (columns of a).
  # The eigenvector belonging to the lowest eigenvalue gets left out, as it is
  # linearly dependent to (1,1, ... ,1) and thus bears no information.
  a <- t(beta)[2:(k+1), ]


  # prevent conversion to atomic vector, if k = 1
  dim(a) <- c(k,n)

  # save the data points in tibble as usual
  projected_data <- tibble::as_tibble(t(a),.name_repair = 'minimal')

  # useful names
  colnames(projected_data) <- paste0('X_',1:k)

  # To calculate the projection function. but wtf no clue
  #
  #volume <- sum(D) - n
  #
  #K_hat <- function(x,y) K(x,y) - sum(apply(1:n,function(i) K(data[i,],x)))*sum(apply(1:n,function(i) K(y,data[i,])))*1/volume
  #
  #lambda <- eigen(D_sqrt %*% L %*% D_sqrt, symmetric=TRUE)
  #
  #projection_function <- function(x)

  return(list(projected_data = projected_data, projection_function = function(...) stop('not implemented yet')))

}


#' Spectral Clustering
#'
#' Algorithm to project a data set to k dimensional space and then cluster it.
#' @inheritParams spectralProjection
#' @param cluster_algorithm A cluster algorithm to be used on the spectral
#'   projected data. One of: \code{'K-Means'}, \code{'K-Medioids'},
#'   \code{'hierarchical Clustering'}, \code{'DBSCAN'}, \code{'OPTICS'}
#' @param ... Further parameters to pass on to the clustering algorithm.
#'
#' @returns a clustering object
#'
#' @export
spectralClustering <- function(data,
                               k,
                               mercer_kernel = 'gauss',
                               gamma = NULL,
                               custom_mercer_kernel = NULL,
                               metric = NULL,
                               p = NULL,
                               custom_metric = NULL,
                               kernel_epsilon = Inf,
                               cluster_algorithm = 'K-Means',
                               ...,
                               .print_info = FALSE){

  if(.print_info) print('Now: calculating spectral projection')

  # apply the spectral projection to the data
  spectral_projection <- spectralProjection(data=data,
                                          k=k,
                                          mercer_kernel = mercer_kernel,
                                          gamma=gamma,
                                          custom_mercer_kernel = custom_mercer_kernel,
                                          metric = metric,
                                          p = p,
                                          custom_metric = custom_metric,
                                          kernel_epsilon = kernel_epsilon)


  if(.print_info) print('Success, Now: calculating clustering on projected data')

  # Based on the cluster algorithm chosen: Apply it and modify the Output
  if(cluster_algorithm == 'K-Means'){
    # apply kMeans
    clustering <- kMeans(spectral_projection$projected_data, ... )

    # we would like to be able to access the projected data
    clustering$projected_clustered_data <- clustering$clustered_data
    # but were interested in the clustering of the original data points
    clustering$clustered_data <- data |> tibble::add_column(cluster = clustering$clustered_data$cluster)

  }
  else if(cluster_algorithm == 'K-Medioids'){
    # apply kMeadioids
    clustering <- kMedioids(spectral_projection$projected_data, ...)

    # we would like to be able to access the projected data
    clustering$projected_clustered_data <- clustering$clustered_data
    # but were interested in the clustering of the original data points
    clustering$clustered_data <- data |> tibble::add_column(cluster = clustering$clustered_data$cluster)

  }
  else if(cluster_algorithm == 'hierarchical Clustering'){
    # apply hierarchical_clustering
    clustering <- hierarchical_clustering(spectral_projection$projected_data, ...)

    # TODO Snep: Modify the output to your liking (see for example kMeans case)
  }
  else if(cluster_algorithm == 'DBSCAN'){
    # apply DBSCAN
    clustering <- DBSCAN(spectral_projection$projected_data, ...)

    # TODO Aaaaaron: Modify the output to your liking (see for example kMeans case)
  }
  else if(cluster_algorithm == 'OPTICS'){
    # apply OPTICS
    clustering <- OPTICS(spectral_projection$projected_data, ...)

    # TODO Aaaaaron: Modify the output to your liking (see for example kMeans case)
  }
  else{
    stop('Unknown clustering algorithm. Try one of \'kMeans\', \'kMedioids\', \'hierarchical Clustering\', \'DBSCAN\' or \'OPTICS\'. ')
  }

  if(.print_info) print('Success, All done!')

  return(clustering)
}

#data <- generateClusterTestDataSimple(n=100,dim=3,cluster_amount = 3)
#
#viewClusters(data)
#viewClusters(spectralprojection(data,k=1,gamma=1)$projected_data)
#viewClusters(projected_data)

# spectral_clustering <- spectralClustering()

#spectral_projection <- spectralProjection(connected_circles_data,k=1,gamma=10,.print_info = TRUE)
#spectral_clustering <- spectralClustering(connected_circles_data,k=3,gamma=50,cluster_algorithm = 'K-Means',K=2)
#spectral_clustering

#clustering <- spectralClustering(study_courses_data,k=2,gamma=1,custom_metric=studies_difference,K=6)


