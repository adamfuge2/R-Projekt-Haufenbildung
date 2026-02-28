

#' K-Medioids
#'
#' The standard K-Medioids algorithm. Tries fitting the data into K many
#' clusters centered around so called medioids, the data points closest to the
#' centroids of the resulting
#'
#' The resulting function can be used for new unknown data!
#'
#' @param data    a tibble with with every row representing a data point. The
#'   number columns is therefore the dimensionality, The number of rows is the
#'   sample size and called n.
#' @param K         A whole number between 0 and n+1. The amount of Clusters the
#'   algorithm tries to find in the data
#' @param metric    A metric whose inputs are the rows of data.
#'
#' @returns a clustering function,  a function relating every data point to
#'   their cluster. Can be used on new data! input: atomic vectors Of the data
#'   row type returns: a number 1 to k, representing the related cluster.
kMedioids <- function(data,K,metric = euclidean){

  ## some necessary variables
  n <- base::nrow(data)
  dim <- base::ncol(data)
  old_min_cost <- Inf
  old_centroids <- tibble::tibble()


  ## Invariants: test the input
  base::stopifnot('Data must have more than 0 rows' = n>0)
  base::stopifnot('K is not a whole number' = is.wholenumber(K))
  base::stopifnot('K must be between 0 and n+1' = (0 < K && K < n+1))

  D <- dissimilarityMatrix(data,metric = metric)

  ## (BUILD) Define starting centroids
  medioid_indeces <- greedySearchMedioidIndeces(data,K,metric,dissimilarity_matrix=D)



  new_min_cost <-  D[medioid_indeces,] |> apply(c(2),min) |>  sum()

  while(new_min_cost < old_min_cost){
    ## Weve found a new, better medioids configuration!
    if(.print_info)
      base::print(base::paste0('Found a new best clustering! The new best cost is ',new_min_cost))

    ## Save old centroids, to compare with next centroids
    old_medioid_indeces <- medioid_indeces
    old_min_cost <- new_min_cost

    ## calculate which changed medioids would diminsh the inner inequality most


    costs <- base::matrix(base::rep(1:n,base::nrow(centroids)),ncol = base::nrow(centroids))
    for(k in 1:base::nrow(centroids)){
      costs[,k] <- sapply(costs[,k],function(x) innerInequalityAfterChangingMedioid(x,medioid_indeces,k,dissimilarity_matrix=D))
    }

    ## Save which indeces of data point o and medioid m would have the lower cost
    m_opt <- arrayInd(which.min(costs), dim(costs))[2]
    o_opt <- arrayInd(which.min(costs), dim(costs))[1]


    # change medioid
    medioid_indeces[m_opt] <- o_opt

    # new minimal
    new_min_cost <- costs[o_opt,m_opt]
  }

  clustering_function <- function(x) 1:K |>
    sapply(function(k) metric(x,base::unlist(data[medioid_indeces(k),]))) |>
    base::which.min()

  best_clustered_data <- data |> dplyr::mutate(cluster = D[medioid_indeces,] |> apply(c(2),which.min))


  ## returns clustered data and a function returning the cluster a datapoint (atomic vector) belongs to
  return(structure(
    list(
      clustered_data = best_clustered_data,
      clustering_function = clustering_function,
      inner_innequality = new_min_cost),
    description = 'Data clustered by K-Medioids algorithm',
    class= 'clustering'
  )
  )
}




#' Inner Inequality after changing Medioids
#'
#' Helper function for K-Medioids
#'
#' @param o         an index of a data point (of external data)
#'   to exchange with the medioid
#' @param medioid_indeces The indeces of the original medioids
#'   centroid/medioid.
#' @param m         an index of a data point to exchange with the medioid
#' @param dissimilarity_matrix    A matrix having encoded all distances between data points
#'
#' @returns the inner inequality, called cost, after the exchange of medioids
innerInequalityAfterChangingMedioid <- function(o,medioid_indeces,m,dissimilarity_matrix){

  # change medioid
  medioid_indeces[m] <- o

  # dont reward changes, which would result in a lower than promised cluster amount
  if(!base::identical(medioid_indeces,unique(medioid_indeces))){
    return(Inf)
  }

  # return the costs
  return(dissimilarity_matrix[medioid_indeces,] |> apply(c(2),min) |>  sum() )
}






#' Greedy search Medioids
#'
#' greedy search of medioids is part of PAM algorithm for finding a good enough
#' starting cluster constellation for the k-medioids algorithm
#'
#' @param data    a tibble with with every row representing a data point. The
#'   number columns is therefore the dimensionality, The number of rows is the
#'   sample size and called n.
#' @param K         A whole number between 0 and n+1. The amount of Clusters the
#'   algorithm tries to find in the data
#' @param metric    A metric whose inputs are the rows of data.
#' @param dissimilarity_matrix    A matrix having encoded all distances between data points
#'
#' @returns The indeces of good-enough clustering medioid in the \code{data}
greedySearchMedioidIndeces <- function(data,K,metric=euclidean,dissimilarity_matrix=NULL){

  # Invariants
  base::stopifnot('K is not a whole number' = is.wholenumber(K))
  base::stopifnot('K must positive' = (0 < K))

  # if not already given, calculate dissimilarity matrix
  if(is.null(dissimilarity_matrix)) M <- dissimilarityMatrix(data,metric)
  else M <- dissimilarity_matrix

  # we choose the first medioid as the data point minimizing th sum of distances to all data points
  medioid_index <- M |> base::apply(c(1),sum) |> which.min()
  medioids <- data[medioid_index,]

  # ignore chosen medioid in fourther decisions
  M <- M[-medioid_index,-medioid_index]

  medioid_indeces <- medioid_index

  if(K>1){
    for(k in 2:K){

      # For all data points calculate their minimum distance to the chosen medioids

      D <- sapply(1:base::nrow(data),function(o) min(sapply(1:(k-1), function(m) metric(data[o,],medioids[m,]))))

      # Remove the already chosen medioids and format to a matrix (for later)
      D <- D[D != 0] |> base::rep(base::nrow(data)-k+1) |>
        base::matrix(ncol = base::nrow(data)-k+1, byrow = TRUE)



      # score every data points j based on their distance to the existing medioids vs
      # all other i
      # BUT ignore all data point j whose delta is negative
      # aka the data points i that are closer to the existing medioids than the
      # data point j
      M_k <- apply(D-M, c(1,2), function(x) max(x,0))

      # we take the data point, which would be most advantageous to add to the medioids
      medioid_index <- M_k |> base::apply(c(1),sum) |> which.max()

      # remove this medioid from being chosen in the future
      M <- M[-medioid_index,-medioid_index]

      # account for the removed rows from M
      for(x in medioid_indeces) if(medioid_index >= x){medioid_index<-medioid_index+1}

      # lastly: add the newly found medioid
      medioid_indeces <- c(medioid_indeces,medioid_index)
      medioids <- dplyr::add_row(medioids, data[medioid_index,])
    }
  }
  return(medioid_indeces)
}



## Example
## generate some test data
#data <- generateClusterTestDataSimple2D(n=50,5)
#
## lets view it
#viewClusters(data)
#
###############################################################
## Apply the K-Medioids algortithm                           ##
#clustering <- kMedioids(data,K=5)
## as K-medioids has O(n²) runtime this may take a while     ##
###############################################################
#
## is this any good? We can calculate the inner inequalty of this clustering
## here, lower is better
#innerInequality(data,clustering)
#
#
## or we can find the silhouette coefficient of the clustering
## the closer this is to 1, the better
#meanSilhouette(data,clustering)
#
#
#viewClusters(clustering$clustered_data)
#
#
#
#
#
### We can also apply this to data, whose number of clusters is unknown
#data <- generateClusterTestDataSimple2D(n=100, n_clusters=base::floor(stats::runif(1,1,10)))
#
## this is what it looks like
#viewClusters(data)
#
###############################################################
## We make a guess for K and apply the K-Medioids algortithm ##
#clustering <- K_medioids(data,K=4)
## this may take a while                                     ##
###############################################################
#
## is it any good?
#viewClusters(data,clustering)
#meanSilhouette(data,clustering)
#
###############################################################
## Try a higher K and apply the K-Medioids algortithm        ##
#clustering <- K_medioids(data,K=7)
## this may take a while                                     ##
###############################################################
#
## By comparing the mean Silhouette one can defer the number of clusters
#viewClusters(data,clustering)
#meanSilhouette(data,clustering)
#
#
#
#
#
#
### New Data
## The resulting function does take inputs not of the original data set
## note that the amount of unkown data is vastly greater than the training data
#clusters <- list(tibble::tibble(X=0.05,Y=0.05),
#                 tibble::tibble(X=0.03,Y=0.02),
#                 tibble::tibble(X=0.03,Y=0.08),
#                 tibble::tibble(X=0.07,Y=0.04))
#training_data <- generateClusterTestData2DFromPaths(n=50, clusters)
#unknown_data <- generateClusterTestData2DFromPaths(n=1000, clusters)
#
## derive a clustering using K-medioids
#clustering <- K_medioids(training_data,4)
## this would take ages for 1000 data points
#
## lets take a look
#viewClusters(training_data,clustering)
#viewClusters(unknown_data,clustering)
#
#
#
#
#
#
#
#
#
#
#
### greedy searched medioids
## To start of with an already good choice of cluster medians, this algorithm uses
## greedy search. This can also be used, to give a fast, but unoptimized clustering:
#
## some data
#data <- generateClusterTestDataSimple2D(n=100,5)
#
#
#####################################################
## Use greedy search, to find medioids fast        ##
#medioids <- greedySearchMedioidIndeces(data,K=5)
##                                                 ##
#####################################################
#
## Defer a clustering
#clustering <- clusteringFromCentroids(medioids)
#
## lets see the unoptimized clustering
#viewClusters(data,clustering)
#
#####################################################
## Lets see the real K-Medioids in action!         ##
#clustering <- K_medioids(data,K=5)
## go grab a cup of tea, this takes a few minutes  ##
#####################################################
#
## lets see the optimized clustering
#viewClusters(data,clustering)
#
#
#
