

#' K-Medoids
#'
#' The standard K-Medoids algorithm. Tries fitting the data into K many
#' clusters centered around so called medoids, the data points closest to the
#' centroids of the resulting
#'
#' The resulting function can be used for new unknown data!
#'
#' @param data    a tibble with with every row representing a data point. The
#'   number columns is therefore the dimensionality, The number of rows is the
#'   sample size and called n.
#' @param K         A whole number between 0 and n+1. The amount of Clusters the
#'   algorithm tries to find in the data
#' @inheritParams getDistanceFunction
#' @param .print_info A logical. Prints some useful information for debugging.
#'
#' @returns A list of the class 'clustering'. Contains \itemize{
#'   \item{\strong{\code{'clustered_data'}}} a tibble of original data with a new column called \code{'cluster'}
#'   \item{\strong{\code{'clustering_function'}}} a function applicable to known and
#'   unknown data points. Returns the cluster the data point belongs to.
#'   \item{\strong{\code{'inner_inequality'}}} a numeric. The sum of all differences of the data points to their cluster medoids.
#'   }
#' @export
kMedoids <- function(data,
                      K,
                      distance_method = 'euclidean',
                      p=NULL,
                      custom_distance_function=NULL,
                      .print_info = FALSE){

  ## some necessary variables
  n <- base::nrow(data)
  dim <- base::ncol(data)
  old_min_cost <- Inf
  old_centroids <- tibble::tibble()

  ## Invariants: test the input
  base::stopifnot('Data must have more than 0 rows' = n>0)

  base::stopifnot('K is not a whole number' = is.wholenumber(K))
  base::stopifnot('K must be between 0 and n+1' = (0 < K && K < n+1))

  if(.print_info) print('calculating dissimilarity matrix')

  ## As we will never need to handle new data points, calculate all distances
  ## between the data points now.
  D <- dissimilarityMatrix(data,
                           distance_method = distance_method,
                           p = p,
                           custom_distance_function = custom_distance_function)



  if(.print_info) print('Done. \n now find starting medoids')

  ## (BUILD) Define starting medoids
  medoid_indeces <- greedySearchMedoidIndeces(data,K,dissimilarity_matrix=D)



  if(.print_info) print('Done. \n now calculating first costs')

  new_min_cost <-  structure(D[medoid_indeces,],dim=c(K,n)) |>
    apply(c(2),min) |>
    sum()


  # Special case: only 1 cluster. We want to allow it, to compare which cluster amount to choose
  if(K==1){
    return(structure(
      list(
        clustered_data = clustered_data(data, cluster=1),
        clustering_function = function(x) 1,
        centroids = data[medoid_indeces,],
        inner_inequality = new_min_cost,
        sum_of_squares = structure(D[medoid_indeces,]^2,dim=c(K,n)) |> apply(c(2),min) |> sum(),
        mean_silhouette = 0
      ),
      description = 'Data clustered by K-Means algorithm',
      class= c('K-Means-clustering',  'clustering')
    )
    )

  }

  while(new_min_cost < old_min_cost){
    ## We've found a new, better medoids configuration!
    if(.print_info)
      base::print(base::paste0('Found a new best clustering! The new best cost is ',new_min_cost))

    ## Save old medoids, to compare with next medoids
    old_medoid_indeces <- medoid_indeces
    old_min_cost <- new_min_cost

    ## calculate which changed medoids would diminsh the inner inequality most
    costs <- base::matrix(base::rep(1:n,length(medoid_indeces)),ncol = length(medoid_indeces))
    for(k in 1:length(medoid_indeces)){
      costs[,k] <- sapply(costs[,k],function(x) innerInequalityAfterChangingMedoid(x,medoid_indeces,k,dissimilarity_matrix=D))
    }

    ## Save which indeces of data point o and medoid m would have the lower cost
    m_opt <- arrayInd(which.min(costs), dim(costs))[2]
    o_opt <- arrayInd(which.min(costs), dim(costs))[1]

    if(.print_info)
      print(paste0('changing medoid ', m_opt, ' with data point ', o_opt))
    # change medoid

    medoid_indeces[m_opt] <- o_opt

    # new minimal
    new_min_cost <- costs[o_opt,m_opt]

  }

  distance <- getDistanceFunction(distance_method = distance_method,
                                  p = p,
                                  custom_distance_function = custom_distance_function)

  clustering_function <- function(x) 1:K |>
    sapply(function(k) distance(x,base::unlist(data[old_medoid_indeces[k],]))) |>
    base::which.min()

  best_clustered_data <- data |> dplyr::mutate(cluster = structure(D[old_medoid_indeces,],dim=c(K,n)) |> apply(c(2),which.min))


  ## returns clustered data and a function returning the cluster a datapoint (atomic vector) belongs to
  return(structure(
    list(
      clustered_data = clustered_data(best_clustered_data),
      clustering_function = clustering_function,
      medoids = data[old_medoid_indeces,],
      inner_inequality = old_min_cost,
      sum_of_squares = structure(D[old_medoid_indeces,]^2,dim=c(K,n)) |> apply(c(2),min) |> sum(),
      mean_silhouette = 1:n |> sapply(function(index) silhouette_faster(best_clustered_data,index,D)) |> mean()
    ),
    description = 'Data clustered by K-Medoids algorithm',
    class= c('K-Medoids-clustering',  'clustering')
  )
  )
}




#' Inner Inequality after changing Medoids
#'
#' Helper function for K-Medoids
#'
#' @param o         an index of a data point (of external data)
#'   to exchange with the medoid
#' @param medoid_indeces The indeces of the original medoids
#'   centroid/medoid.
#' @param m         an index of a data point to exchange with the medoid
#' @param dissimilarity_matrix    A matrix having encoded all distances between data points
#'
#' @returns the inner inequality, called cost, after the exchange of medoids
innerInequalityAfterChangingMedoid <- function(o,medoid_indeces,m,dissimilarity_matrix){

  # change medoid
  medoid_indeces[m] <- o

  # dont reward changes, which would result in a lower than promised cluster amount
  if(!base::identical(medoid_indeces,unique(medoid_indeces))){
    return(Inf)
  }

  # return the costs
  return(structure(dissimilarity_matrix[medoid_indeces,],dim=c(length(medoid_indeces),nrow(dissimilarity_matrix))) |> apply(c(2),min) |>  sum() )
}






#' Greedy search Medoids
#'
#' greedy search of medoids is part of PAM algorithm for finding a good enough
#' starting cluster constellation for the k-medoids algorithm
#'
#' @param data    a tibble with with every row representing a data point. The
#'   number columns is therefore the dimensionality, The number of rows is the
#'   sample size and called n.
#' @param K         A whole number between 0 and n+1. The amount of Clusters the
#'   algorithm tries to find in the data
#' @inheritParams getDistanceFunction
#' @param dissimilarity_matrix    A matrix having encoded all distances between data points
#'
#' @returns The indeces of good-enough clustering medoid in the \code{data}
greedySearchMedoidIndeces <- function(data,
                                       K,
                                       distance_method='euclidean',
                                       p=NULL,
                                       custom_distance_function=NULL,
                                       dissimilarity_matrix=NULL){


  ## Invariants: test the input
  base::stopifnot('Data must have more than 0 rows' = ncol(data)>0)
  base::stopifnot('K must be an integer larger than 0' = is.wholenumber(K) && (0 < K))
  base::stopifnot('data must have at least K unique data points' = K <= nrow(unique(data)))

  if(K == nrow(unique(data))) return(data |>
                                       dplyr::mutate('index' = row(data[1])) |>
                                       dplyr::summarise('index' = dplyr::first(.data$index)     , .by = all_of(1:ncol(data))) |>
                                       dplyr::select('index') |>
                                       unlist(use.names = FALSE))

  # if not already given, calculate dissimilarity matrix
  if(is.null(dissimilarity_matrix)) {
    M <- dissimilarityMatrix(data,
                             distance_method = distance_method,
                             p = p,
                             custom_distance_function = custom_distance_function)
    }
  else M <- dissimilarity_matrix

  dimnames(M) <- NULL

  # we choose the first medoid as the data point minimizing th sum of distances to all data points
  medoid_index <- M |> base::apply(c(1),sum) |> which.min()
  medoids <- data[medoid_index,]

  D <- M[medoid_index,-medoid_index]

  # ignore chosen medoid in fourther decisions,
  M <- M[-medoid_index,-medoid_index]

  medoid_indeces <- medoid_index

  if(K>1){
    for(k in 2:K){

      # For all data points calculate their minimum distance to the chosen medoid

      # Format to a matrix (for later)
      D <- D |> base::rep(base::nrow(data)-k+1) |>
        base::matrix(ncol = base::nrow(data)-k+1, byrow = TRUE)



      # score every data points j based on their distance to the existing medoids vs
      # all other i
      # BUT ignore all data point j whose delta is negative
      # aka the data points i that are closer to the existing medoids than the
      # data point j
      M_k <- apply(D-M, c(1,2), function(x) max(x,0))

      # we take the data point, which would be most advantageous to add to the medoids
      medoid_index <- M_k |> base::apply(c(1),sum) |> which.max()

      # calculate the minimum distance of a point to any already chosen medoid
      # join D and the distances of the newly chosen medoid
      D[2,] <- M[medoid_index,]
      # simplify D
      D <- D[c(1,2),-medoid_index]
      # calculate new minimum
      D <- apply(D,2,min)

      # remove this medoid from being chosen in the future
      M <- M[-medoid_index,-medoid_index]

      # account for the removed rows from M
      for(x in sort(medoid_indeces)) if(medoid_index >= x) medoid_index<-medoid_index+1

      # lastly: add the newly found medoid
      medoid_indeces <- c(medoid_indeces,medoid_index)
      medoids <- dplyr::add_row(medoids, data[medoid_index,])
    }
  }
  return(medoid_indeces)
}



## Example
## generate some test data
#data <- generateClusterTestDataSimple2D(n=50,5)
#
## lets view it
#viewClusters(data)
#
###############################################################
## Apply the K-Medoids algortithm                           ##
#clustering <- kMedoids(data,K=5)
## as K-medoids has O(n²) runtime this may take a while     ##
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
## We make a guess for K and apply the K-Medoids algortithm ##
#clustering <- K_medoids(data,K=4)
## this may take a while                                     ##
###############################################################
#
## is it any good?
#viewClusters(data,clustering)
#meanSilhouette(data,clustering)
#
###############################################################
## Try a higher K and apply the K-Medoids algortithm        ##
#clustering <- K_medoids(data,K=7)
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
## derive a clustering using K-medoids
#clustering <- K_medoids(training_data,4)
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
### greedy searched medoids
## To start of with an already good choice of cluster medians, this algorithm uses
## greedy search. This can also be used, to give a fast, but unoptimized clustering:
#
## some data
#data <- generateClusterTestDataSimple2D(n=100,5)
#
#
#####################################################
## Use greedy search, to find medoids fast        ##
#medoids <- greedySearchMedoidIndeces(data,K=5)
##                                                 ##
#####################################################
#
## Defer a clustering
#clustering <- clusteringFromCentroids(medoids)
#
## lets see the unoptimized clustering
#viewClusters(data,clustering)
#
#####################################################
## Lets see the real K-Medoids in action!         ##
#clustering <- K_medoids(data,K=5)
## go grab a cup of tea, this takes a few minutes  ##
#####################################################
#
## lets see the optimized clustering
#viewClusters(data,clustering)
#
#
#
