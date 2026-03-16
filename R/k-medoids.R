

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
#' @references https://en.wikipedia.org/wiki/K-medoids
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
  medoid_Indices <- greedySearchMedoidIndeces(data,K,dissimilarity_matrix=D)



  if(.print_info) print('Done. \n now calculating first costs')

  new_min_cost <-  structure(D[medoid_Indices,],dim=c(K,n)) |>
    apply(c(2),min) |>
    sum()


  # Special case: only 1 cluster. We want to allow it, to compare which cluster amount to choose
  if(K==1){
    return(structure(
      list(
        clustered_data = clustered_data(data, cluster=1),
        clustering_function = function(x) 1,
        centroids = data[medoid_Indices,],
        inner_inequality = new_min_cost,
        sum_of_squares = structure(D[medoid_Indices,]^2,dim=c(K,n)) |> apply(c(2),min) |> sum(),
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
    old_medoid_Indices <- medoid_Indices
    old_min_cost <- new_min_cost

    ## calculate which changed medoids would diminsh the inner inequality most
    costs <- base::matrix(base::rep(1:n,length(medoid_Indices)),ncol = length(medoid_Indices))
    for(k in 1:length(medoid_Indices)){
      costs[,k] <- sapply(costs[,k],function(x) innerInequalityAfterChangingMedoid(x,medoid_Indices,k,dissimilarity_matrix=D))
    }

    ## Save which Indices of data point o and medoid m would have the lower cost
    m_opt <- arrayInd(which.min(costs), dim(costs))[2]
    o_opt <- arrayInd(which.min(costs), dim(costs))[1]

    if(.print_info)
      print(paste0('changing medoid ', m_opt, ' with data point ', o_opt))
    # change medoid

    medoid_Indices[m_opt] <- o_opt

    # new minimal
    new_min_cost <- costs[o_opt,m_opt]

  }

  distance <- getDistanceFunction(distance_method = distance_method,
                                  p = p,
                                  custom_distance_function = custom_distance_function)

  clustering_function <- function(x) 1:K |>
    sapply(function(k) distance(x,base::unlist(data[old_medoid_Indices[k],]))) |>
    base::which.min()

  best_clustered_data <- data |> dplyr::mutate(cluster = structure(D[old_medoid_Indices,],dim=c(K,n)) |> apply(c(2),which.min))


  ## returns clustered data and a function returning the cluster a datapoint (atomic vector) belongs to
  return(structure(
    list(
      clustered_data = clustered_data(best_clustered_data),
      clustering_function = clustering_function,
      medoids = data[old_medoid_Indices,],
      inner_inequality = old_min_cost,
      sum_of_squares = structure(D[old_medoid_Indices,]^2,dim=c(K,n)) |> apply(c(2),min) |> sum(),
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
#' @param medoid_Indices The Indices of the original medoids
#'   centroid/medoid.
#' @param m         an index of a data point to exchange with the medoid
#' @param dissimilarity_matrix    A matrix having encoded all distances between data points
#'
#' @returns the inner inequality, called cost, after the exchange of medoids
innerInequalityAfterChangingMedoid <- function(o,medoid_Indices,m,dissimilarity_matrix){

  # change medoid
  medoid_Indices[m] <- o

  # dont reward changes, which would result in a lower than promised cluster amount
  if(!base::identical(medoid_Indices,unique(medoid_Indices))){
    return(Inf)
  }

  # return the costs
  return(structure(dissimilarity_matrix[medoid_Indices,],dim=c(length(medoid_Indices),nrow(dissimilarity_matrix))) |> apply(c(2),min) |>  sum() )
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
#' @returns The Indices of good-enough clustering medoid in the \code{data}
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

  medoid_Indices <- medoid_index

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
      for(x in sort(medoid_Indices)) if(medoid_index >= x) medoid_index<-medoid_index+1

      # lastly: add the newly found medoid
      medoid_Indices <- c(medoid_Indices,medoid_index)
      medoids <- dplyr::add_row(medoids, data[medoid_index,])
    }
  }
  return(medoid_Indices)
}
