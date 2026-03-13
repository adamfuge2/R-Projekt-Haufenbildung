#' Hierarchical Clustering
#'
#' The Hierarchical Clustering algorithm.
#' Starts by assigning each datapoint a cluster-ID and then combining two clusters
#' closest to each other given a linkage mode and metric in each iteration. Merge
#' until n clusters remain.
#'
#' @param data a tibble or matrix of arbitrary dimension with each row representing
#' one data point.
#' @param K guessed number of clusters (at which the iteration stops).
#' @param mode linkage mode used to determine the distance between two clusters.
#' Available are \code{'centroid'}, \code{'single'}, \code{'complete'},
#' \code{'average'}.
#' @inheritParams getDistanceFunction
#' @param .print_info A logical of length 1. If \code{TRUE} additional information
#'   will be displayed during runtime. Used in debugging.
#'
#' @returns A list of the class 'clustering'. Contains \itemize{
#'   \item{\strong{\code{'clustered_data'}}} a tibble of original data with a new column called \code{'cluster'}
#'   \item{\strong{\code{'clustering_function'}}} a function applicable to known and
#'   unknown data points. Returns the cluster the data point belongs to.
#'   \item{\strong{\code{'inner_inequality'}}} a numeric. The sum of all differences of the data points to their cluster centroid.
#'   }
#' @export
hierarchicalClustering <- function(data, K, mode = "centroid", distance_method = "euclidean", p = NULL, custom_distance_function = NULL, .print_info = FALSE){

  # reformat the input and remove NAs
  data <- reformatDataInput(data)

  n <- base::nrow(data)

  if(K > n) stop(paste0("Dataset with ", n, " datapoints cannot have ", K, " clusters!"))

  # assign each datapoint a cluster-ID
  cluster <- 1:n


  distance <- getDistanceFunction(distance_method = distance_method,
                                  p = p,
                                  custom_distance_function = custom_distance_function)

  # calculate distances
  if(.print_info) print('Calculating Dissimilarity Matrix')
  D_points <- dissimilarityMatrix(data,
                                  distance_method = distance_method,
                                  p = p,
                                  custom_distance_function = custom_distance_function)
  if(.print_info) print('Done')

  diag(D_points) <- Inf
  D_cluster <- D_points

  # mode selection
  if(mode == "centroid"){
    mode <- generateLinkCentroidFast(data, distance)
  }
  else if(mode == "single"){
    mode <- linkSingleFast
  }
  else if(mode == "complete"){
    mode <- linkCompleteFast
  }
  else if(mode == "average"){
    mode <- linkAverageFast
  }
  else stop("Unknown linkage mode. Currently implemented selection includes: centroid, single, complete, average")

  if(.print_info) print("Starting main loop")
  while(cluster |> unique() |> length() > K){
    neighbors <- base::arrayInd(which.min(D_cluster), dim(D_cluster))  # indices of pair with least distance
    cluster[cluster==neighbors[2]] <- neighbors[1]

    D_cluster[neighbors[2], ] <- Inf
    D_cluster[, neighbors[2]] <- Inf

    distances <- sapply(cluster, function(x) .dist(x, mode, cluster, D_points, neighbors))

    D_cluster[neighbors[1], cluster] <- distances
    D_cluster[cluster, neighbors[1]] <- distances
  }
  if(.print_info) print("Done, formatting result")

  data <- dplyr::mutate(tibble::as_tibble(data), cluster = dplyr::dense_rank(cluster))

  return(structure(
    list(
      clustered_data = data),
    description = 'Data clustered by hierarchical clustering algorithm',
    class = 'clustering'
  )
  )
}
