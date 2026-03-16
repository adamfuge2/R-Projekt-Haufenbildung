#' Hierarchical Clustering
#'
#' The Hierarchical Clustering algorithm.
#' Starts by assigning each data point a cluster-ID and then combining two clusters
#' closest to each other given a linkage mode and metric in each iteration. Merge
#' until n clusters remain.
#'
#' @param data a tibble or matrix of arbitrary dimension with each row
#'   representing one data point.
#' @param min_cluster_amount An integer greater than 1. The \strong{first approach} to
#'   decide the cluster amount: Prevent the merge of the clusters with the
#'   largest step up in distances, but consider(and calculate) at least all
#'   merges resulting in this many (or more) clusters. The clustering will have
#'   at least this many clusters. Note that because we are comparing merges, you
#'   can not end up with one big cluster this way. (You can only choose one of these approaches)
#' @param exact_cluster_amount An integer greater than 0. The \strong{second approach} to
#'   decide the cluster amount: Stop merging clusters when this cluster amount
#'   has been reached. (You can only choose one of these approaches)
#' @param distance_limit A numeric greater than 0. The \strong{third approach} to decide
#'   the cluster amount: Stop merging clusters when the distance between the
#'   clusters is greater than this limit. (You can only choose one of these approaches)
#' @param mode linkage mode used to determine the distance between two clusters.
#'   Available are \code{'centroid'}, \code{'single'}, \code{'complete'},
#'   \code{'average'}.
#' @inheritParams getDistanceFunction
#' @param .print_info A logical of length 1. If \code{TRUE} additional
#'   information will be displayed during runtime. Used in debugging.
#'
#' @returns A list of the class 'clustering'. Contains \itemize{
#'   \item{\strong{\code{'clustered_data'}}} a tibble of original data with a new column called \code{'cluster'}
#'   \item{\strong{\code{'clustering_function'}}} a function applicable to known and
#'   unknown data points. Returns the cluster the data point belongs to.
#'   \item{\strong{\code{'inner_inequality'}}} a numeric. The sum of all differences of the data points to their cluster centroid.
#'   }
#' @export
hierarchicalClustering <- function(data,
                                   min_cluster_amount=2,
                                   exact_cluster_amount=NULL,
                                   distance_limit=NULL,
                                   mode = "single",
                                   distance_method = "euclidean",
                                   p = NULL,
                                   custom_distance_function = NULL,
                                   .print_info = FALSE){
  # Ways of calculating the 'right' cluster amount we call approach. Only on can be chosen at a time
  stopifnot(
    'Please choose only one approach of choosing a cluster amount. \n Only give provide at maximum one of \'min_cluster_amount\',\'exact_cluster_amount\' or \'distance_limit\'' =
      sum(c(
      missing(min_cluster_amount),
      missing(exact_cluster_amount),
      missing(distance_limit)
    )) >= 2
  )

  # K is the lower limit of Clusters this Algorithm computes.
  if(!missing(min_cluster_amount) || (missing(exact_cluster_amount) && missing(distance_limit))){
    # if a minimum  cluster amount got given or nothing at all (DEFAULT)
    # we will compute one more, as we are using the 'cut the dendrogram' approach.
    K <- min_cluster_amount - 1
  }
  else if(!missing(exact_cluster_amount)){
    # if an exact cluster amount got given we will compute exactly that many.
    K <- exact_cluster_amount
  }
  else if(!missing(distance_limit)){
    # if a distance limit for joining clusters got given, calculate all clusterings
    K <- 1
  }

  # reformat the input and remove NAs
  data <- reformatDataInput(data)

  # the number of data points we call n
  n <- base::nrow(data)

  # we keep track of the distances of joined clusters.
  # Used to decide a 'best' value for the luster amount later
  join_distances <- rep(0,n)

  if(K > n || K < 1) stop(paste0("Dataset with ", n, " data points cannot have ", K, " clusters!"))

  # assign each data point a cluster-ID
  cluster <- 1:n

  # we also keep track of the clusters,
  # such that we dont have to calculate the clusterings multiple times.
  saved_cluster <- matrix(0,ncol = n, nrow = n)
  saved_cluster[,n] <- cluster

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

  # Prepare the Matrix for finding the minimal distance to join/merge clusters.
  # As clusters can have distance 0 to itself, we want to ignore merging clusters with themselves
  diag(D_points) <- Inf

  # D_points will remain constant: The dissimilarity matrix n the data points
  # D_cluster will change: The dissimilarity matrix on the clusters using linkages
  D_cluster <- D_points

  # linkage mode selection
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

  # merge clusters until the limit K is reached
  while(cluster |> unique() |> length() > K){
    # calculate the minimum linkage-distance between clusters and save the distance
    join_distances[cluster |> unique() |> length()-1] <-  min(D_cluster)
    # indices of pair with least distance:
    # neighbors[1] cluster will be merged into neighbors[2]
    neighbors <- base::arrayInd(which.min(D_cluster), dim(D_cluster))
    cluster[cluster==neighbors[1]] <- neighbors[2]

    # save the Clustering after this merge for later
    saved_cluster[,cluster |> unique() |> length()] <- cluster

    # Ignore the now inexistant cluster in future calculations
    D_cluster[neighbors[1], ] <- Inf
    D_cluster[, neighbors[1]] <- Inf

    # calculate new distances
    distances <- sapply(unique(cluster), function(x) .dist(x, mode, cluster, D_points, neighbors))

    # update the Dissimilarity matrix between clusters
    D_cluster[neighbors[2], unique(cluster)] <- distances
    D_cluster[unique(cluster), neighbors[2]] <- distances
  }
  if(.print_info) print("Done, deferring cluster amount")


  # Now extract the 'right' cluster amount from the clusterings
  if(!missing(min_cluster_amount) || (missing(exact_cluster_amount) && missing(distance_limit))){
    # if a minimum  cluster amount got given or nothing at all (DEFAULT)
    # we use the 'cut the dendrogram' approach, meaning:
    # identify the largest difference between the distances of the merges
    # (aka find the longest nonintersected vertical line in a dendrogram)
    # and choose the related cluster amount.
    # (aka cut the dendrogram perpendicular to that line)
    cluster_amount <- K + which.max(join_distances[K:n] - c(join_distances[(K+1):n],0))
  }
  if(!missing(exact_cluster_amount)){
    # if an exact cluster amount got given, that is our desired cluster amount
    cluster_amount <- exact_cluster_amount
  }
  if(!missing(distance_limit)){
    # if a distance limit for joining clusters got given, we 'cut' the 'dendrogram'
    # at the distance limit, and look how many merges would thus not do.
    # this is our desired best cluster amount.
    cluster_amount <- length(join_distances[join_distances > distance_limit]) +1
  }

  # using the linkages we can also defer clusters of unknown data points
  # by looking at with which cluster they would merge
  f <- function(data_point){
    distances <- sapply(1:n,function(i) distance(data_point,data[i,]))
    D_points_new <- cbind(D_points,distances)
    D_points_new <- rbind(D_points_new,c(distances,Inf))
    cluster_new <- c(saved_cluster[,cluster_amount],n+1)

    linkage_distances <- sapply(unique(saved_cluster[,cluster_amount]),
                                function(x) mode(n+1, x, cluster_new, D_points_new))

    unique(saved_cluster[,cluster_amount])[which.min(linkage_distances)]
  }


  return(structure(
    list(
      clustered_data = clustered_data(data,saved_cluster[,cluster_amount]),
      cluster_amount = cluster_amount,
      clustering_function = f),
    description = 'Data clustered by hierarchical clustering algorithm',
    class = c('hierarchical-clustering','clustering')
  )
  )
}
