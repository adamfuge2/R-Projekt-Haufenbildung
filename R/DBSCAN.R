# DBSCAN Algorithm
#'
#' Density-based clustering of applications with noise.
#'
#' @param data      a tibble with with every row representing a data point.
#'            The number columns is therefore the dimensionality,
#'            The number of rows is the sample size and called n.
#' @param epsilon neighborhood radius
#' @param min_Pts minimum number of points to form a cluster
#' @inheritParams getDistanceFunction
#'
#' @returns A list of class `"clustering"` containing
#' \itemize{
#'   \item `clustered_data` original data with cluster labels
#'   \item `clustering_function` function assigning clusters
#' }
#'
#' @references https://en.wikipedia.org/wiki/DBSCAN
#' @export
dbscan <- function(data, epsilon, min_Pts,
                   distance_method = "euclidean",
                   p = NULL,
                   custom_distance_function = NULL) {

 stopifnot("data must have at least one row" = nrow(data) >= 1)
 stopifnot("data must have at least one columns" = ncol(data) >= 1)
 if(is.null(distance_method) && is.null(custom_distance_function)) {
   stopifnot("data must contain only numeric values for default distance" = all(vapply(data, is.numeric, logical(1))))
 }
 stopifnot("epsilon must be positive and numeric" = is.numeric(epsilon) && epsilon > 0)
 stopifnot("min_Pts must be a positive integer" = is.wholenumber(min_Pts) && min_Pts > 0)
 stopifnot("data must be data.frame or tibble" = is.data.frame(data))

 x <- as.matrix(data)
 if(is.null(distance_method)) {
   dist_x <- as.matrix(stats::dist(x))
 } else { dist_x <- dissimilarityMatrix(
                                data,
                                distance_method = distance_method,
                                p = p,
                                custom_distance_function = custom_distance_function)
 }
 n <- nrow(x)
 cluster_id = 0L
 visited <- rep(FALSE, n)
 cluster_labels <- rep(0L, n)

 regionQuery <- function(point) {
   which(dist_x[point, ] <= epsilon)
 }

 expandCluster <- function(point, area, cluster_id) {
   cluster_labels[point] <<- cluster_id
   # add point to cluster / assign cluster to point
   i <- 1
   while(i <= length(area)) { # for each element p in region:
     pt <- area[i]
     if (visited[pt] == FALSE) { # if p not visited:
       visited[pt] <<- TRUE # mark p as visited
       new_area <- regionQuery(pt) # new_region = data.regionQuery(p, eps)

       if(length(new_area) >= min_Pts) { # if sizeof(new_region) >= min_Pts
         area <- unique(c(area, new_area)) # remove duplicate, points exist only once in cluster; region = region joined with new_region
       }
     }
     if(cluster_labels[pt] == 0L) { # if p not in any cluster
       cluster_labels[pt] <<- cluster_id # unmark p as noise
     }
     i <- i + 1
   }
 }


 # for each element in data (point in n)
 for(point in seq_len(n))
  if(visited[point] == FALSE) {
    visited[point] <- TRUE    # mark point as visited
    area <- regionQuery(point) # get neighbors
    if(length(area) < min_Pts) {
      cluster_labels[point] <- 0L # mark i as noise
    }
    else {
      cluster_id <- cluster_id + 1
      expandCluster(point, area, cluster_id)
    }
  }

 clustering_function <- function(point) {

  point_df <- as.data.frame(as.list(point))
  colnames(point_df) <- colnames(data)

  dists <- dissimilarityMatrix(
    rbind(point_df, data),
    distance_method = distance_method,
    p = p,
    custom_distance_function = custom_distance_function
   )[1, -1]

  neighbors <- which(dists <= epsilon)

  if(length(neighbors) < min_Pts) {
    return(0L)
  }

  neighbor_clusters <- cluster_labels[neighbors]
  neighbor_clusters <- neighbor_clusters[neighbor_clusters != 0]

  if(length(neighbor_clusters) == 0) {
   return(0)
  }

  return(neighbor_clusters[[1]])

 }

 return(structure(
   list(
     clustered_data = clustered_data(data,cluster_labels),
     clustering_function = clustering_function
   ),
   description = 'Data clustered by DBSCAN algorithm',
   class= c('dbscan','clustering')
 ))
}
