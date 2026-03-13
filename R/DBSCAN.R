# DBSCAN Algorithm
#'
#' Density-based clustering of applications with noise.
#'
#' @param data      a tibble with with every row representing a data point.
#'            The number columns is therefore the dimensionality,
#'            The number of rows is the sample size and called n.
#' @param epsilon neighborhood radius
#' @param min_Pts minimum number of points to form a cluster
#'
#' @returns A list of class `"clustering"` containing
#' \itemize{
#'   \item `clustered_data` original data with cluster labels
#'   \item `clustering_function` function assigning clusters
#' }
#'
#' @export
dbscan <- function(data, epsilon, min_Pts) {

 stopifnot("data must have at least one row" = nrow(data) >= 1)
 stopifnot("data must have at least one columns" = ncol(data) >= 1)
 stopifnot("data must contain only numeric values" = #we can discuss if we want that check
             all(vapply(data, is.numeric, logical(1)))) # Sonderfall einbauen; wenn alles numeric machen wir den so, sonst dissimillarity-matrix draufwerfen
 stopifnot("epsilon must be positive and numeric" = is.numeric(epsilon) && epsilon > 0)
 stopifnot("min_Pts must be a positive integer" = is.wholenumber(min_Pts) && min_Pts > 0)
 stopifnot("data must be data.frame or tibble" = is.data.frame(data))

 x <- as.matrix(data)    # data will be created by generateClusterTestDataSimple2D() or similiar function
 dist_x <- as.matrix(stats::dist(x)) # die Distanzmatrix von data
 n <- nrow(x) # number of data points
 cluster_id = 0L
 visited <- rep(FALSE, n)
 cluster_labels <- rep(0L, n)

 regionQuery <- function(point) {
   which(dist_x[point, ] <= epsilon) # Liste an Punkten, die innerhalb von Distanz epsilon um Punkt point liegen, point inklusive
 }

 expandCluster <- function(point, area, cluster_id) {
   cluster_labels[point] <<- cluster_id # must be super-assigned, or vanished after function call
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

 clustered_data <- data
 clustered_data$cluster <- cluster_labels

 clustering_function <- function(point) {
   point <- as.numeric(point)
   d <- sqrt(rowSums((x - matrix(point, nrow(x), ncol(x), byrow = TRUE))^2))
   neighbors <- which(d <= epsilon)
   if(length(neighbors) < min_Pts){
     return(0L)
   }
   cluster_labels[neighbors[1]]
 }

 return(structure(
   list(
     clustered_data = clustered_data,
     clustering_function = clustering_function
   ),
   description = 'Data clustered by DBSCAN algorithm',
   class= 'clustering'
 ))
}
