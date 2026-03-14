############# Linkage Modes ######################
##
## A linkage mode is a method of calculating the dissimilarity
## of two clusters (of size >= 1) given a distance to determine a distance
##
## Inputs:
## distance,    distance as defined above
## data1,     a tibble with n(>=1) rows of dimension d
## data2      a tibble with m(>=1) rows of dimension d
##
## Returns:
## numeric    a real number >= 0.


#' Centroid determinator
#'
#' The distance between the centroids of two clusters
#'
#' @param data a tibble with n(>=1) rows of dimension d
#'
#' @returns a tibble of dimension d with one row representing the centroid
#' @export
centroid_det <- function(data){   #determine centroid of a tibble (returns a tibble with one row)
  data |>
    dplyr::summarise(dplyr::across(tidyselect::everything(), ~ mean(.x, na.rm = TRUE)))
}


#' Linkage mode: centroid
#'
#' The distance between the centroids of two clusters
#'
#' @param distance  distance function (euclidean, maximumDistance...)
#' @param data_1   a tibble with n(>=1) rows of dimension d
#' @param data_2   a tibble with n(>=1) rows of dimension d
#'
#' @returns a real number (numeric) >= 0
#' @export
linkCentroid <- function(distance, data_1, data_2){
  centr_1 <- centroid_det(data_1)
  centr_2 <- centroid_det(data_2)
  return(distance(centr_1, centr_2))
}



#' Linkage mode: average
#'
#' Mean intercluster dissimilarity. The average of the distances of all
#' combinations between a point in cluster 1 and a point in cluster 2 given
#' a distance function
#'
#' @param distance  distance function (euclidean, maximumDistance...)
#' @param data_1   a tibble with n(>=1) rows of dimension d
#' @param data_2   a tibble with n(>=1) rows of dimension d
#'
#' @returns a real number (numeric) >= 0
#' @export
linkAverage <- function(distance, data_1, data_2){   #employ the average distance method
  data_1 |>
    dplyr::rowwise() |>
    dplyr::mutate(
      'mean_dist' = mean(
        sapply(1:nrow(data_2), function(j){
          distance(data_1[dplyr::cur_group_rows(), ], data_2[j, ])
        })
      )
    ) |>
    dplyr::select('mean_dist') |>
    dplyr::ungroup() |>
    dplyr::summarise('mean' = mean(.data$mean_dist)) |>
    dplyr::pull(.data$mean)
}



#' Linkage mode: single
#'
#' Minimal intercluster dissimilarity. Determines the smallest distance between
#' points in cluster and 1 and points in cluster 2 given a distance function.
#'
#' @param distance  distance function (euclidean, maximumDistance...)
#' @param data_1   a tibble with n(>=1) rows of dimension d
#' @param data_2   a tibble with n(>=1) rows of dimension d
#'
#' @returns a real number (numeric) >= 0
#' @export
linkSingle <- function(distance, data_1, data_2){
  data_1 |>
    dplyr::rowwise() |>
    dplyr::mutate(
      'min_dist' = min(
        sapply(1:nrow(data_2), function(j){
          distance(data_1[dplyr::cur_group_rows(), ], data_2[j, ])
        })
      )
    ) |>
    dplyr::select('min_dist') |>
    dplyr::ungroup() |>
    dplyr::summarise('min' = min(.data$min_dist)) |>
    dplyr::pull(.data$min)
}



#' Linkage mode: complete
#'
#' Maximum intercluster dissimilarity. Determines the largest distance
#' between points in cluster 1 and points in cluster 2 given a metric.
#'
#' @param distance  distance function (euclidean, maximumDistance...)
#' @param data_1   a tibble with n(>=1) rows of dimension d
#' @param data_2   a tibble with n(>=1) rows of dimension d
#'
#' @returns a real number (numeric) >= 0
#' @export
linkComplete <- function(distance, data_1, data_2){
  data_1 |>
    dplyr::rowwise() |>
    dplyr::mutate(
      'max_dist' = max(
        sapply(1:nrow(data_2), function(j){
          distance(data_1[dplyr::cur_group_rows(), ], data_2[j, ])
        })
      )
    ) |>
    dplyr::select('max_dist') |>
    dplyr::ungroup() |>
    dplyr::summarise('max' = max(.data$max_dist)) |>
    dplyr::pull(.data$max)
}







################ FASTER #################

#' Centroid Fast Function Generator
#'
#' Takes a dataset and a distance function (metric) to generate a centroid-linkage mode
#' compatible in signature with the other linkage modes.
#'
#' @family Linkage Modes
#' @param data a tibble with n(>=1) rows of dimension d
#' @param distance_function a distance function (euclidean, maximum,...)
#'
#' @returns linkage mode centroid
#' @export
generateLinkCentroidFast <- function(data, distance_function){
  linkCentroidFast <- function(cluster_id_1, cluster_id_2, cluster, dissimilarity_matrix){
    cluster_1_indices <- which(cluster == cluster_id_1)
    cluster_2_indices <- which(cluster == cluster_id_2)

    centroid_1 <- colMeans(data[cluster_1_indices, ])
    centroid_2 <- colMeans(data[cluster_2_indices, ])
    return(distance_function(centroid_1, centroid_2))
  }
  return(linkCentroidFast)
}

#' Linkage mode: average (FASTER)
#'
#' Mean intercluster dissimilarity. The average of the distances of all
#' combinations between a point in cluster 1 and a point in cluster 2 given
#' a metric
#' @family Linkage Modes
#'
#' @param cluster_id_1 number denoting the first cluster ID
#' @param cluster_id_2 number denoting the second cluster ID
#' @param cluster atomic vector assigning a cluster to each data point
#' @param dissimilarity_matrix a matrix containing all distances between points
#' within the dataset
#'
#' @returns a real number (numeric) >= 0
#' @export
linkAverageFast <- function(cluster_id_1, cluster_id_2, cluster, dissimilarity_matrix){
  cluster_1_indices <- which(cluster == cluster_id_1)
  cluster_2_indices <- which(cluster == cluster_id_2)

  mean(dissimilarity_matrix[cluster_1_indices,cluster_2_indices])
}



#' Linkage mode: single (FASTER)
#'
#' Minimal intercluster dissimilarity. Determines the smallest distance between
#' points in cluster and 1 and points in cluster 2 given a metric.
#'
#' @family Linkage Modes
#'
#' @inheritParams linkAverageFast
#'
#' @returns a real number (numeric) >= 0
#' @export
linkSingleFast <- function(cluster_id_1, cluster_id_2, cluster, dissimilarity_matrix){   #employ the average distance method
  cluster_1_indices <- which(cluster == cluster_id_1)
  cluster_2_indices <- which(cluster == cluster_id_2)

  min(dissimilarity_matrix[cluster_1_indices,cluster_2_indices])
}



#' Linkage mode: complete (FASTER)
#'
#' Maximum intercluster dissimilarity. Determines the largest distance
#' between points in cluster 1 and points in cluster 2 given a metric.
#'
#' @family Linkage Modes
#'
#' @inheritParams linkAverageFast
#'
#' @returns a real number (numeric) >= 0
#' @export
linkCompleteFast <- function(cluster_id_1, cluster_id_2, cluster, dissimilarity_matrix){   #employ the average distance method
  cluster_1_indices <- which(cluster == cluster_id_1)
  cluster_2_indices <- which(cluster == cluster_id_2)

  max(dissimilarity_matrix[cluster_1_indices,cluster_2_indices])
}


############ Distance Function #################
#' Internal distance function
#'
#' Helper function to determine distance vector between two clusters (for hierarchical clustering)
#'
#' @param x number denoting the cluster whose distance should be determined
#' @param mode linkage mode (function)
#' @param cluster atomic vector assigning a cluster to each data point
#' @param D_points dissimilarity matrix for individual points
#' @param neighbors atomic vector of length 2 denoting the two cluster IDs of
#' minimal distance
#'
#' @return a real number (numeric) >= 0
.dist <- function(x, mode, cluster, D_points, neighbors){
  if(x == neighbors[2])
    return(Inf)
  else
    return(mode(neighbors[2], x, cluster, D_points))
}


