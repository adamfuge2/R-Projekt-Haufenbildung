#' Hierarchical Clustering
#'
#' The Hierarchical Clustering algorithm.
#' Starts by assigning each datapoint a cluster-ID and then combining two clusters
#' closest to each other given a linkage mode and metric in each iteration. Merge
#' until n clusters remain.
#'
#' @param data a tibble or matrix of arbitrary dimension with each row representing
#' one datapoint.
#' @param n guessed number of clusters (at which the iteration stops).
#' @param mode linkage mode used to determine the distance between two clusters.
#' Available are \code{'centroid'}, \code{'single'}, \code{'complete'},
#' \code{'average'}.
#' @param metric A character. One of \code{'euclidean'}, \code{'maximum'},
#'   \code{'Lp'} or \code{'manhattan'}.
#' @param p A numeric greater than or equal to 1. If \code{metric} was
#'   chosen to be \code{'Lp'}, this will be used as the p of the p-Metric.
#' @param custom_metric A semi definite and symmetric function whose inputs are
#'   two of the \code{data} row type.
#'
#' @returns A list of the class 'clustering'. Contains \itemize{
#'   \item{\strong{\code{'clustered_data'}}} a tibble of original data with a new column called \code{'cluster'}
#'   \item{\strong{\code{'clustering_function'}}} a function applicable to known and
#'   unknown data points. Returns the cluster the data point belongs to.
#'   \item{\strong{\code{'inner_inequality'}}} a numeric. The sum of all differences of the data points to their cluster centroid.
#'   }
#' @export
hierarchical_clustering_A <- function(data, K, mode = "centroid", metric = "euclidean", p = NULL, custom_metric = NULL, .print_info = FALSE){

  dim <- base::ncol(data)
  n <- base::nrow(data)

  if(K > n) stop(paste0("Dataset with ", n, " datapoints cannot have ", K, " clusters!"))

  #assign each datapoint a cluster-ID
  data <- tibble::rowid_to_column(tibble::as_tibble(data), "cluster") |>
    dplyr::relocate(cluster, .after = last_col())

  cluster <- 1:n

  # metric selection
  if(is.null(custom_metric)){
    if(metric=='euclidean')
      metric <- euclidean
    else if(metric=='maximum')
      metric <- maximumMetric
    else if(metric=='Lp'){
      stopifnot('If you chose the Lp metric, please provide a value for p' = !is.null(p))
      stopifnot('p must be a numeric greater than or equal to 1' = is.numeric(p) && p>=1 )
      metric <- pMetric(p)
    }
    else if(metric=='manhattan')
      metric <- pMetric(1)
    else stop('Unknown metric. Look up on the help page which metrics are available ore input a custum metric using the argument custom_metric.')
  }
  else metric <- custom_metric

  # mode selection
  if(mode == "centroid")
    mode <- linkCentroid
  else if(mode == "single")
    mode <- linkSingle
  else if(mode == "complete")
    mode <- linkComplete
  else if(mode == "average")
    mode <- linkAverage
  else stop("Unknown linkage mode. Currently implemented selection includes: centroid, single, complete, average")

  D <- dissimilarityMatrix(data,metric)

  diag(D) <- Inf
  arrayInd(which.min(D), dim(D))



  while(cluster |> unique() |> length() > K){
    min_dist <- Inf   #minimal distance between two points
    neighbors <- c(0,0)
    cluster <- data$cluster |> base::unique()
    for (i in cluster){
      for (j in cluster[cluster != i]){
        tbl1 <- dplyr::filter(data, cluster == i) |> dplyr::mutate(cluster=NULL)  #remove the 'cluster' entry (otherwise it would add to the distance)
        tbl2 <- dplyr::filter(data, cluster == j) |> dplyr::mutate(cluster=NULL)
        dist <- mode(metric, tbl1, tbl2)
        if (dist <= min_dist) {
          min_dist <- dist
          neighbors <- base::sort(c(i, j))
        }
      }
    }
    data <- data |> dplyr::mutate(cluster = ifelse(cluster == neighbors[2], neighbors[1], cluster )) #assign all datapoints within the merged cluster the same id
  }
  data <- dplyr::mutate(data, cluster = dplyr::dense_rank(cluster))


  centroids <- data |>
    dplyr::ungroup() |>
    dplyr::group_by(cluster) |>
    dplyr::summarise_at(1:dim,mean) |>
    dplyr::select(-cluster)

  return(structure(
    list(
      clustered_data = data,
      clustering_function = function(x)
        centroids |>
        apply(1,function(centroid) metric(x,centroid)) |>
        base::which.min()),
    description = 'Data clustered by hierarchical clustering algorithm',
    class = 'clustering'
  )
  )
}

############# Linkage Modes ######################
##
## A linkage mode is a method of calculating the dissimilarity
## of two clusters (of size >= 1) given a metric to determine a distance
##
## Inputs:
## metric,    metric as defined above
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
#' @param metric  metric function (euclidean, maximumMetric...)
#' @param data_1   a tibble with n(>=1) rows of dimension d
#' @param data_2   a tibble with n(>=1) rows of dimension d
#'
#' @returns a real number (numeric) >= 0
#' @export
linkCentroid <- function(metric, data_1, data_2){
  centr_1 <- centroid_det(data_1)
  centr_2 <- centroid_det(data_2)
  return(metric(centr_1, centr_2))
}



#' Linkage mode: average (FASTER)
#'
#' Mean intercluster dissimilarity. The average of the distances of all
#' combinations between a point in cluster 1 and a point in cluster 2 given
#' a metric
#' @famiy linkages
#'
#' @param TODO
#'
#'
#' @returns a real number (numeric) >= 0
#' @export
linkAverageFaster <- function(cluster_id_1, cluster_id_2, clusters, dissimilarity_matrix){   #employ the average distance method
  cluster_1_indeces <- which(cluster == cluster_id_1)
  cluster_2_indeces <- which(cluster == cluster_id_2)

  mean(dissimilarity_matrix[cluster_1_indeces,cluster_2_indeces])
}



#' Linkage mode: single (FASTER)
#'
#' Minimal intercluster dissimilarity. Determines the smallest distance between
#' points in cluster and 1 and points in cluster 2 given a metric.
#'
#' @famiy linkages
#'
#' @param TODO
#'
#'
#' @returns a real number (numeric) >= 0
#' @export
linkSingleFast <- function(cluster_id_1, cluster_id_2, clusters, dissimilarity_matrix){   #employ the average distance method
  cluster_1_indeces <- which(cluster == cluster_id_1)
  cluster_2_indeces <- which(cluster == cluster_id_2)

  min(dissimilarity_matrix[cluster_1_indeces,cluster_2_indeces])
}



#' Linkage mode: complete (FASTER)
#'
#' Maximum intercluster dissimilarity. Determines the largest distance
#' between points in cluster 1 and points in cluster 2 given a metric.
#'
#' @famiy linkages
#'
#' @param TODO
#'
#' @returns a real number (numeric) >= 0
#' @export
linkCompleteFast <- function(cluster_id_1, cluster_id_2, clusters, dissimilarity_matrix){   #employ the average distance method
  cluster_1_indeces <- which(cluster == cluster_id_1)
  cluster_2_indeces <- which(cluster == cluster_id_2)

  max(dissimilarity_matrix[cluster_1_indeces,cluster_2_indeces])
}
