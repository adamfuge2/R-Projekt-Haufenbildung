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
hierarchical_clustering <- function(data, n, mode = "centroid", metric = "euclidean", p = NULL, custom_metric = NULL){

  dim <- base::ncol(data)
  if(n > ncol(data)) stop(paste0("Dataset with ", ncol(data), " datapoints cannot have ", n, " clusters!"))

  #assign each datapoint a cluster-ID
  data <- tibble::rowid_to_column(tibble::as_tibble(data), "cluster") |>
    dplyr::relocate(cluster, .after = last_col())

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

  while(data$cluster |> unique() |> length() > n){
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
