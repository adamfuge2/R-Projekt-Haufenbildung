## Hierarchical cluster algorithm. Starts by assigning each datapoint a unique cluster
## ID and merging the two closest clusters to one at each step regarding a certain linkage
## mode and metric (see above). Merge until n clusters remain.
##
## Inputs:
## data,      a tibble or atomic vector with rows representing datapoints
## n,         number of clusters in the last iteration
## mode,      linkage mode (centroid, single, complete, average)
## metric,    a metric as defined above
##
## Returns:
## res        a tibble of same length as data with an entry 'cluster'
hierarchical_clustering <- function(data, n, mode = "centroid", metric = "euclidean", p = NULL, custom_metric = NULL){
  data <- tibble::rowid_to_column(tibble::tibble(data), "cluster")   #assign each datapoint a cluster-ID
  dim <- base::ncol(data) - 1

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
    mode <- centroid
  else if(mode == "single")
    mode <- single
  else if(mode == "complete")
    mode <- complete
  else if(mode == "average")
    mode <- average
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


  return(data) #### remove

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
        base::which.min(),
      inner_inequality = ),
    description = 'Data clustered by hierarchical clustering algorithm',
    class = 'clustering'
    )
  )
}


df <- generateClusterTestDataSimple(n = 20, dim = 3)
data <- hierarchical_clustering(df, 3)

dim <- 3
centroids <- data |>
  dplyr::ungroup() |>
  dplyr::group_by(cluster) |>
  dplyr::summarise_at(1:dim,mean) |>
  dplyr::select(-cluster)

metric <- euclidean
distances <- centroids |> apply(1,function(centroid){data[,1:dim] |> apply(1,function(x){metric(x,centroid)})}) |> t()

distances <- data |> dplyr::mutate(dist = metric())
distances
