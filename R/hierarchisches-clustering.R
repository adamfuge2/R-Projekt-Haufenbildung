## Hierarchical cluster algorithm. Starts by assigning each datapoint a unique cluster
## ID and merging the two closest clusters to one at each step regarding a certain linkage
## mode and distance (see above). Merge until n clusters remain.
##
## Inputs:
## data,      a tibble or atomic vector with rows representing datapoints
## n,         number of clusters in the last iteration
## mode,      linkage mode (centroid, single, complete, average)
## distance,    a distance function as defined above
##
## Returns:
## res        a tibble of same length as data with an entry 'cluster'
hierarchical_clustering <- function(data, n, mode = centroid, distance = euclidean){
  data <- tibble::rowid_to_column(data, "cluster")   #assign each datapoint a cluster-ID

  while(data$cluster |> unique() |> length() > n){
    min_dist <- Inf   #minimal distance between two points
    neighbors <- c(0,0)
    cluster <- data$cluster |> unique()

    for (i in cluster){
      for (j in cluster[cluster != i]){
        tbl1 <- dplyr::filter(data, cluster == i) |> dplyr::mutate(cluster=NULL)  #remove the 'cluster' entry (otherwise it would add to the distance)
        tbl2 <- dplyr::filter(data, cluster == j) |> dplyr::mutate(cluster=NULL)
        dist <- mode(distance, tbl1, tbl2)
        if (dist <= min_dist) {
          min_dist <- dist
          neighbors <- base::sort(c(i, j))
        }
      }
    }
    data <- data |> dplyr::mutate(cluster = ifelse(cluster == neighbors[2], neighbors[1], cluster )) #assign all datapoints within the merged cluster the same id
  }
  data <- dplyr::mutate(data, cluster = dplyr::dense_rank(cluster))
  return(data)
}
