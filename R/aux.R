############# Test Data Generators ######################

## spherical test data generator
##
## Inputs:
## n,           a positive integer. The number of total data points to be generated.
## n_clusters,  a positive integer. The number of clusters to generate.
##              If none given, choose a random amount < sqrt(n).
##
## Returns:
## a tibble,    every row representing a data point.
generateClusterTestDataSimple2D = function(n=100,n_clusters=NULL){
  if(base::missing(n_clusters)){
    n_clusters <- base::floor(stats::runif(1,min = 1, max = 2*base::sqrt(n)))
  }

  x_clusters <- stats::runif(n_clusters, min=0, max=1)
  y_clusters <- stats::runif(n_clusters, min=0, max=1)
  sd_clusters <- stats::runif(n_clusters, min=0.001, max=0.1)



  test_data = tibble::tibble(selected_clusters = base::floor(stats::runif(n,1,n_clusters+1))) |>
    dplyr::rowwise() |>
    dplyr::mutate(X = stats::rnorm(1,x_clusters[selected_clusters],sd_clusters[selected_clusters]),Y = stats::rnorm(1,y_clusters[selected_clusters],sd_clusters[selected_clusters])) |>
    dplyr::select(X,Y) |>
    dplyr::ungroup()


  return(test_data)
}



## nonspherical test data generator
##
## Inputs:
## n,               a positive integer. The number of total data points to be generated.
## list_of_paths,   a list containing tibbles. Each tibble containing points (in rows)
##                  to be interpreted as paths along which the data is generated
##
## Returns:
## a tibble,        every row representing a data point.
generateClusterTestData2DFromPaths = function(n=100,list_of_paths){

  n_clusters <- length(list_of_paths)
  paths <-lapply(list_of_paths,tibbleAsPath)
  sd_clusters <- stats::runif(n_clusters, min=0.001, max=0.02)
  points <- tibble::tibble(X=numeric(),Y=numeric())


  for(i in 1:n){
    which_cluster <- base::floor(runif(1,1,n_clusters+1))
    points <- tibble::add_row(points,paths[[which_cluster]](runif(1,0,1))+
                                tibble::tibble(X=rnorm(1,0,sd_clusters[which_cluster]), Y=rnorm(1,0,sd_clusters[which_cluster])))
  }


  return(points)
}








################## Helpers ####################

## Used to test if x is a whole number
##
## Inputs:
## x        a numeric to be tested
## tol      a numeric. An allowed tollerance, as not to fail at impercise calculations
##
## Returns:
## logical  TRUE, if x is a whole number, FALSE if not
is.wholenumber <- function(x, tol = base::.Machine$double.eps^0.5)  base::abs(x - base::round(x)) < tol


## converts a tibble of cluster centers (centroids) to a clustering function
##
## Inputs:
## centroids,   a tibble with every row being a centroid
## metric,           a metric whose 2 inputs are of the centroids row type
##
## Returns:
## function,    a clustering function relating any data point to their cluster.
##              input: rows or atomic vectors Of the data row type
##              returns: a whole number > 0, representing the related cluster
##
clusteringFromCentroids<- function(centroids,metric=euclidean){
  function(x)
    1:base::nrow(centroids) |>
    sapply(function(k) metric(x,base::unlist(centroids[k,]))) |>
    base::which.min()
}


## Calculate the inner inequality of the clusters, also called cost
## The lower the number, the heuristically better the clustering
##
## Inputs:
## data,        a tibble with with every row representing a data point.
## clustering,  a clustering function relating any data point to their cluster.
## metric,      a metric whose 2 inputs are of the data row type.
##
## Returns:
## numeric,     a real number > 0.
##
innerInequality <- function(data,clustering,metric=euclidean){

  clustered_data <- data |>
    dplyr::rowwise() |>
    dplyr::mutate(cluster=clustering(dplyr::c_across(dplyr::all_of(1:2))))

  centroids <- clustered_data |>
    dplyr::ungroup() |>
    dplyr::group_by(cluster) |>
    dplyr::summarise_at(1:base::ncol(data),base::mean) |>
    dplyr::select(-cluster)

  return(
    clustered_data |>
      dplyr::rowwise() |>
      dplyr::mutate(distances = metric(dplyr::c_across(1:base::ncol(data)), centroids[cluster,])) |>
      dplyr::ungroup() |>
      dplyr::summarise(base::sum(distances)) |>
      unlist(use.names=FALSE)
  )
}


## Silhouette of a clustering on ONE SPECIFIC data point.
## Gives insight as to how well the data point fits into its assigned cluster.
## A value near -1 means bad fit, a value near 1 means good fit.
## Be aware: Trivial cases outputs have been chosen arbitrarily/heuristically
##
## Inputs:
## data,        a tibble with with every row representing a data point.
## clustering,  a clustering function relating any data point to their cluster.
## o,           an atomic vector or tibble row.
##              This is the data point we calculate the silhouette of
## metric,      a metric whose 2 inputs are of the data row type.
##
## Returns:
## numeric,     a real number between -1 and 1
##
silhouette <- function(data,clustering,o,metric=euclidean){
  ## if o is not part of data, append it. This is a suprise tool that might hel us later
  data[base::nrow(data)+1,] <- o |>
    matrix(nrow=1) |>
    tibble::as_tibble(.name_repair = make.names)
  data <- dplyr::distinct(data)

  ## apply the clustering function to the data
  clustered_data <- data |>
    dplyr::rowwise() |>
    dplyr::mutate(cluster = clustering(dplyr::c_across(dplyr::all_of(1:2)))) |>
    dplyr::mutate(distance = metric(dplyr::c_across(1:base::ncol(data)), o))

  ## as set up above, o is now part of the data set with minimal distance to itself.
  ## We can thus derive the cluster of o by looking for the cluster of the data
  ## point with the least distance to o
  cluster_of_o <- clustered_data |>
    dplyr::arrange(distance) |>
    utils::head(1) |>
    dplyr::select(cluster) |>
    base::unlist(use.names=FALSE)

  ## Return zero, if o is the only data point in its cluster,
  ## else we would have a devide by zero error later.
  ## The choice 0 is ARBITRARY, but as the silhouette is bounded by -1 and 1 this
  ## choice means monoelemental clusterings are
  ## more encouraged than wrong clusterings (which have a negative silhouettecoefficient)
  ## and less encouraged than good natural clusterings (which have a slihouettecoefficient close to 1)
  if( clustered_data |>
      dplyr::filter(cluster == cluster_of_o) |>
      base::nrow() == 1){
    return(0)
  }

  ## remove o from the data set
  clustered_data <- clustered_data |>
    dplyr::ungroup()|>
    dplyr::slice(-1)



  ## calculate the distance of o to the clusters
  ## (meaning the mean distance of o to the points belonging to the clusters)
  clustered_data <- clustered_data |>
    dplyr::summarise(mean_distance = base::mean(distance), .by = cluster) |>
    dplyr::arrange(mean_distance)

  ## The overlap of o with its respective cluster
  ## note that this would be undefined if the cluster was now empty
  a_of_o <- clustered_data |>
    dplyr::filter(cluster == cluster_of_o) |>
    dplyr::select(mean_distance) |>
    base::unlist(use.names=FALSE)

  ## The best overlap of o with a cluster thats not the one of o
  b_of_o <- clustered_data |>
    dplyr::filter(cluster != cluster_of_o) |>
    dplyr::select(mean_distance) |>
    utils::head(1) |>
    base::unlist(use.names=FALSE)

  ## return the so called silhouette
  return( (b_of_o - a_of_o)/max(b_of_o, a_of_o) )
}


## Mean Silhouette of a clustering
## Gives insight as to how well the clustering fits the data in general
## by calculating the silhouette of all points and averaging them.
## A value near -1 means bad fit, a value near 1 means good fit.
## Be aware: Trivial cases outputs have been chosen arbitrarily/heuristically
##
## Inputs:
## data,        a tibble with with every row representing a data point.
## clustering,  a clustering function relating any data point to their cluster.
## metric,      a metric whose 2 inputs are of the data row type.
##
## Returns:
## numeric,     a real number between -1 and 1
##
meanSilhouette <- function(data,clustering,metric=euclidean){

  if( data |>
      dplyr::rowwise() |>
      dplyr::mutate(cluster = clustering(dplyr::c_across(dplyr::everything()))) |>
      dplyr::ungroup() |>
      dplyr::summarize(.by = cluster) |>
      base::nrow() == 1){
    ## If there's only one cluster, we cut the calculation short and return 0.
    ## Note that this is an ARBITRARY choice, but seems reasonable as it gives no info
    ## about wether the clustering with 1 cluster is good ore bad
    return(0)
  }

  ## Calculate the silhouette for every point and return their mean
  return( data |>
            dplyr::rowwise() |>
            dplyr::mutate(silhouette = silhouette(data,clustering,dplyr::c_across(all_of(1:base::ncol(data))),metric))|>
            dplyr::ungroup() |>
            dplyr::summarize(mean(silhouette)) |>
            base::unlist(use.names=FALSE)
  )
}



## Coerce a tibble into a mathematical path
##
## Inputs:
## data,        a tibble with every row representing a point along the path
##
## Returns:
## path,        a function relating a one d variable t to the data space
##              input: a numeric t in [0,1]
##              returns: a data row type
##
tibbleAsPath <- function(data){
  return(
    function(t){
      base::stopifnot('A path maps t in [0,1] to points in R^dim space' = 0<=t && t<=1)
      return( (t*(nrow(data)-1) - base::floor(t*(nrow(data)-1))) * data[1+base::floor(t*(base::nrow(data)-1)),] +
                (1-(t*(nrow(data)-1) - base::floor(t*(nrow(data)-1))))*data[1+base::ceiling(t*(base::nrow(data)-1)),])
    }
  )
}










################# Viewers ######################

## View clustered 2D data
## acts as a wrapper for view_data() in the case of unclustered data
##
## Inputs:
## data,        a tibble with every row representing a data point.
## clustering,  a clustering function applicable to the data.
##              If none given, the data will be displayed without clusters, wraps view_data().
viewClusters <- function(data,clustering=NULL){
  ## Invariant
  stopifnot('This function can currently only display clusterings of 2D data' = ncol(data)==2)


  if(base::missing(clustering)) viewData(data)
  else{
    colnames(data) <- c('X','Y')

    ## apply the cluster function to the data
    clustered_data <- data |> dplyr::rowwise() |> dplyr::mutate(cluster=clustering(dplyr::c_across(all_of(1:2))), cluster_label = clusterLabeling(cluster))

    ## Defer the number of clusters
    K <- clustered_data |> dplyr::filter(cluster!=0) |> dplyr::distinct(cluster) |> base::nrow()

    ## Display data as 2D scatter plot
    ggplot2::ggplot(clustered_data,ggplot2::aes(x=X,y=Y,colour = cluster_label)) +
      ggplot2::geom_point() +
      ggplot2::scale_color_manual(
        values = c(
          "Outlier" = "black",
          setNames(
            grDevices::rainbow(K),paste0('Cluster ',1:K)
          )
        )
      ) +
      ggplot2::coord_fixed()
  }
}



## View 2D data as a simple scatter plot
##
## Inputs:
## data,        a tibble with every row representing a data point.
viewData <- function(data){
  colnames(data) <- c('X','Y')

  ggplot2::ggplot(data,ggplot2::aes(x=X,y=Y)) +
    ggplot2::geom_point()
}


clusterLabeling <- function(x){
  if(x==0) return('Outlier')
  return(paste0('Cluster ',x))
}




######################### metrics #################################
##
## A metric is a function measuring the
## distance/closeness/inequality/relatedness/etc...
## for every metric here the following must (approximately) hold:
## 1. metric(x,y) = 0  if and only if x = y
## 2. metric(x,y) = metric(y,x)
## 3. metric(x,z) <= metric(x,y) + metric(y,z)
##
## Inputs:
## x,         an atomic vector or tibble row.
## y,         the same type as x.
##
## Returns:
## numeric,   a real number >= 0.



## The standard euclidean distance
##
## Inputs:
## x,         an atomic vector or tibble row with only real numbers
## y,         an atomic vector or tibble row with only real numbers
##
## Returns:
## numeric,   a real number >= 0.
euclidean <- function(x,y) base::sum((x-y)^2)


## The standard Manhattan, taxi or maximum metric
##
## Inputs:
## x,         an atomic vector or tibble row with only real numbers
## y,         an atomic vector or tibble row with only real numbers
##
## Returns:
## numeric,   a real number >= 0.
maximumMetric <- function(x,y) base::max(x-y)


## The standard Lp metric
##
## Inputs:
## x,         an atomic vector or tibble row with only real numbers
## y,         an atomic vector or tibble row with only real numbers
## p,         a real numeric with 1 <= p < Inf
##
## Returns:
## numeric,   a real number >= 0.
pMetric <- function(p) function(x,y) base::sum(base::abs(x-y)^p)^(2/p)





## Centroid determinator.
## Calculates the centroid of a tibble as the average of the
## vectors given in the rows.
##
## Input:
## data     a tibble with every row representing a point within the cluster
##
## Returns:
## tibble   a tibble with one row and same dimension as data representing the centroid
centroid_det <- function(data){   #determine centroid of a tibble (returns a tibble with one row)
  data |>
    dplyr::summarise(dplyr::across(tidyselect::everything(), ~ mean(.x, na.rm = TRUE)))
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



## The distance between the centroids of the two clusters
##
## Inputs:
## metric,    a metric as defined above
## data1,     a tibble with n(>=1) rows of dimension d
## data2      a tibble with m(>=1) rows of dimension d
##
## Returns:
## numeric    a real number >= 0.
centroid <- function(metric, data1, data2){
  centr_1 <- centroid_det(data1)
  centr_2 <- centroid_det(data2)
  return(metric(centr_1, centr_2))
}



## Mean intercluster dissimilarity. The average of the distances of all
## combinations between a point in cluster 1 and a point in cluster 2 given a metric
##
## Inputs:
## metric,    a metric as defined above
## data1,     a tibble with n(>=1) rows of dimension d
## data2      a tibble with m(>=1) rows of dimension d
##
## Returns:
## numeric    a real number >= 0.
average <- function(metric, data1, data2){   #employ the average distance method
  data1 |>
    dplyr::rowwise() |>
    dplyr::mutate(
      mean_dist = mean(
        sapply(1:nrow(data2), function(j){
          metric(data1[dplyr::cur_group_rows(), ], data2[j, ])
        })
      )
    ) |>
    dplyr::select(mean_dist) |>
    dplyr::ungroup() |>
    dplyr::summarise(mean = mean(.data$mean_dist)) |>
    dplyr::pull(mean)
}



## Minimal intercluster dissimilarity. Determines the smallest distance
## between points in cluster 1 and points in cluster 2 given a metric.
##
## Inputs:
## metric,    a metric as defined above
## data1,     a tibble with n(>=1) rows of dimension d
## data2      a tibble with m(>=1) rows of dimension d
##
## Returns:
## numeric    a real number >= 0.
single <- function(metric, data1, data2){
  data1 |>
    dplyr::rowwise() |>
    dplyr::mutate(
      min_dist = min(
        sapply(1:nrow(data2), function(j){
          metric(data1[dplyr::cur_group_rows(), ], data2[j, ])
        })
      )
    ) |>
    dplyr::select(min_dist) |>
    dplyr::ungroup() |>
    dplyr::summarise(min = min(.data$min_dist)) |>
    dplyr::pull(min)
}



## Maximum intercluster dissimilarity. Determines the largest distance
## between points in cluster 1 and points in cluster 2 given a metric.
##
## Inputs:
## metric,    a metric as defined above
## data1,     a tibble with n(>=1) rows of dimension d
## data2      a tibble with m(>=1) rows of dimension d
##
## Returns:
## numeric    a real number >= 0.
complete <- function(metric, data1, data2){
  data1 |>
    dplyr::rowwise() |>
    dplyr::mutate(
      max_dist = max(
        sapply(1:nrow(data2), function(j){
          metric(data1[dplyr::cur_group_rows(), ], data2[j, ])
        })
      )
    ) |>
    dplyr::select(max_dist) |>
    dplyr::ungroup() |>
    dplyr::summarise(max = max(.data$max_dist)) |>
    dplyr::pull(max)
}

