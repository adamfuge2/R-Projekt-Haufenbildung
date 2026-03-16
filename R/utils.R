################## Helpers ####################

#' Test if x is a whole number
#'
#' @param x        a numeric to be tested
#' @param tol      a numeric. An allowed tolerance, as not to fail at imprecise calculations
#'
#' @returns a logical:  TRUE, if x is a whole number, FALSE if not
is.wholenumber <- function(x, tol = base::.Machine$double.eps^0.5)  base::abs(x - base::round(x)) < tol


#' Coerce a data frame to clustering
#'
#' Coerces a tibble of cluster centers (centroids) to a clustering function
#'
#' @param centroids   a tibble with every row being a centroid
#' @inheritParams getDistanceFunction
#'
#'
#' @returns function,    a clustering function relating any data point to their cluster.
#'              input: rows or atomic vectors Of the data row type
#'              returns: a whole number > 0, representing the related cluster
#'
clusteringFromCentroids<- function(centroids,distance_method='euclidean',p=NULL,custom_distance_function=NULL){
  distance <- getDistanceFunction(distance_method = distance_method,
                                  p = p,
                                  custom_distance_function = custom_distance_function)
  function(x)
    1:base::nrow(centroids) |>
    sapply(function(k) distance(x,base::unlist(centroids[k,]))) |>
    base::which.min()
}




#' Silhouette of a clustering on ONE SPECIFIC data point.
#'
#' Gives insight as to how well the data point fits into its assigned cluster.
#' A value near -1 means bad fit, a value near 1 means good fit.
#'
#' Be aware: Trivial cases outputs have been chosen arbitrarily/heuristically
#'
#'
#' @param data        a tibble with with every row representing a data point.
#' @param clustering_function  a clustering function relating any data point to their cluster.
#' @param o           an atomic vector or tibble row.
#'              This is the data point we calculate the silhouette of
#' @param distance      a distance function whose 2 inputs are of the data row type.
#' @param is_part_of_data A logical of length 1. If \code{TRUE} we assume that the data point is already part of the `data`.
#'
#' @returns numeric, a real number between -1 and 1
#'
silhouette <- function(data,clustering_function,o,distance=euclidean,is_part_of_data=TRUE){
  # for more understandable code and inputs we rename this here
  cluster <- clustering_function

  ## if o is part of data, remove it
  if(is_part_of_data) data <- data |> dplyr::filter(duplicated(data) | !apply(data,1,function(row) all(row==o)) )

  ## apply the clustering function to the data
  clusters <- apply(data,1,function(data_point) cluster(data_point))
  distances <- apply(data,1,function(data_point) distance(data_point,o))

  # do i really have to explain this?
  cluster_of_o <- cluster(o)

  ## Return zero, if o is the only data point in its cluster,
  ## else we would have a devide by zero error later.
  ## The choice 0 is ARBITRARY, but as the silhouette is bounded by -1 and 1 this
  ## choice means monoelemental clusterings are
  ## more encouraged than wrong clusterings (which have a negative silhouettecoefficient)
  ## and less encouraged than good natural clusterings (which have a slihouettecoefficient close to 1)
  if( length(clusters[clusters==cluster_of_o]) == 0 ){
    return(0)
  }

  ## Return zero, if theres only 1 cluster,
  ## else we would have a devide by zero error later.
  ## The choice 0 is ARBITRARY, but as the silhouette is bounded by -1 and 1 this
  ## choice means trivial clusterings are always less encouraged than good natural
  ## clusterings (which have a slihouettecoefficient close to 1)
  if( length(unique(clusters)) == 1 && clusters[[1]] == cluster(o) ){
    return(0)
  }

  cluster_ids <- unique(clusters[clusters!=0])

  mean_of_distances <- cluster_ids |> sapply(function(k) mean(distances[clusters == k]))

  a_of_o <- mean_of_distances[cluster_of_o]
  b_of_o <- mean_of_distances[-cluster_of_o] |> min()

  ## return the so called silhouette
  return( (b_of_o - a_of_o)/max(b_of_o, a_of_o) )
}

#' A faster silhouette calculator
#'
#' Calculates the Silhouette of a point like [silhouette()], but in order to
#' fasten up the calculations \strong{does not really} check its inputs. It also assumes
#' that the data point in question is part of the data and that the
#' clustered_data has a column called `'cluster'`.
#'
#' @param clustered_data A clustered_data object, or at least a tibble with a column called `'cluster'`
#' @param index_of_o An integer between 1 and `nrow(clustered_data)`
#' @param dissimilarity_matrix A matrix representing the distances of the data points
#'
#' @returns numeric, a real number between -1 and 1.
#'    The larger, the better the data point fits into its own cluster compared
#'    to the next nearest cluster.
silhouette_faster <- function(clustered_data,index_of_o,dissimilarity_matrix){

  o <- clustered_data[index_of_o,-ncol(clustered_data)]
  cluster_of_o <- clustered_data[index_of_o,ncol(clustered_data)] |> unlist(use.names = FALSE)


  ## Return zero, if theres only 1 cluster,
  ## else we would have a devide by zero error later.
  ## The choice 0 is ARBITRARY, but as the silhouette is bounded by -1 and 1 this
  ## choice means trivial clusterings are always less encouraged than good natural
  ## clusterings (which have a slihouettecoefficient close to 1)
  if( length(unique(clustered_data$cluster)) == 1 ){
    return(0)
  }

  ## We assume o is part of data. We need to remove it
  clustered_data <- clustered_data[-index_of_o,]



  ## store the clusters seperately
  clusters <- clustered_data$cluster


  distances_to_o <- dissimilarity_matrix[index_of_o,-index_of_o]



  ## Return zero, if o is the only data point in its cluster,
  ## else we would have a devide by zero error later.
  ## The choice 0 is ARBITRARY, but as the silhouette is bounded by -1 and 1 this
  ## choice means monoelemental clusterings are
  ## more encouraged than wrong clusterings (which have a negative silhouettecoefficient)
  ## and less encouraged than good natural clusterings (which have a slihouettecoefficient close to 1)
  if( !(cluster_of_o %in% clusters) ){
    return(0)
  }


  cluster_ids <- unique(clusters[clusters!=0])

  ## We assume o is part of data. We need to remove it
  clustered_data <- clustered_data[-index_of_o,]

  mean_of_distances <- cluster_ids |> sapply(function(k) mean(distances_to_o[clusters == k]))


  a_of_o <- mean_of_distances[cluster_ids == cluster_of_o]
  b_of_o <- mean_of_distances[cluster_ids != cluster_of_o] |> min()


  ## return the so called silhouette
  return( (b_of_o - a_of_o)/max(b_of_o, a_of_o) )
}


#' Mean Silhouette of a clustering
#'
#' Gives insight as to how well the clustering fits the data in general
#' by calculating the silhouette of all points and averaging them.
#' A value near -1 means bad fit, a value near 1 means good fit.
#'
#' Be aware: Trivial cases outputs have been chosen arbitrarily/heuristically
#'
#' @param data        a tibble with with every row representing a data point.
#' @param clustering_function  a clustering function relating any data point to their cluster.
#' @inheritParams getDistanceFunction
#'
#' @returns a numeric, a real number between -1 and 1
#'
meanSilhouette <- function(data,
                           clustering_function,
                           distance_method='euclidean',
                           p=NULL,
                           custom_distance_function=NULL){

  D <- dissimilarityMatrix(data,
                           distance_method = distance_method,
                           p=p,
                           custom_distance_function=custom_distance_function)

  cluster <- data |>
    apply(1,clustering_function)

  data$cluster <- cluster

  if( cluster |>
      unique() |>
      length() == 1){
    ## If there's only one cluster, we cut the calculation short and return 0.
    ## Note that this is an ARBITRARY choice, but seems reasonable as it gives no info
    ## about whether the clustering with 1 cluster is good ore bad
    return(0)
  }


  ## Calculate the silhouette for every point and return their mean
  return( 1:nrow(data) |>
            sapply(function(data_point_index) silhouette_faster(data,data_point_index,D)) |>
            mean()

  )
}



#' Coerce a tibble into a mathematical path
#'
#' @param data        a tibble with every row representing a point along the path
#'
#'
#' @returns path, a function relating a one dim variable t to the data space
#'              input: a numeric t in [0,1]
#'              returns: a data row type
#'
tibbleAsPath <- function(data){
  return(
    function(t){
      base::stopifnot('A path maps t in [0,1] to points in R^dim space' = 0<=t && t<=1)
      return( (t*(nrow(data)-1) - base::floor(t*(nrow(data)-1))) * data[1+base::floor(t*(base::nrow(data)-1)),] +
                (1-(t*(nrow(data)-1) - base::floor(t*(nrow(data)-1))))*data[1+base::ceiling(t*(base::nrow(data)-1)),])
    }
  )
}


#' Dissimilarity matrix
#'
#' Calculates the matrix encoding the differences in between all data points
#'
#' @param data      a tibble with with every row representing a data point.
#' @inheritParams getDistanceFunction
#'
#' @returns a dissimilarity matrix, a row and column for every data point
#' @export
dissimilarityMatrix <-function(data,distance_method='euclidean',p=NULL,custom_distance_function=NULL){
  if(is.null(custom_distance_function)){
    if(any(distance_method == c('euclidean',
                                'maximum',
                                'manhattan',
                                'canberra',
                                'binary',
                                'minkowski')))
      return(structure(as.matrix(stats::dist(data,method = distance_method,p=p)),dimnames=NULL))
    else stop('Unknown metric. Look up on the help page which metrics are available ore input a custum metric using the argument custom_distance_function.')
  }


  if(length(unique(lapply(data,typeof))) == 1){
    data <- as.matrix(data)
    return(  apply(data,1,function(x) apply(data,1,function(y) custom_distance_function(x,y))))
  }

  # if nothing else works, at least half the work by only calculating the upper
  # triangle matrix
  upper <- sapply(1:nrow(data),function(i) c(sapply(1:i,function(j) custom_distance_function(data[i,],data[j,])),rep(0,nrow(data)-i)))
  return(upper + t(upper))
}


#' Reformat Data Input
#'
#' Removes NA and inserts column names if none are given
#'
#' @param data a tibble or matrix of arbitrary dimension with each row representing
#' one datapoint.
#'
#' @returns tibble of same type as input
#' @export
reformatDataInput <- function(data){
  if(base::length(base::colnames(data)) == 0)
    base::colnames(data) <- paste0('X_', 1:ncol(data))
  return(stats::na.omit(tibble::as_tibble(data)))
}


#' The Inequality of a vector to the data
#'
#' Sums the distances of a vector to all pints in the data set.
#'
#' @param data      a tibble with with every row representing a data point.
#' @param vector    A vector (i.e. tibble row or atomic vector)
#' @param distance    A distance function whose inputs are of the data row type
#'
#' @returns a numeric >= 0
#'
sumOfDistancestTo <- function(data,vector,distance){
  data |> dplyr::rowwise() |> dplyr::mutate('distance' = distance(dplyr::c_across(all_of(1:ncol(data))) , vector)) |>
    dplyr::ungroup() |>
    dplyr::summarise('sum' = sum(.data$distance)) |>
    base::unlist(use.names = FALSE)
}

#' Defer distance function from inputs
#'
#' @param distance_method A character. One of \code{'euclidean'}, \code{'maximum'},
#'   \code{'Lp'} or \code{'manhattan'}.
#' @param p A numeric greater than or equal to 1. If \code{distance_method} was
#'   chosen to be \code{'Lp'}, this will be used as the p of the p-Metric.
#' @param custom_distance_function A semi definite and symmetric function whose inputs are
#'   two of the \code{data} row type.
#'
#' @returns a distance function
#' @export
getDistanceFunction <- function(distance_method = 'euclidean',
                                p = NULL,
                                custom_distance_function = NULL){
  if(is.null(custom_distance_function)){
    if(distance_method == 'euclidean')
      return(euclidean)
    else if(distance_method == 'maximum')
      return(maximumDistance)
    else if(distance_method=='minkowski'){
      stopifnot('If you chose the Lp distance function, please provide a value for p' = !is.null(p))
      stopifnot('p must be a numeric greater than or equal to 1' = is.numeric(p) && p>=1 )
      return(pDistance(p))
    }
    else if(distance_method=='manhattan')
      return(manhattan)
    else if(distance_method=='canberra')
      return(canberra)
    else if(distance_method=='binary')
      return(binary)
    else stop('Unknown metric. Look up on the help page which metrics are available ore input a custum metric using the argument custom_distance_function.')
  }
  else return(custom_distance_function)
}

as.clustered_data <- function(data,clustering_function){
  if(length(unique(lapply(data,typeof))) == 1){
    m_data <- as.matrix(data)
    return(  data |> dplyr::mutate('cluster'=apply(m_data,1,function(data_point) clustering_function(data_point))))
  }

  return(data |> dplyr::mutate(cluster=sapply(1:nrow(data),function(data_point_index) clustering_function(data[data_point_index,]))))
}
