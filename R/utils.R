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
#' @param distance           a distance function whose 2 inputs are of the centroids row type
#'
#' Returns:
#' function,    a clustering function relating any data point to their cluster.
#'              input: rows or atomic vectors Of the data row type
#'              returns: a whole number > 0, representing the related cluster
#'
clusteringFromCentroids<- function(centroids,distance=euclidean){
  function(x)
    1:base::nrow(centroids) |>
    sapply(function(k) distance(x,base::unlist(centroids[k,]))) |>
    base::which.min()
}


#' Calculate the inner inequality of the clusters, also called cost
#'
#' The lower the number, the heuristically better the clustering
#'
#'
#' @param data,        a tibble with with every row representing a data point.
#' @param clustering_function,  a clustering function relating any data point to their cluster.
#' @param distance,      a distance function whose 2 inputs are of the data row type.
#'
#' @returns numeric, a real number > 0.
#'
innerInequality <- function(data,clustering_function,distance=euclidean){

  clustered_data <- data |>
    dplyr::rowwise() |>
    dplyr::mutate('cluster'=clustering_function(dplyr::c_across(dplyr::everything())))

  centroids <- clustered_data |>
    dplyr::ungroup() |>
    dplyr::group_by('cluster') |>
    dplyr::summarise_at(1:base::ncol(data),base::mean) |>
    dplyr::select(dplyr::all_of(1:(ncol(data)-1)))

  return(
    clustered_data |>
      dplyr::rowwise() |>
      dplyr::mutate('distances' = distance(dplyr::c_across(1:base::ncol(data)), centroids[.data$cluster,])) |>
      dplyr::ungroup() |>
      dplyr::summarise(base::sum(.data$distances)) |>
      unlist(use.names=FALSE)
  )
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
#' @param clustering  a clustering function relating any data point to their cluster.
#' @param distance    A character. One of \code{'euclidean'}, \code{'maximum'},
#'   \code{'Lp'} or \code{'manhattan'}.
#' @param p         A numeric greater than or equal to 1. If \code{distance} was
#'   chosen to be \code{'Lp'}, this will be used as the p of the p-Metric.
#' @param custom_distance_function A semi definite and symmetric function whose inputs are
#'   two of the \code{data} row type.
#'
#' @returns a numeric, a real number between -1 and 1
#'
meanSilhouette <- function(data,clustering,distance='euclidean',p=NULL,custom_distance_function=NULL){



  if(is.null(custom_distance_function)){
    if(distance=='euclidean')
      distance <- euclidean
    else if(distance=='maximum')
      distance <- maximumDistance
    else if(distance=='Lp'){
      stopifnot('If you chose the Lp distance function, please provide a value for p' = !is.null(p))
      stopifnot('p must be a numeric greater than or equal to 1' = is.numeric(p) && p>=1 )
      distance <- pDistance(p)
    }
    else if(distance=='manhattan')
      distance <- pDistance(1)
    else stop('Unknown distance function. Look up on the help page which metrics/distance functions are available ore input a custum distance function using the argument custom_distance_function.')
  }
  else distance <- custom_distance_function

  if( data |>
      apply(1,clustering) |>
      unique() |>
      length() == 1){
    ## If there's only one cluster, we cut the calculation short and return 0.
    ## Note that this is an ARBITRARY choice, but seems reasonable as it gives no info
    ## about whether the clustering with 1 cluster is good ore bad
    return(0)
  }

  ## Calculate the silhouette for every point and return their mean
  return( data |>
            apply(1,function(data_point) silhouette(data,clustering,data_point,distance)) |>
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
#' Calculates the matrix encoding the differences inbetween all data points
#'
#' @param data      a tibble with with every row representing a data point.
#' @param distance    A distance function whose inputs are of the data row type
#'
#' @returns a dissimilarity matrix, a row and coloumn for every data point
#'
dissimilarityMatrix <-function(data,distance){
  if(length(unique(lapply(data,typeof))) == 1){
    data <- as.matrix(data)}
  basis <- array(base::rep(1:base::nrow(data),base::nrow(data)), dim=c(base::nrow(data),base::nrow(data) ))
  M <- array(dim = c(base::nrow(data),base::nrow(data),2 ))
  M[,,1] <- basis
  M[,,2] <- t(basis)

  return(apply(M,c(1,2),function(x) distance(data[x[1],],data[x[2],])))
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

#' defer distance function from inputs
getDistanceFunction <- function(distance='euclidean',p=NULL,custom_distance_function=NULL){
  if(is.null(custom_distance_function)){
    if(distance=='euclidean')
      return(euclidean)
    else if(distance=='maximum')
      return(maximumDistance)
    else if(distance=='Lp'){
      stopifnot('If you chose the Lp distance function, please provide a value for p' = !is.null(p))
      stopifnot('p must be a numeric greater than or equal to 1' = is.numeric(p) && p>=1 )
      return(pDistance(p))
    }
    else if(distance=='manhattan')
      return(pDistance(1))
    else stop('Unknown metric. Look up on the help page which metrics are available ore input a custum metric using the argument custom_distance_function.')
  }
  else return(custom_distance_function)
}
