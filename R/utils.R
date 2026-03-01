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
#' @param metric           a metric whose 2 inputs are of the centroids row type
#'
#' Returns:
#' function,    a clustering function relating any data point to their cluster.
#'              input: rows or atomic vectors Of the data row type
#'              returns: a whole number > 0, representing the related cluster
#'
clusteringFromCentroids<- function(centroids,metric=euclidean){
  function(x)
    1:base::nrow(centroids) |>
    sapply(function(k) metric(x,base::unlist(centroids[k,]))) |>
    base::which.min()
}


#' Calculate the inner inequality of the clusters, also called cost
#'
#' The lower the number, the heuristically better the clustering
#'
#'
#' @param data,        a tibble with with every row representing a data point.
#' @param clustering,  a clustering function relating any data point to their cluster.
#' @param metric,      a metric whose 2 inputs are of the data row type.
#'
#' @returns numeric, a real number > 0.
#'
innerInequality <- function(data,clustering,metric=euclidean){

  clustered_data <- data |>
    dplyr::rowwise() |>
    dplyr::mutate(cluster=clustering(dplyr::c_across(dplyr::everything())))

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


#' Silhouette of a clustering on ONE SPECIFIC data point.
#'
#' Gives insight as to how well the data point fits into its assigned cluster.
#' A value near -1 means bad fit, a value near 1 means good fit.
#'
#' Be aware: Trivial cases outputs have been chosen arbitrarily/heuristically
#'
#'
#' @param data        a tibble with with every row representing a data point.
#' @param clustering  a clustering function relating any data point to their cluster.
#' @param o           an atomic vector or tibble row.
#'              This is the data point we calculate the silhouette of
#' @param metric      a metric whose 2 inputs are of the data row type.
#'
#' @returns numeric, a real number between -1 and 1
#'
silhouette <- function(data,clustering,o,metric=euclidean){
  ## if o is not part of data, append it. This is a surprise tool that might help us later
  data[base::nrow(data)+1,] <- o |>
    matrix(nrow=1) |>
    tibble::as_tibble(.name_repair = make.names)
  data <- dplyr::distinct(data)

  ## apply the clustering function to the data
  clustered_data <- data |>
    dplyr::rowwise() |>
    dplyr::mutate(cluster = clustering(dplyr::c_across(dplyr::everything()))) |>
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
    dplyr::arrange(distance) |>
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
#' @param metric    A character. One of \code{'euclidean'}, \code{'maximum'},
#'   \code{'Lp'} or \code{'manhattan'}.
#' @param p         A numeric greater than or equal to 1. If \code{metric} was
#'   chosen to be \code{'Lp'}, this will be used as the p of the p-Metric.
#' @param custom_metric A semi definite and symmetric function whose inputs are
#'   two of the \code{data} row type.
#'
#' @returns a numeric, a real number between -1 and 1
#'
meanSilhouette <- function(data,clustering,metric='euclidean',p=NULL,custom_metric=NULL){



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

  if( data |>
      dplyr::rowwise() |>
      dplyr::mutate(cluster = clustering(dplyr::c_across(dplyr::everything()))) |>
      dplyr::ungroup() |>
      dplyr::summarize(.by = cluster) |>
      base::nrow() == 1){
    ## If there's only one cluster, we cut the calculation short and return 0.
    ## Note that this is an ARBITRARY choice, but seems reasonable as it gives no info
    ## about whether the clustering with 1 cluster is good ore bad
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
#' @param metric    A metric whose inputs are of the data row type
#'
#' @returns a dissimilarity matrix, a row and coloumn for every data point
#'
dissimilarityMatrix <-function(data,metric){
  basis <- array(base::rep(1:base::nrow(data),base::nrow(data)), dim=c(base::nrow(data),base::nrow(data) ))
  M <- array(dim = c(base::nrow(data),base::nrow(data),2 ))
  M[,,1] <- basis
  M[,,2] <- t(basis)

  return(apply(M,c(1,2),function(x) metric(data[x[1],],data[x[2],])))
}

#' The Inequality of a vector to the data
#'
#' Sums the distances of a vector to all pints in the data set.
#'
#' @param data      a tibble with with every row representing a data point.
#' @param vector    A vector (i.e. tibble row or atomic vector)
#' @param metric    A metric whose inputs are of the data row type
#'
#' @returns a numeric >= 0
#'
sumOfDistancestTo <- function(data,vector,metric){
  data |> dplyr::rowwise() |> dplyr::mutate(distance = metric(dplyr::c_across(all_of(1:ncol(data))) , vector)) |>
    dplyr::ungroup() |>
    dplyr::summarise(sum = sum(distance)) |>
    base::unlist(use.names = FALSE)
}

















NULL
