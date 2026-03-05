############ Test Data Generators ######################

#' spherical test data generator
#'
#' @param n           a positive integer. The number of total data points to be generated.
#' @param n_clusters  a positive integer. The number of clusters to generate.
#'                    If none given, choose a random amount < sqrt(n).
#'
#' @returns a tibble, every row representing a data point.
#' @export
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

#' @export
generateClusterTestDataSimple <- function(n=100,cluster_amount=NULL,dim=2){
  if(base::missing(cluster_amount)){
    cluster_amount <- base::floor(stats::runif(1,min = 1, max = 2*base::sqrt(n)))
  }

  cluster_centers <- tibble::tibble(variances=stats::runif(cluster_amount, min=0.001, max=0.1))

  for(i in 1:dim){
    cluster_centers <- cluster_centers |> tibble::add_column(!!paste0('X_',i) := stats::runif(cluster_amount, min=0, max=1))
  }

  test_data <- tibble::tibble(selected_clusters = base::floor(stats::runif(n,1,cluster_amount+1)), variances = cluster_centers$variances[selected_clusters] ) |> dplyr::rowwise()

  for(i in 1:dim){
    test_data <- test_data |> dplyr::mutate(!!paste0('X_',i) := stats::rnorm(1,cluster_centers[[selected_clusters,i+1]],variances))
  }

  test_data <- test_data |> dplyr::select(c(-1,-2)) |> dplyr::ungroup()

  return(test_data)
}




#' Nonspherical test data generator
#'
#' @param n               a positive integer. The number of total data points to be generated.
#' @param list_of_paths   a list containing tibbles. Each tibble containing points (in rows)
#'                  to be interpreted as paths along which the data is generated
#'
#' @returns a tibble, every row representing a data point.
#' @export
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
silhouette <- function(data,clustering,o,metric=euclidean,is_part_of_data=TRUE){
  ## if o is part of data, remove it
  if(is_part_of_data) data <- data |> dplyr::filter(duplicated(data) | !apply(data,1,function(row) all(row==o)) )

  ## apply the clustering function to the data
  clusters <- apply(data,1,function(data_point) clustering(data_point))
  distances <- apply(data,1,function(data_point) metric(data_point,o))
  clustered_data <- dplyr::mutate(data,cluster = clusters, distance = distances)

  #
  cluster_of_o <- f(o)

  ## Return zero, if o is the only data point in its cluster,
  ## else we would have a devide by zero error later.
  ## The choice 0 is ARBITRARY, but as the silhouette is bounded by -1 and 1 this
  ## choice means monoelemental clusterings are
  ## more encouraged than wrong clusterings (which have a negative silhouettecoefficient)
  ## and less encouraged than good natural clusterings (which have a slihouettecoefficient close to 1)
  if( is_part_of_data && length(clusters[clusters==cluster_of_o]) == 1 ){
    return(0)
  }

  cluster_ids <- unique(clusters[clusters!=0])

  mean_of_distances <- cluster_ids |> sapply(function(k) mean(distances[clusters == k]))

  a_of_o <- mean_of_distances[cluster_of_o]
  b_of_o <- mean_of_distances[-cluster_of_o] |> min()

  ## return the so called silhouette
  return( (b_of_o - a_of_o)/max(b_of_o, a_of_o) )
}

silhouette_faster <- function(clustered_data,index_of_o,dissimilarity_matrix){

  o <- clustered_data[index_of_o,-ncol(clustered_data)]
  cluster_of_o <- clustered_data[index_of_o,ncol(clustered_data)] |> unlist(use.names = FALSE)



  ## We assume o is part of data. We need to remove it
  clustered_data <- clustered_data[-index_of_o,]



  ## apply the clustering function to the data
  clusters <- clustered_data$cluster


  distances_to_o <- D[index_of_o,-index_of_o]



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
#' @param metric      a metric whose 2 inputs are of the data row type.
#'
#' @returns a numeric, a real number between -1 and 1
#'
meanSilhouette <- function(data,clustering,metric='euclidean',custom_metric=NULL){



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
            apply(1,function(data_point) silhouette(data,clustering,data_point,metric)) |>
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










################# Viewers ######################


#' View 2D cluster data
#'
#' Displays 2D data points color coded by their cluster. Can be used for:
#' \itemize{
#'    \item \strong{data without clusters}, in this case don't input a clustering function and make sure your data \strong{does not} have a column \code{'cluster'}.
#' }Currently only works for clusters represented by numbers >=
#' 0. Every data point in 'cluster' will be labeled as an 'Outlier'.
#'
#' @param clusterd_data   A tibble with every row representing a data point.
#'   Must contain a column called 'cluster' in which the cluster belonging to
#'   the respective data point is stored.
#'
#' @examples
#' data_2D <- generateClusterTestDataSimple(dim=2,cluster_amount=5)
#' clustering <- K_means(data_2D,K=5)
#' clustered_data_2D <- data_2D |> dplyr::rowwise() |> dplyr::mutate(cluster=clustering(dplyr::c_across(everything())))
#' viewClusters2D(clustered_data_2D)
#'
viewClusters2D <-function(clustered_data){

  dim <- ncol(clustered_data)-1
  stopifnot('viewClusters2D can only display clusterings of 2D data' = dim==2)

  prefered_colnames <- c('X','Y')
  colname_index <- 1

  for(i in 1:ncol(clustered_data)){
    if(colnames(clustered_data)[i] != 'cluster'){
      colnames(clustered_data)[i] <- prefered_colnames[colname_index]
      colname_index <- colname_index+1
    }
  }


  ## label the clusters
  clustered_data <- clustered_data |> dplyr::rowwise() |> dplyr::mutate(cluster_label = clusterLabeling(cluster))

  ## Defer the number of clusters
  K <- clustered_data |> dplyr::filter(cluster!=0) |> dplyr::distinct(cluster) |> base::nrow()

  ## Display data as 2D scatter plot
  ggplot2::ggplot(clustered_data,ggplot2::aes(x=X,y=Y,colour = cluster_label)) +
    ggplot2::geom_point() +
    ggplot2::scale_color_manual(values = c("Outlier" = "black", setNames(grDevices::rainbow(K),paste0('Cluster ',1:K)))) +
    ggplot2::coord_fixed()
}




#' View 3D cluster data
#'
#' Displays 3D data points color coded by their cluster. Can be used for:
#' \itemize{
#'    \item \strong{data without clusters}, in this case don't input a clustering function and make sure your data \strong{does not} have a column \code{'cluster'}.
#' }Currently only works for clusters represented by numbers >=
#' 0. Every data point in 'cluster' will be labeled as an 'Outlier'.
#'
#' @param clusterd_data   A tibble with every row representing a data point.
#'   Must contain a column called 'cluster' in which the cluster belonging to
#'   the respective data point is stored.
#'
#' @examples
#' data_3D <- generateClusterTestDataSimple(dim=3,cluster_amount=5)
#' clustering <- K_means(data_3D,K=5)
#' clustered_data_3D <- data_3D |> dplyr::rowwise() |> dplyr::mutate(cluster=clustering(dplyr::c_across(everything())))
#' viewClusters3D(clustered_data_3D)
#'
viewClusters3D <-function(clustered_data){
  dim <- ncol(clustered_data)-1
  stopifnot('clustered_data must have a row called \'cluster\''= 'cluster'%in%colnames(clustered_data))
  stopifnot('viewClusters3D can only display clusterings of 3D data' = dim==3)



  prefered_colnames <- c('X','Y','Z')
  colname_index <- 1

  for(i in 1:ncol(clustered_data)){
    if(colnames(clustered_data)[i] != 'cluster'){
      colnames(clustered_data)[i] <- prefered_colnames[colname_index]
      colname_index <- colname_index+1
    }
  }


  ## label the clusters
  clustered_data <- clustered_data |> dplyr::rowwise() |> dplyr::mutate(cluster_label = clusterLabeling(cluster))


  ## Defer the number of clusters
  K <- clustered_data |> dplyr::filter(cluster!=0) |> dplyr::distinct(cluster) |> base::nrow()

  clustered_data <- clustered_data |> dplyr::mutate(color = c("Outlier" = "black", setNames(grDevices::rainbow(K),paste0('Cluster ',1:K)))[cluster_label])

  x<- clustered_data$X
  y<- clustered_data $Y
  z<- clustered_data $Z
  color <- clustered_data$color

  ## Display data as 2D scatter plot
  scatterplot3d::scatterplot3d(x,y,z,color = color,pch = 16)
}

#' View cluster data
#'
#' Displays 1D to 3D data points color coded by their cluster. Can be used for:
#' \itemize{
#'  \item \strong{data and an applicable clustering function}.
#'  \item \strong{already clustered data},  in this case don't input a clustering function and make sure your data has a column \code{'cluster'}.
#'  \item \strong{data without clusters}, in this case don't input a clustering function and make sure your data \strong{does not} have a column \code{'cluster'}.
#' }
#' Acts as a wrapper for viewClusters2D, viewClusters3D, viewData, viewData2D
#' and viewData3D.\n
#' Currently only works for clusters represented by numbers >= 0. Every data point
#' in 'cluster' will be labeled as an 'Outlier'.
#'
#' @param data        a tibble with every row representing a data point. If \code{data}
#'   has a column named \code{'cluster'}, then this column will be used to color
#'   the data points
#' @param clustering  a clustering function applicable to the data. If none
#'   given, the data will be displayed with the clusters deferred from the
#'   column \code{'cluster'} of data or without clusters, using
#'   \code{viewData(data)}.
#'
#' @examples
#' # 1D data without clusters
#' data_1D <- generateClusterTestDataSimple(dim=1)
#' viewClusters(data_1D)
#' # 2D data without clusters
#' data_2D <- generateClusterTestDataSimple(dim=2)
#' viewClusters(data_2D)
#' # 3D data without clusters
#' data_3D <- generateClusterTestDataSimple(dim=3)
#' viewClusters(data_3D)
#' # 3D data with a clustering function
#' clustering <- K_means(data_3D,K=5)
#' viewClusters(data_3D,clustering)
#' # 2D data with column 'cluster'
#' clustering <- K_means(data_2D,K=5)
#' clustered_data_2D <- data_2D |> dplyr::rowwise() |> dplyr::mutate(cluster=clustering(dplyr::c_across(everything())))
#' viewClusters(clustered_data_2D)
#'
#'
#' @export
viewClusters <- function(data,clustering=NULL){


  if(base::missing(clustering) & !('cluster' %in% colnames(data))) {
    viewData(data)
  }
  else{
    if(!('cluster' %in% colnames(data))){
      data <- data |> dplyr::rowwise() |> dplyr::mutate(cluster=clustering(dplyr::c_across(everything())))
    }

    dim <- base::ncol(data)-1
    ## Invariant
    stopifnot('This function can only display clusterings of 1D to 3D data' = 1<=dim & dim<=3)



    if(ncol(data)-1==1){

      message('this functionality is still work in progress')
      if(colnames(data)[1] == 'cluster') data |> colnames() <- c('X','cluster')
      else data |> colnames() <- c('cluster','X')
      data <- data |> dplyr::mutate(Y = 0)
      viewClusters2D(data)
    }
    else if(ncol(data)-1==2){
      viewClusters2D(data)
    }
    else if(ncol(data)-1==3){
      viewClusters3D(data)
    }
  }
}


#' View 2D data as scatter plot
#'
#' @param data   A tibble with every row representing a data point.
#'
#' @examples
#' data_3D <- generateClusterTestDataSimple(dim=3,cluster_amount=5)
#' viewData3D(data_3D)
#'
viewData2D <- function(data){
  colnames(data) <- c('X','Y')

  ggplot2::ggplot(data,ggplot2::aes(x=X,y=Y)) +
    ggplot2::geom_point()
}


#' View 3D data as scatter plot
#'
#' @param data   A tibble with every row representing a data point.
#'
#' @examples
#' data_2D <- generateClusterTestDataSimple(dim=2,cluster_amount=5)
#' viewData2D(data_2D)
#'
viewData3D <- function(data){

  x <- data[[1]]
  y <- data[[2]]
  z <- data[[3]]

  scatterplot3d::scatterplot3d(x,y,z,color = 'black')
}

#' View data as scatter plot
#'
#' Displays 1D to 3D data points color.\n
#' Acts as a wrapper for viewData2D and viewData3D.
#'
#' @param data   A tibble with every row representing a data point.
#'
#' @examples
#' # 1D data without clusters
#' data_1D <- generateClusterTestDataSimple(dim=1)
#' viewData(data_1D)
#' # 2D data without clusters
#' data_2D <- generateClusterTestDataSimple(dim=2)
#' viewData(data_2D)
#' # 3D data without clusters
#' data_3D <- generateClusterTestDataSimple(dim=3)
#' viewData(data_3D)
#'
#' @export
viewData <- function(data){
  dim <- base::ncol(data)
  stopifnot('viewData can only display 1D to 3D data' = (1 <= dim && dim <= 3))

  if(dim==1){
    message('this functionality is still work in progress')
    data |> colnames() <- 'X'
    data <- data |> dplyr::mutate(Y = 0)
    viewData2D(data)
  }
  else if(dim==2){
    viewData2D(data)
  }
  else if(dim==3){
    viewData3D(data)
  }
}



#' Derive cluster label from cluster number
#'
#' Helper function.\n
#' Converts an identification (generally integers) of a cluster to a more descriptive cluster label.
#' The 0th cluster gets labeled as the Outliers cluster.
#'
#' @param n A clusters identification. Can be a numeric or character.
#'
#' @returns A character like \code{'Cluster 15'}, or \code{'Outlier'} in case n is zero.
#'
#' @examples
#' # example code
#' clusterLabeling(36)
#' clusterLabeling('siebzehn')
#' clusterLabeling(0)
#' clusterLabeling('0')
#'
#'
clusterLabeling <- function(n){
  if(is.na(n) || n==0 || n=='0' || is.null(n)) return('Outlier')
  return(paste0('Cluster ',n))
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



#' The standard euclidean distance
#'
#' @param x         an atomic vector or tibble row with only real numbers
#' @param y         an atomic vector or tibble row with only real numbers
#'
#' @returns numeric,   a real number >= 0.
#' @export
euclidean <- function(x,y) sqrt(base::sum((x-y)^2))


#' The standard Manhattan, taxi or maximum metric
#'
#' @param x         an atomic vector or tibble row with only real numbers
#' @param y         an atomic vector or tibble row with only real numbers
#'
#' @returns numeric,   a real number >= 0.
#' @export
maximumMetric <- function(x,y) base::max(base::abs(x-y))



#' The standard Lp metric
#'
#'
#' @param p         a real numeric with 1 <= p < Inf
#'
#'
#' @returns a metric (function) with inputs \code{x,y} numerical vectors and a
#'   numerical output, a real number >= 0.
#' @export
pMetric <- function(p) function(x,y) base::sum(base::abs(x-y)^p)^(1/p)


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
centroid <- function(metric, data_1, data_2){
  centr_1 <- centroid_det(data_1)
  centr_2 <- centroid_det(data_2)
  return(metric(centr_1, centr_2))
}



#' Linkage mode: average
#'
#' Mean intercluster dissimilarity. The average of the distances of all
#' combinations between a point in cluster 1 and a point in cluster 2 given
#' a metric
#'
#' @param metric  metric function (euclidean, maximumMetric...)
#' @param data_1   a tibble with n(>=1) rows of dimension d
#' @param data_2   a tibble with n(>=1) rows of dimension d
#'
#' @returns a real number (numeric) >= 0
#' @export
average <- function(metric, data_1, data_2){   #employ the average distance method
  data_1 |>
    dplyr::rowwise() |>
    dplyr::mutate(
      mean_dist = mean(
        sapply(1:nrow(data_2), function(j){
          metric(data_1[dplyr::cur_group_rows(), ], data_2[j, ])
        })
      )
    ) |>
    dplyr::select(mean_dist) |>
    dplyr::ungroup() |>
    dplyr::summarise(mean = mean(.data$mean_dist)) |>
    dplyr::pull(mean)
}



#' Linkage mode: single
#'
#' Minimal intercluster dissimilarity. Determines the smallest distance between
#' points in cluster and 1 and points in cluster 2 given a metric.
#'
#' @param metric  metric function (euclidean, maximumMetric...)
#' @param data_1   a tibble with n(>=1) rows of dimension d
#' @param data_2   a tibble with n(>=1) rows of dimension d
#'
#' @returns a real number (numeric) >= 0
#' @export
single <- function(metric, data_1, data_2){
  data_1 |>
    dplyr::rowwise() |>
    dplyr::mutate(
      min_dist = min(
        sapply(1:nrow(data_2), function(j){
          metric(data_1[dplyr::cur_group_rows(), ], data_2[j, ])
        })
      )
    ) |>
    dplyr::select(min_dist) |>
    dplyr::ungroup() |>
    dplyr::summarise(min = min(.data$min_dist)) |>
    dplyr::pull(min)
}



#' Linkage mode: complete
#'
#' Maximum intercluster dissimilarity. Determines the largest distance
#' between points in cluster 1 and points in cluster 2 given a metric.
#'
#' @param metric  metric function (euclidean, maximumMetric...)
#' @param data_1   a tibble with n(>=1) rows of dimension d
#' @param data_2   a tibble with n(>=1) rows of dimension d
#'
#' @returns a real number (numeric) >= 0
#' @export
complete <- function(metric, data_1, data_2){
  data_1 |>
    dplyr::rowwise() |>
    dplyr::mutate(
      max_dist = max(
        sapply(1:nrow(data_2), function(j){
          metric(data_1[dplyr::cur_group_rows(), ], data_2[j, ])
        })
      )
    ) |>
    dplyr::select(max_dist) |>
    dplyr::ungroup() |>
    dplyr::summarise(max = max(.data$max_dist)) |>
    dplyr::pull(max)
}


NULL
