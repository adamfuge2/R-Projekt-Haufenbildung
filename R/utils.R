############ Test Data Generators ######################

#' spherical test data generator
#'
#' @param n           a positive integer. The number of total data points to be generated.
#' @param n_clusters  a positive integer. The number of clusters to generate.
#'                    If none given, choose a random amount < sqrt(n).
#'
#' @returns a tibble, every row representing a data point.
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
#' Returns:
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
#' @param metric      a metric whose 2 inputs are of the data row type.
#'
#' @returns a numeric, a real number between -1 and 1
#'
meanSilhouette <- function(data,clustering,metric=euclidean){

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










################# Viewers ######################


viewClusters2D <-function(data,clustering){
  stopifnot('viewClusters2D can only display clusterings of 2D data' = 1<=dim & dim <= 3)

  colnames(data) <- c('X','Y')

  ## apply the cluster function to the data
  clustered_data <- data |> dplyr::rowwise() |> dplyr::mutate(cluster=clustering(dplyr::c_across(all_of(1:2))), cluster_label = clusterLabeling(cluster))

  ## Defer the number of clusters
  K <- clustered_data |> dplyr::filter(cluster!=0) |> dplyr::distinct(cluster) |> base::nrow()

  ## Display data as 2D scatter plot
  ggplot2::ggplot(clustered_data,ggplot2::aes(x=X,y=Y,colour = cluster_label)) +
    ggplot2::geom_point() +
    ggplot2::scale_color_manual(values = c("Outlier" = "black", setNames(grDevices::rainbow(K),paste0('Cluster ',1:K)))) +
    ggplot2::coord_fixed()
}




viewClusters3D <-function(data,clustering){
  stopifnot('viewClusters3D can only display clusterings of 3D data' = ncol(data)==3)

  colnames(data) <- c('X','Y','Z')

  ## apply the cluster function to the data
  clustered_data <- data |> dplyr::rowwise() |> dplyr::mutate(cluster=clustering(dplyr::c_across(all_of(1:3))), cluster_label = clusterLabeling(cluster))

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

#' View clustered 2D data
#'
#' acts as a wrapper for view_data() in the case of unclustered data
#'
#' @param data        a tibble with every row representing a data point.
#' @param clustering  a clustering function applicable to the data.
#'    If none given, the data will be displayed without clusters, wraps view_data().
viewClusters <- function(data,clustering=NULL){
  dim <- base::ncol(data)
  ## Invariant
  stopifnot('This function can only display clusterings of 1D to 3D data' = 1<=dim & dim<=3)


  if(base::missing(clustering)) viewData(data)
  else{
    if(ncol(data)==1){
      message('this functionality is still work in progress')
      data |> colnames() <- 'X'
      data <- data |> dplyr::mutate(Y = 0)
      viewClusters2D(data,clustering)
    }
    else if(ncol(data)==2){
      viewClusters2D(data,clustering)
    }
    else if(ncol(data)==3){
      viewClusters3D(data,clustering)
    }
  }
}


#' View 2D data as a simple scatter plot
#'
#' Inputs:
#' @param data        a tibble with every row representing a data point.
viewData2D <- function(data){
  colnames(data) <- c('X','Y')

  ggplot2::ggplot(data,ggplot2::aes(x=X,y=Y)) +
    ggplot2::geom_point()
}

viewData3D <- function(data){
  colnames(data) <- c('X','Y','Z')

  scatterplot3d::scatterplot3d(data,color = 'black')
}

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



#' The standard euclidean distance
#'
#' @param x         an atomic vector or tibble row with only real numbers
#' @param y         an atomic vector or tibble row with only real numbers
#'
#' @returns numeric,   a real number >= 0.
euclidean <- function(x,y) sqrt(base::sum((x-y)^2))


#' The standard Manhattan, taxi or maximum metric
#'
#' @param x         an atomic vector or tibble row with only real numbers
#' @param y         an atomic vector or tibble row with only real numbers
#'
#' @returns numeric,   a real number >= 0.
maximumMetric <- function(x,y) base::max(x-y)


#' The standard Lp metric
#'
#'
#' @param p         a real numeric with 1 <= p < Inf
#'
#'
#' @returns a metric (function) with inputs \code{x,y} numerical vectors and a
#'   numerical output, a real number >= 0.
pMetric <- function(p) function(x,y) base::sum(base::abs(x-y)^p)^(2/p)


NULL
