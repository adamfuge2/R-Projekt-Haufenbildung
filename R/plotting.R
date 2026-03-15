################# Cluster Viewers ######################

#' View 1D cluster data
#'
#' Displays 1D data points color coded by their cluster. Can be used for:
#' \itemize{
#'    \item \strong{data without clusters}, in this case don't input a clustering function and make sure your data \strong{does not} have a column \code{'cluster'}.
#' }Currently only works for clusters represented by numbers >=
#' 0. Every data point in 'cluster' will be labeled as an 'Outlier'.
#'
#' @param clustered_data   A tibble with every row representing a data point.
#'   Must contain a column called 'cluster' in which the cluster belonging to
#'   the respective data point is stored.
#'
#' @examples
#' data_1D <- generateClusterData(dim=1,cluster_amount=3)
#' clustered_data_1D <- kMeans(data_1D,K=3)$clustered_data
#' clusterfuck:::viewClusters1D(clustered_data_1D)
#'
viewClusters1D <-function(clustered_data){

  dim <- ncol(clustered_data)-1
  stopifnot('viewClusters1D can only display clusterings of 1D data' = dim==1)

  prefered_colnames <- c('Y')
  colname_index <- 1

  for(i in 1:ncol(clustered_data)){
    if(colnames(clustered_data)[i] != 'cluster'){
      colnames(clustered_data)[i] <- prefered_colnames[colname_index]
      colname_index <- colname_index+1
    }
  }


  ## label the clusters
  clustered_data <- clustered_data |> dplyr::rowwise() |> dplyr::mutate('cluster_label' = clusterLabeling(.data$cluster))

  ## Defer the number of clusters
  K <- clustered_data |> dplyr::filter(.data$cluster!=0) |> dplyr::distinct(.data$cluster) |> base::nrow()

  clusternames <- paste0('Cluster ',unique(clustered_data$cluster[clustered_data$cluster!=0]))
  if(K==0) clusternames <- character()

  ## Display data as 2D scatter plot
  ggplot2::ggplot(clustered_data,ggplot2::aes(y=.data$Y, x=row(clustered_data)[,1] ,colour = .data$cluster_label)) +
            ggplot2::geom_point() +
            ggplot2::scale_color_manual(values = c("Noise" = "black", stats::setNames(grDevices::rainbow(K),clusternames))) +
            ggplot2::labs(x='Data Points',x='Value')
}


#' View 2D cluster data
#'
#' Displays 2D data points color coded by their cluster. Can be used for:
#' \itemize{
#'    \item \strong{data without clusters}, in this case don't input a clustering function and make sure your data \strong{does not} have a column \code{'cluster'}.
#' }Currently only works for clusters represented by numbers >=
#' 0. Every data point in 'cluster' will be labeled as an 'Outlier'.
#'
#' @param clustered_data   A tibble with every row representing a data point.
#'   Must contain a column called 'cluster' in which the cluster belonging to
#'   the respective data point is stored.
#'
#' @examples
#' data_2D <- generateClusterData(dim=2,cluster_amount=5)
#' clustered_data_2D <- kMeans(data_2D,K=5)$clustered_data
#' clusterfuck:::viewClusters2D(clustered_data_2D)
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
  clustered_data <- clustered_data |> dplyr::rowwise() |> dplyr::mutate('cluster_label' = clusterLabeling(.data$cluster))

  ## Defer the number of clusters
  K <- clustered_data |> dplyr::filter(.data$cluster!=0) |> dplyr::distinct(.data$cluster) |> base::nrow()


  clusternames <- paste0('Cluster ',unique(clustered_data$cluster[clustered_data$cluster!=0]))
  if(K==0) clusternames <- character()

  ## Display data as 2D scatter plot
  ggplot2::ggplot(clustered_data,ggplot2::aes(x=.data$X,y=.data$Y,colour = .data$cluster_label)) +
    ggplot2::geom_point() +
    ggplot2::scale_color_manual(values = c("Noise" = "black", stats::setNames(grDevices::rainbow(K),clusternames)))
}




#' View 3D cluster data
#'
#' Displays 3D data points color coded by their cluster. Can be used for:
#' \itemize{
#'    \item \strong{data without clusters}, in this case don't input a clustering function and make sure your data \strong{does not} have a column \code{'cluster'}.
#' }Currently only works for clusters represented by numbers >=
#' 0. Every data point in 'cluster' will be labeled as an 'Outlier'.
#'
#' @param clustered_data   A tibble with every row representing a data point.
#'   Must contain a column called 'cluster' in which the cluster belonging to
#'   the respective data point is stored.
#'
#' @examples
#' data_3D <- generateClusterData(dim=3,cluster_amount=5)
#' clustered_data_3D <- kMeans(data_3D,K=5)$clustered_data
#' clusterfuck:::viewClusters3D(clustered_data_3D)
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
  clustered_data <- clustered_data |> dplyr::rowwise() |> dplyr::mutate('cluster_label' = clusterLabeling(.data$cluster))


  ## Defer the number of clusters
  K <- clustered_data |> dplyr::filter(.data$cluster!=0) |> dplyr::distinct(.data$cluster) |> base::nrow()

  clustered_data <- clustered_data |> dplyr::mutate('color' = c("Noise" = "black", stats::setNames(grDevices::rainbow(K),paste0('Cluster ',unique(clustered_data$cluster))))[.data$cluster_label])

  x<- clustered_data$X
  y<- clustered_data$Y
  z<- clustered_data$Z
  color <- clustered_data$color

  ## Display data as 3D scatter plot
  scatterplot3d::scatterplot3d(x = x, y = y, z = z, color = color)
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
#' and viewData3D. Currently only works for clusters represented by numbers >=
#' 0. Every data point in 'cluster' will be labeled as an 'Outlier'.
#'
#' @param data        a tibble with every row representing a data point. If
#'   \code{data} has a column named \code{'cluster'}, then this column will be
#'   used to color the data points
#' @param clustering  a clustering function applicable to the data. If none
#'   given, the data will be displayed with the clusters deferred from the
#'   column \code{'cluster'} of data or without clusters, using
#'   \code{viewData(data)}.
#' @param print_directly If TRUE, the plot will be displayed directly instead of returned
#'
#' @examples
#' # 1D data without clusters
#' data_1D <- generateClusterData(dim=1)
#' viewClusters(data_1D)
#' # 2D data without clusters
#' data_2D <- generateClusterData(dim=2)
#' viewClusters(data_2D)
#' # 3D data without clusters
#' data_3D <- generateClusterData(dim=3)
#' viewClusters(data_3D)
#' # 3D data with a clustering function
#' clustered_data <- kMeans(data_3D,K=5)$clustered_data
#' viewClusters(clustered_data)
#' # 2D data with column 'cluster'
#' clustering_function <- kMeans(data_2D,K=5)$clustering_function
#' viewClusters(data_2D,clustering_function)
#'
#'
#' @export
viewClusters <- function(data,clustering=NULL,print_directly=TRUE){


  if(base::missing(clustering) & !('cluster' %in% colnames(data))) {
    viewData(data)
  }
  else{
    if(!('cluster' %in% colnames(data))){
      data <- as.clustered_data(data,clustering_function = clustering)
    }

    dim <- base::ncol(data)-1
    ## Invariant
    stopifnot('This function can only display clusterings of 1D to 3D data' = 1<=dim & dim<=3)



    if(ncol(data)-1==1){
      plot <- viewClusters1D(data)
      if(print_directly)
        print(plot)
      else
        return(plot)
    }
    else if(ncol(data)-1==2){
      plot <- viewClusters2D(data)
      if(print_directly)
        print(plot)
      else
        return(plot)
    }
    else if(ncol(data)-1==3){
      plot <- viewClusters3D(data)
    }


  }
}



#' View 1D data as scatter plot
#'
#' @param data   A tibble with every row representing a data point.
#'
#' @examples
#' data_1D <- generateClusterData(dim=1,cluster_amount=2)
#' clusterfuck:::viewData1D(data_1D)
#'
viewData1D <- function(data){

colnames(data) <- 'Y'

## Display data as 2D scatter plot
print(ggplot2::ggplot(data,ggplot2::aes(y=.data$Y, x=row(data)[,1] )) +
        ggplot2::geom_point() +
        ggplot2::labs(x='Data Points',y='Value'))
}


#' View 2D data as scatter plot
#'
#' @param data   A tibble with every row representing a data point.
#'
#' @examples
#' data_3D <- generateClusterData(dim=3,cluster_amount=5)
#' clusterfuck:::viewData3D(data_3D)
#'
viewData2D <- function(data){
  colnames(data) <- c('X','Y')

  ggplot2::ggplot(data,ggplot2::aes(x=.data$X,y=.data$Y)) +
    ggplot2::geom_point()
}


#' View 3D data as scatter plot
#'
#' @param data   A tibble with every row representing a data point.
#'
#' @examples
#' data_3D <- generateClusterData(dim=3,cluster_amount=5)
#' clusterfuck:::viewData3D(data_3D)
#'
viewData3D <- function(data){

  x <- data[[1]]
  y <- data[[2]]
  z <- data[[3]]

  scatterplot3d::scatterplot3d(x,y,z,color = 'black')
}

#' View data as scatter plot
#'
#' Displays 1D to 3D data points color.
#' Acts as a wrapper for viewData2D and viewData3D.
#'
#' @param data   A tibble with every row representing a data point.
#'
#' @examples
#' # 1D data without clusters
#' data_1D <- generateClusterData(dim=1)
#' viewData(data_1D)
#' # 2D data without clusters
#' data_2D <- generateClusterData(dim=2)
#' viewData(data_2D)
#' # 3D data without clusters
#' data_3D <- generateClusterData(dim=3)
#' viewData(data_3D)
#'
#' @export
viewData <- function(data){
  dim <- base::ncol(data)
  stopifnot('viewData can only display 1D to 3D data' = (1 <= dim && dim <= 3))

  if(dim==1){
    viewData1D(data)
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
#' Helper function.
#' Converts an identification (generally integers) of a cluster to a more descriptive cluster label.
#' The 0th cluster gets labeled as the Noise cluster.
#'
#' @param n A clusters identification. Can be a numeric or character.
#'
#' @returns A character like \code{'Cluster 15'}, or \code{'Noise'} in case n is zero.
#'
#' @examples
#' # example code
#' clusterfuck:::clusterLabeling(36)
#' clusterfuck:::clusterLabeling('siebzehn')
#' clusterfuck:::clusterLabeling(0)
#' clusterfuck:::clusterLabeling('0')
#'
#'
clusterLabeling <- function(n){
  if(is.na(n) || n==0 || n=='0' || is.null(n)) return('Noise')
  return(paste0('Cluster ',n))
}


#################### as method for clustering class ######################

#' @export
print.clustering <- function(x, plot=TRUE,...){
  cat('A clustering object:\n')
  cat(attr(x,'description'),'\n\n')
  names <- attr(x,'names')
  for(i in 1:length(x)){
    cat('\n$',names[[i]],'\n',sep='')
    if('clustered_data' %in% class(x[[i]])){
      print(x[[i]], plot)}
    else{
      print(x[[i]])}
  }
}

#' @export
print.clustered_data <- function(x, plot=TRUE, ...){
  cat('A clustered_data object:\n')
  cat('a tibble with a column called \'cluster\'\n')
  if(ncol(x) <= 4 && ncol(x) >= 2 && plot==TRUE)
    viewClusters(x)

  # doing this, beacause NextMethod was doing WEIRD stuff
  class(x) <- class(x)[2:length(class(x))]
  print(x)
}

#' @export
print.cluster_data <- function(x, ...){
  cat('Data made for clustering:\n')
  NextMethod(x,...)
  if(ncol(x) <= 3 && ncol(x) >= 1)
    viewData(x)
}

#' @export
print.spectral_clustering <- function(x, ...){
  if(!is.null(x$clustered_data) &&
     !is.null(x$projected_clustered_data) &&
     ncol(x$clustered_data) <= 4 &&
     ncol(x$clustered_data) >= 2 &&
     ncol(x$projected_clustered_data) <= 4 &&
     ncol(x$projected_clustered_data) >= 2){

    viewClusters(x$projected_clustered_data)
    viewClusters(x$clustered_data)

    NextMethod(x,plot=FALSE)}
  else
    NextMethod(x)
}

#' @export
clustered_data_validate <- function(x){
  stopifnot('cluster' %in% colnames(x))
}

#' @export
new_clustered_data <- function(data,cluster){
  structure(dplyr::mutate(data,'cluster'=cluster),class=c('clustered_data',class(data)))
}

#' @export
clustered_data <- function(data,cluster=NULL){
  if(is.null(cluster))
    clustered_data <- structure(data,class=c('clustered_data',class(data)))
  else
    clustered_data <- new_clustered_data(data,cluster)
  clustered_data_validate(clustered_data)
  clustered_data
}

