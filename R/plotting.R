################# Cluster Viewers ######################


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
  print(ggplot2::ggplot(clustered_data,ggplot2::aes(x=X,y=Y,colour = cluster_label)) +
          ggplot2::geom_point() +
          ggplot2::scale_color_manual(values = c("Outlier" = "black", setNames(grDevices::rainbow(K),paste0('Cluster ',1:K)))) +
          ggplot2::coord_fixed()
  )
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
  print(scatterplot3d::scatterplot3d(x,y,z,color = color,pch = 16))
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
#' Displays 1D to 3D data points color.
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
#' Helper function.
#' Converts an identification (generally integers) of a cluster to a more descriptive cluster label.
#' The 0th cluster gets labeled as the Outliers cluster.
#'
#' @param n A clusters identification. Can be a numeric or character.
#'
#' @returns A character like \code{'Cluster 15'}, or \code{'Outlier'} in case n is zero.
#'
#' @examples
#' # example code
#' clusterfuck::clusterLabeling(36)
#' clusterfuck::clusterLabeling('siebzehn')
#' clusterfuck::clusterLabeling(0)
#' clusterfuck::clusterLabeling('0')
#'
#'
clusterLabeling <- function(n){
  if(is.na(n) || n==0 || n=='0' || is.null(n)) return('Outlier')
  return(paste0('Cluster ',n))
}


#################### as method for clustering class ######################

#' @export
print.clustering <- function(x, ...){
  viewClusters(x$clustered_data)
}






