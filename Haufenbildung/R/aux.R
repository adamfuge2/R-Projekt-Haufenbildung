
############# Test Data Generators ######################

## spherical test data generator
generateClusterTestDataSimple2D = function(n=10,nclusters=NULL){
  if(base::missing(nclusters)){
    nclusters <- base::floor(stats::runif(1,min = 1, max = 2*base::sqrt(n)))
  }
  
  x_clusters <- stats::runif(nclusters, min=0, max=1)
  y_clusters <- stats::runif(nclusters, min=0, max=1)
  sd_clusters <- stats::runif(nclusters, min=0.001, max=0.1)
  
  
  
  testdata = tibble::tibble(selected_clusters = base::floor(stats::runif(n,1,nclusters+1))) |>
    dplyr::rowwise() |>
    dplyr::mutate(X = stats::rnorm(1,x_clusters[selected_clusters],sd_clusters[selected_clusters]),Y = stats::rnorm(1,y_clusters[selected_clusters],sd_clusters[selected_clusters])) |>
    dplyr::select(X,Y) |>
    dplyr::ungroup()
  
  
  return(testdata)
}


## nonspherical test data generator
generateClusterTestData2DFromPaths = function(n=100,list_of_paths){
  
  n_clusters <- length(list_of_paths)
  paths <-lapply(list_of_paths,tibble.as.simplepath)
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

## Used to test if K is a whole number
is.wholenumber <- function(x, tol = base::.Machine$double.eps^0.5)  base::abs(x - base::round(x)) < tol


## Calculate the inner inequality of the clusters, also called cost
## The lower the number, the 'better' the clustering
inner_inequality <- function(data,clustering,D){
  
  ## if not specified, use euclidean metric
  if(base::missing(D)){D <- function(x,y){base::sum((x-y)^2)}}
  
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
      dplyr::mutate(distances = D(dplyr::c_across(1:base::ncol(data)), centroids[cluster,])) |>
      dplyr::ungroup() |>
      dplyr::summarise(base::sum(distances)) |>
      unlist(use.names=FALSE)
  )
}

## Turn a tibble into a mathematical path
tibble.as.simplepath <- function(data){
  return(
    function(t){
      stopifnot('A path maps t in [0,1] to points in R^dim space' = 0<=t && t<=1)
      return( (t*(nrow(data)-1) - base::floor(t*(nrow(data)-1))) * data[1+base::floor(t*(base::nrow(data)-1)),] +
                (1-(t*(nrow(data)-1) - base::floor(t*(nrow(data)-1))))*data[1+base::ceiling(t*(base::nrow(data)-1)),])
    }
  )
}










################# Vizualisation ######################

## View clustered 2D data
## Inputs:
## data,        a tibble with rows representing data points
## clustering,  a clustering function applicable to the data
view_clusters <- function(data,clustering=NULL){
  ## Invariant
  stopifnot('This function can currently only display clusterings of 2D data' = ncol(data)==2)
  
  if(base::missing(clustering)) view_data(data)
  else{
    
    ## apply the cluster function to the data
    clustered_data <- data |> dplyr::rowwise() |> dplyr::mutate(cluster=clustering(dplyr::c_across(all_of(1:2))))
    
    ## Defer the number of clusters
    K <- dplyr::n_distinct(clustered_data$cluster)
    
    ## Display data as 2D scatter plot
    ggplot2::ggplot(clustered_data,ggplot2::aes(x=clustered_data[[1]],y=clustered_data[[2]],colour = cluster)) + 
      ggplot2::geom_point() + 
      ggplot2::scale_color_gradient2(midpoint = K/2+1/2, low="darkblue", mid="green",high="red", space ="Lab" ) +
      ggplot2::coord_fixed()
  }
}

## simple scatter plot of data
view_data <- function(data){
  ggplot2::ggplot(data,ggplot2::aes(x=data[[1]],y=data[[2]])) + 
    ggplot2::geom_point() 
}




data <- generateClusterTestDataSimple2D(n=100)
view_clusters(datam,function(x))
