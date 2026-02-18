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
inner_inequality <- function(data,clustering,metric){
  
  ## if not specified, use euclidean metric
  if(base::missing(metric)){metric <- function(x,y){base::sum((x-y)^2)}}
  
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


silhouette <- function(data,clustering,o,metric=NULL){
  ## if o is not part of data, append it. This is a suprise tool that might hel us later
  data[base::nrow(data)+1,] <- o |> 
    matrix(nrow=1) |> 
    tibble::as_tibble(.name_repair = make.names)
  data <- dplyr::distinct(data)

  ## if not specified, use euclidean metric
  if(base::missing(metric)){metric <- function(x,y){base::sum((x-y)^2)}}
  
  ## apply the clustering function to the data
  clustered_data <- data |> 
    dplyr::rowwise() |> 
    dplyr::mutate(cluster=clustering(dplyr::c_across(dplyr::all_of(1:2)))) |>
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



meanSilhouette <- function(data,clustering,metric=NULL){
  ## if not specified, use euclidean metric
  if(base::missing(metric)){metric <- function(x,y){base::sum((x-y)^2)}}
  
  if( data |> dplyr::rowwise() |> dplyr::mutate(cluster = clustering(dplyr::c_across(dplyr::everything()))) |> dplyr::ungroup() |> dplyr::summarize(.by = cluster) |> base::nrow() == 1){
    ## If there's only one cluster, we cut the calculation short and return 0.
    ## Note that this is an ARBITRARY choice, but seems reasonable as it gives no info
    ## about wether the clustering with 1 cluster is good ore bad
    return(0)
  }
  
  ## Calculate the silhouette for every point and return their mean
  return( data |> 
    dplyr::rowwise() |> 
    dplyr::mutate(silhouette=silhouette(data,clustering,dplyr::c_across(all_of(1:base::ncol(data))),metric))|> 
    dplyr::ungroup() |> 
    dplyr::summarize(mean(silhouette)) |>
    base::unlist(use.names=FALSE)
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










################# Viewers ######################

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

################# metric

euclidean <- function(x,y) base::sum((x-y)^2)


clusteringFromCentroids<- function(centroids){
  function(x) 
    1:base::nrow(centroids) |>
    sapply(function(k) metric(x,base::unlist(centroids[k,]))) |>
    base::which.min()
}




