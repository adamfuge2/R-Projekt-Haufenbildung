
## imports
install.packages('tibble')
install.packages('dplyr')
install.packages('ggplot2')
install.packages('rlang')
library(tibble)
library(dplyr)
library(ggplot2)
library(rlang)

## testdata generator
generateClusterTestDataSimple2D = function(n=10,nclusters=NULL){
  if(base::missing(nclusters)){
    nclusters <- floor(runif(1,min = 1, max = 2*sqrt(n)))
  }
  
  x_clusters <- runif(nclusters, min=0, max=1)
  y_clusters <- runif(nclusters, min=0, max=1)
  sd_clusters <- runif(nclusters, min=0.001, max=0.1)
  
  
  
  testdata = tibble(selected_clusters = floor(runif(n,1,nclusters+1))) |>
    rowwise() |>
    mutate(X = rnorm(1,x_clusters[selected_clusters],sd_clusters[selected_clusters]),Y = rnorm(1,y_clusters[selected_clusters],sd_clusters[selected_clusters])) |>
    select(X,Y) |>
    ungroup()
    
  
  return(testdata)
}

## Used to test if K is a whole number
is.wholenumber <- function(x, tol = base::.Machine$double.eps^0.5)  base::abs(x - base::round(x)) < tol
















## definition of k_means algorithm

k_means <- function(data,k,d=NULL,tries=k){
  
  ## if not specified, use euclidean metric
  if(base::missing(d)){d <- function(x,y){base::sqrt(base::sum((x-y)^2))}}
  
  ## some necessary variables
  n <- base::nrow(data)
  dim <- ncol(data)
  minimal_cost <- Inf
  
  
  ## Invariants: test the input
  stopifnot('Data must have more than 0 rows' = n>0)
  stopifnot('k is not a whole number' = is.wholenumber(k))
  stopifnot('k must be between 0 and n+1' = (0 < k && k < n+1))
  
  
  ## Start of actual algorithm
  ## Try 5 times, to minimize the dependency on random chance
  for(repeats in 1:tries){
  
    ## Define starting centroids
    centroids <- data[,1:dim] |> 
                  ungroup() |>
                  slice_sample(n=k)
  
    ## Main loop: repeat iterating the cluster means, until no more change 
    while(!identical(centroids, old_centroids)){
      
      ## Calculate distances to centroids
      for(centroid_number in 1:nrow(centroids)){
        data <- data |> rowwise() |> mutate(!!paste0('distanceToCentroid',centroid_number) := d(c_across(1:dim),unlist(centroids[centroid_number,])))
      }
      
      ## Defer the clustering with respect to the centroid
      data <- data |> mutate(cluster = which.min(c_across((dim+1):(dim+k))))
      
      ## Save old centroids, to compare with next centroids
      old_centroids <- centroids
      
      ## Calculate new centroids as the MEAN of the clusters
      ## THIS is where this algorithm gets its name from
      centroids <- data |>
        ungroup() |>
        group_by(cluster) |>
        summarise_at(1:dim,mean) |> 
        select(-cluster)
    }
    
    ## Calculate the cost of the found cluster, 
    ## meaning the sum over all variances of the points to their cluster centroid
    cost <- data |>
      ungroup() |>
      group_by(cluster) |>
      summarise_at((dim+1):(dim+nrow(centroids)), function(x){sum(x*x)}) |>
      select(-cluster) |>
      as.matrix() |>
      diag() |>
      sum()
    
    
    #### For testing purpoises: compare the found clustering to the currently best clustering
    ##ggplot(data,aes(x=data[[1]],y=data[[2]],colour = cluster)) + 
    ##  geom_point() + 
    ##  scale_color_gradient2(midpoint = k/2+1/2, low="darkblue", mid="green",high="red", space ="Lab" )
    ##
    ##
    ##ggplot(best_clustered_data,aes(x=data[[1]],y=data[[2]],colour = cluster)) + 
    ##  geom_point() + 
    ##  scale_color_gradient2(midpoint = k/2+1/2, low="darkblue", mid="green",high="red", space ="Lab" )
    
    
    ## Check if the found cluster has minimal cost and if so, 
    ## update the currently best clustering guess 
    if(cost < minimal_cost){
      minimal_cost <- cost
      best_centroids <- centroids
      best_clustered_data <- data
    }
  }
  
  
  
  ## return the data, having only added a row for the clusters
  return(data |> select(all_of(c(1:dim,dim+k+1))))
}






## For illustration purpoises:

## Lets create some test data
data <- generateClusterTestDataSimple2D(n=100, nclusters = 8)

## This is what it looks like
ggplot(data,aes(x=data[[1]],y=data[[2]])) + geom_point()

##################################################
## Apply the k-means-algorithm with k = 4       ##
clustered_data <- k_means(data, k = 4)          ##
## (This may take a while)                      ##
##################################################

## Lets look at the results 
ggplot(clustered_data,aes(x=data[[1]],y=data[[2]],colour = cluster)) + 
  geom_point() + 
  scale_color_gradient2(midpoint = k/2+1/2, low="blue", mid="green",high="red", space ="Lab" ) +
  coord_fixed()

##################################################
## Now try the k-means-algorithm with larger k  ##
clustered_data <- k_means(data, k = 6)          ##
## (This may take a while)                      ##
##################################################


## Better!
ggplot(clustered_data,aes(x=data[[1]],y=data[[2]],colour = cluster)) + 
  geom_point() + 
  scale_color_gradient2(midpoint = k/2+1/2, low="darkblue", mid="green",high="red", space ="Lab" ) +
  coord_fixed()


