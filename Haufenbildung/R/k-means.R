#k-Means Algorithmus




## The standard K-means-Algorithm
## Tries fitting the data into K many clusters centered around so called
## centroids which are derived from the mean value of guessed clusters
##
## Inputs:
## data,      a tibble with with every row representing a data point. 
##            The number columns is therefore the dimensionality,
##            The number of rows is the sample size and called n.
## K          A whole number between 0 and n+1.
##            The amount of Clusters the algorithm tries to find in the data
## D          A metric whose inputs are the rows of data as atomic vectors
## 
## Returns:
## function,  a function relating every data point to their cluster.
##            Can be used on new data!
##            input: atomic vectors Of the data row type
##            returns: a number 1-k, representing the related cluster
##
K_means <- function(data,K,D=NULL){
  
  ## if not specified, use euclidean metric
  if(base::missing(D)){D <- function(x,y){base::sum((x-y)^2)}}
  
  ## some necessary variables
  n <- base::nrow(data)
  dim <- base::ncol(data)
  minimal_cost <- Inf
  old_centroids <- tibble::tibble()
  
  
  ## Invariants: test the input
  base::stopifnot('Data must have more than 0 rows' = n>0)
  base::stopifnot('K is not a whole number' = is.wholenumber(K))
  base::stopifnot('K must be between 0 and n+1' = (0 < K && K < n+1))
  
  
  ## Define starting centroids
  centroids <- data[,1:dim] |> 
    dplyr::ungroup() |>
    dplyr::slice_sample(n=K)
  
  
  ## Main loop: repeat iterating the cluster means, until no more change 
  while(!base::identical(centroids, old_centroids)){
    
    ## Calculate distances to centroids
    for(centroid_number in 1:base::nrow(centroids)){
      data <- data |> dplyr::rowwise() |> dplyr::mutate(!!base::paste0('distanceToCentroid',centroid_number) := D(dplyr::c_across(all_of(1:dim)),base::unlist(centroids[centroid_number,])))
    }
    
    ## Defer the clustering with respect to the centroid
    data <- data |> dplyr::mutate(cluster = base::which.min(dplyr::c_across((dim+1):(dim+K))))
    
    ## Save old centroids, to compare with next centroids
    old_centroids <- centroids
    
    ## Calculate new centroids as the MEAN of the clusters
    ## THIS is where this algorithm gets its name from
    centroids <- data |>
      dplyr::ungroup() |>
      dplyr::group_by(cluster) |>
      dplyr::summarise_at(1:dim,mean) |> 
      dplyr::select(-cluster)
  }
  
  ## return a function returning the cluster a datapoint (atomic vector) belongs to
  return(function(x) {
    distances <- base::numeric()
    for(k in 1:K){
      distances = c(distances,D(x,base::unlist(centroids[k,])))
    }
    return(base::which.min(distances))})
}


## The K-means-Algorithm optimized to look for GLABAL optimal clusters
## Tries fitting the data into K many clusters centered around so called
## centroids which are derived from the mean value of guessed clusters.
## This process is then repeated a number of times and only the best clustering
## with respect to inner Inequality is returned.
##
## Inputs:
## data,      a tibble with with every row representing a data point. 
##            The number columns is therefore the dimensionality,
##            The number of rows is the sample size and called n.
## K,         a whole number between 0 and n+1.
##            The amount of Clusters the algorithm tries to find in the data
## D,         a metric whose inputs are the rows of data as atomic vectors
## tries,     an integer greater than one. 
##            The amount of times the algorithms should try to find a globally best
##            clustering.
##
## Returns:
## function,  a function relating every data point to their cluster.
##            Can be used on new data!
##            input: atomic vectors Of the data row type
##            returns: a number 1-k, representing the related cluster
##
K_means_global <- function(data,K,D=NULL,tries=K){
  
  ## if not specified, use euclidean metric
  if(base::missing(D)){D <- function(x,y){base::sum((x-y)^2)}}
  
  ## some necessary variables
  n <- base::nrow(data)
  dim <- base::ncol(data)
  minimal_cost <- Inf
  old_centroids <- tibble::tibble()
  
  
  ## Invariants: test the input
  base::stopifnot('Data must have more than 0 rows' = n>0)
  base::stopifnot('K is not a whole number' = is.wholenumber(K))
  base::stopifnot('K must be between 0 and n+1' = (0 < K && K < n+1))
  
  
  ## Start of actual algorithm
  ## Try 5 times, to minimize the dependency on random chance
  for(repeats in 1:tries){
    
    clustering <- K_means(data,K,D)
    
    ## Calculate the cost of the found cluster, 
    ## meaning the sum over all distances of the points to their cluster centroid
    cost <- inner_inequality(data,clustering)
    
    #### For testing purposes: compare the found clustering to the currently best clustering
    ##ggplot(data,aes(x=data[[1]],y=data[[2]],colour = cluster)) + 
    ##  geom_point() + 
    ##  scale_color_gradient2(midpoint = K/2+1/2, low="darkblue", mid="green",high="red", space ="Lab" )
    ##
    ##
    ##ggplot(best_clustered_data,aes(x=data[[1]],y=data[[2]],colour = cluster)) + 
    ##  geom_point() + 
    ##  scale_color_gradient2(midpoint = K/2+1/2, low="darkblue", mid="green",high="red", space ="Lab" )
    
    ## Check if the found cluster has minimal cost and if so, 
    ## update the currently best clustering guess 
    if(cost < minimal_cost){
      minimal_cost <- cost
      best_clustering <- clustering
      
      base::print(base::paste0('found new best clustering with cost ',cost))
    }
  }
  
  
  
  ## return a function returning the cluster a datapoint (atomic vector) belongs to
  return(best_clustering)
}

find_cluster_amount <- function(data){
  clusterings <- list()
  improvement <- 2
  K <- 1
  
  while(improvement>1){
    print(paste0('checking K = ',K))
    
    clusterings[[K]] <- K_means_global(data,K,tries = 10) 
    
    inner_inequalities <- lapply(clusterings,function(x) inner_inequality(data,x))
    
    plot(1:K,inner_inequalities,asp=1)
    
    if(K>1)improvement <- inner_inequalities[[K-1]] - inner_inequalities[[K]]
    
    
    print(paste0('Improvement from K = ',K,' to K = ',K,' is ',improvement))
    
    K <- K+1
    
  }
  
  
  return(K-2)
}


## Example:

## Lets create some test data
data <- generateClusterTestDataSimple2D(n=100, nclusters = 10)

## This is what it looks like
view_clusters(data,function(x) 1)

##################################################
## Apply the K-means-algorithm with K = 4       ##
clustering <- K_means(data, K = 4)
## (This may take a while)                      ##
##################################################

## Lets look at the results 
view_clusters(data,clustering)

inner_inequality(data,clustering)

######################################################
## Apply the K-means-algorithm with K = 4           ##
## But now look for global minimum of costs         ##
clustering <- K_means_global(data, K = 4, tries=10)
## (This may take a while)                          ##
######################################################

## Lets look at the results 
view_clusters(data,clustering)

##################################################
## Now try the K-means-algorithm with larger K  ##
clustering <- K_means_global(data, K = 6, tries=10)
## (This may take a while)                      ##
##################################################


## Better!
view_clusters(data,clustering)

## Giving new data:
new_data <- tibble::tibble(X = stats::runif(10000,min=0,max=1), Y = stats::runif(10000,min=0,max=1))

## view the clustering applied to unknown data
view_clusters(new_data,clustering)











## Another example: What if we dont know the amount of clusters?
##

data <- generateClusterTestDataSimple2D(n=100, nclusters=base::floor(stats::runif(1,1,10)))

view_clusters(data,function(x) 1)

## Use find_cluster_amount, to calculate K, using K_means an the 'elbow-graph-approach' 
## be careful, this is a heuristic approach
K <- find_cluster_amount(data)

clustering <- K_means_global(data, K,tries = 10)
view_clusters(data,clustering)





## Another example: Nonspherical data. 
## this is where this algorithm is rather bad at

## lets grab some test data
list_of_paths <- list(tibble::tibble(X = c(0.2,0.2,0.6,0.6), Y = c(0.4,0.8,0.8,0.4)),tibble::tibble(X = c(0.4,0.4,0.8,0.8), Y = c(0.6,0.2,0.2,0.6)))
data <- generateClusterTestData2DFromPaths(n=300,list_of_paths)

## there are, rather obviously, 2 clusters
ggplot2::ggplot(data,ggplot2::aes(x=data[[1]],y=data[[2]])) + 
  ggplot2::geom_point() 

## lets try k_mean
clustering <- K_means_global(data, 2,tries = 10)

## not very succsessful
view_clusters(data,clustering)



