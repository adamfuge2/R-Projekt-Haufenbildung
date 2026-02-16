

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
K_medioids <- function(data,K,D=NULL){
  
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
  
  
  ## (BUILD) Define starting centroids
  centroids <- data[,1:dim] |> 
    dplyr::ungroup() |>
    dplyr::slice_sample(n=K)
  
  
  ## Calculate distances to centroids
  for(centroid_number in 1:base::nrow(centroids)){
    data <- data |> dplyr::rowwise() |> 
            dplyr::mutate(!!base::paste0('distanceToCentroid',centroid_number) := D(dplyr::c_across(all_of(1:dim)),base::unlist(centroids[centroid_number,])))
  }
  
  
  ## Defer the clustering with respect to the centroid
  data <- data |> dplyr::mutate(cluster = base::which.min(dplyr::c_across((dim+1):(dim+K))))
  
  clustering <- function(x) {
    distances <- base::numeric()
    for(k in 1:K){
      distances = c(distances,D(x,base::unlist(centroids[k,])))
    }
    return(base::which.min(distances))}
    
  view_clusters(data[1:dim],clustering)
  
  ## Main loop: repeat iterating the cluster means, until no more change 
  while(TRUE)){
    
    
    costs <- inner_inequality(data[,1:dim],clustering)
    
    if(costs < minimal_cost){
      ## Weve found a new, better medioids configuration!
      
      ## Save old centroids, to compare with next centroids
      old_centroids <- centroids
      minimal_cost <- costs
      
      ## Reset the selectable next medioids
      possible_next_centroids_per_old_centroid <- data[,1:dim]
      
      for(k in 1:K)
        possible_next_centroids_per_old_centroid <- possible_next_centroids_per_old_centroid |> dplyr::filter(!!base::paste0('distanceToCentroid',k) != 0) 
      
      possible_next_centroids <- tibble::tibble()
      
      
      for(k in 1:K)
        pssible_next_centroids <- possible_next_centroids_per_old_centroid |> dplyr::mutate(centroidToReplace = k) |> dplyr::full_join(possible_next_centroids)
      
      
      
      }
    else{
      if(base::nrow(possible_next_centroids) > 1)
        possible_next_centroids <- possible_next_centroids[2:base::nrow(possible_next_centroids)]
      else break
    }
    
    cetroids[utils::head(possible_next_centroids)[[centroidToReplace]] , ] <- utils::head(possible_next_centroids)[,1:dim]
    
    
    
    
    
    data_point_to_replace_centroid_with <- pssible_next_centroids |> dplyr::slice_sample(n=1)
    
  }
  
  ## return a function returning the cluster a datapoint (atomic vector) belongs to
  return(function(x) {
    distances <- base::numeric()
    for(k in 1:K){
      distances = c(distances,D(x,base::unlist(centroids[k,])))
    }
    return(base::which.min(distances))})
}

## andere idee
K_medioids <- function(data,K,D=NULL){
  
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
  
  
  ## (BUILD) Define starting centroids
  centroids <- data[,1:dim] |> 
    dplyr::ungroup() |>
    dplyr::slice_sample(n=K)
  
  
  ## Calculate distances to centroids
  for(centroid_number in 1:base::nrow(centroids)){
    data <- data |> dplyr::rowwise() |> 
      dplyr::mutate(!!base::paste0('distanceToCentroid',centroid_number) := D(dplyr::c_across(all_of(1:dim)),base::unlist(centroids[centroid_number,])))
  }
  
  
  ## Defer the clustering with respect to the centroid
  data <- data |> dplyr::mutate(cluster = base::which.min(dplyr::c_across((dim+1):(dim+K))))
  
  clustering <- function(x) {
    distances <- base::numeric()
    for(k in 1:K){
      distances = c(distances,D(x,base::unlist(centroids[k,])))
    }
    return(base::which.min(distances))}
  
  view_clusters(data[1:dim],clustering)
  
  ## Main loop: repeat iterating the cluster means, until no more change 
  while(TRUE)){
    
    
    costs <- inner_inequality(data[,1:dim],clustering)
    
    if(costs < minimal_cost){
      ## Weve found a new, better medioids configuration!
      
      ## Save old centroids, to compare with next centroids
      old_centroids <- centroids
      minimal_cost <- costs
      
      ## Reset the selectable next medioids
      possible_next_centroids_per_old_centroid <- data[,1:dim]
      
      for(k in 1:K)
        possible_next_centroids_per_old_centroid <- possible_next_centroids_per_old_centroid |> dplyr::filter(!!base::paste0('distanceToCentroid',k) != 0) 
      
      possible_next_centroids <- tibble::tibble()
      
      
      for(k in 1:K)
        pssible_next_centroids <- possible_next_centroids_per_old_centroid |> dplyr::mutate(centroidToReplace = k) |> dplyr::full_join(possible_next_centroids)
      
      
      
    }
    else{
      if(base::nrow(possible_next_centroids) > 1)
        possible_next_centroids <- possible_next_centroids[2:base::nrow(possible_next_centroids)]
      else break
    }
    
    cetroids[utils::head(possible_next_centroids)[[centroidToReplace]] , ] <- utils::head(possible_next_centroids)[,1:dim]
    
    
    
    
    
    data_point_to_replace_centroid_with <- pssible_next_centroids |> dplyr::slice_sample(n=1)
    
  }

## return a function returning the cluster a datapoint (atomic vector) belongs to
return(function(x) {
  distances <- base::numeric()
  for(k in 1:K){
    distances = c(distances,D(x,base::unlist(centroids[k,])))
  }
  return(base::which.min(distances))})
}













data <- generateClusterTestDataSimple2D(n=100,nclusters=3)

view_clusters(data,function(x) 1 )


