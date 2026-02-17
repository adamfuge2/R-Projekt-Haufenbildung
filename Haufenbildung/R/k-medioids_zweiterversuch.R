
## andere idee
K_medioids <- function(data,K,D=NULL){
  
  ## if not specified, use euclidean metric
  if(base::missing(D)){D <- function(x,y){base::sum((x-y)^2)}}
  
  ## some necessary variables
  n <- base::nrow(data)
  dim <- base::ncol(data)
  old_min_cost <- Inf
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
      dplyr::mutate(!!base::paste0('distanceToCentroid',centroid_number) := D(dplyr::c_across(dplyr::all_of(1:dim)),base::unlist(centroids[centroid_number,])))
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
  
    
  costs <- inner_inequality(data[,1:dim],clustering)
    
  while(new_min_cost < old_min_cost){
    ## Weve found a new, better medioids configuration!
    
    ## Save old centroids, to compare with next centroids
    old_centroids <- centroids
    old_min_cost <- new_min_cost
    
    ## calculate which changed medioids would diminsh the inner inequality most
    
    costs <- base::matrix(base::rep(1:n,K),ncol = K)
    for(k in 1:K){
      costs[,k] <- sapply(costs[,k],function(x) inner_inequality_after_changining_medioid(data[,1:dim],x,centroids,k))
    }
    
    m_opt <- arrayInd(which.min(costs), dim(costs))[2]
    o_opt <- arrayInd(which.min(costs), dim(costs))[1]
    
    
    # change medioid
    centroids[m_opt,] <- data[o_opt,1:dim]
    
    # new minimal
    new_min_cost <- costs[o_opt,m_opt]
  }
    

  
  ## return a function returning the cluster a datapoint (atomic vector) belongs to
  
  return(   function(x) 1:k |> 
              sapply(function(k) D(x,base::unlist(centroids[k,]))) |> 
              base::which.min() )
}





inner_inequality_after_changining_medioid <- function(data,o,centroids,m){
  
  # change medioid
  centroids[m,] <- data[o,]
  
  if(!base::identical(centroids,dplyr::distinct(centroids))){
    return(Inf)
  }
  
  # calculate clustering
  clustering <- function(x) {base::which.min(sapply(1:base::nrow(centroids),function(k) D(x,base::unlist(centroids[k,]))))}

  # return the costs
  return( inner_inequality(data,clustering,D))
}


greedySearchMedioids(data,K,metric){
  if(K==1){
    data |> dplyr::rowwise() |> dplyr::mutate(cost = sumOfDistancestTo(data,dplyr::c_across(all_of(1:ncol(data))),metric))
    
    return()
  }
  
  
  
}

data
vector<- c(0.5,0.5)
metric <- function(x,y){base::sum((x-y)^2)}

sumOfDistancestTo <- function(data,vector,metric){
  data |> dplyr::rowwise() |> dplyr::mutate(distance = metric(dplyr::c_across(all_of(1:ncol(data))) , vector)) |>
    dplyr::ungroup() |>
    dplyr::summarise(sum = sum(distance)) |>
    base::unlist()
}



greedySearchMedioids(data,K){
  
  
  
  
}



## Beispiel
# generate some test data
data <- generateClusterTestDataSimple2D(n=100,5)

# lets view it
view_clusters(data)

#
clustering <- K_medioids(data,K=5)

inner_inequality(data,clustering)

view_clusters(data,clustering)

