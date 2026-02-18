


## andere idee
K_medioids <- function(data,K,metric=NULL){
  
  ## if not specified, use euclidean metric
  if(base::missing(metric)){metric <- function(x,y){base::sum((x-y)^2)}}
  
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
  centroids <- greedySearchMedioids(data,K,metric)
  
  
  ## Calculate distances to centroids
  for(centroid_number in 1:base::nrow(centroids)){
    data <- data |> dplyr::rowwise() |> 
      dplyr::mutate(!!base::paste0('distanceToCentroid',centroid_number) := metric(dplyr::c_across(dplyr::all_of(1:dim)),base::unlist(centroids[centroid_number,])))
  }
  
  ## Defer the clustering with respect to the centroid
  data <- data |> dplyr::mutate(cluster = base::which.min(dplyr::c_across((dim+1):(dim+K))))
  
  clustering <- function(x) {
    distances <- base::numeric()
    for(k in 1:K){
      distances = c(distances,metric(x,base::unlist(centroids[k,])))
    }
    return(base::which.min(distances))}
  
  view_clusters(data[1:dim],clustering)
  
    
  new_min_cost <- inner_inequality(data[,1:dim],clustering)
    
  while(new_min_cost < old_min_cost){
    ## Weve found a new, better medioids configuration!
    base::print(base::paste0('Found a new best clustering! The new best cost is ',new_min_cost))
    
    ## Save old centroids, to compare with next centroids
    old_centroids <- centroids
    old_min_cost <- new_min_cost
    
    ## calculate which changed medioids would diminsh the inner inequality most
    costs <- base::matrix(base::rep(1:n,base::nrow(centroids)),ncol = base::nrow(centroids))
    for(k in 1:base::nrow(centroids)){
      costs[,k] <- sapply(costs[,k],function(x) inner_inequality_after_changining_medioid(data[,1:dim],x,centroids,k,metric))
    }
    
    ## Save which indeces of data point o and medioid m would have the lower cost
    m_opt <- arrayInd(which.min(costs), dim(costs))[2]
    o_opt <- arrayInd(which.min(costs), dim(costs))[1]
    
    
    # change medioid
    centroids[m_opt,] <- data[o_opt,1:dim]
    
    # new minimal
    new_min_cost <- costs[o_opt,m_opt]
  }
    

  
  ## return a function returning the cluster a datapoint (atomic vector) belongs to
  
  return(   function(x) 1:k |> 
              sapply(function(k) metric(x,base::unlist(centroids[k,]))) |> 
              base::which.min() )
}





inner_inequality_after_changining_medioid <- function(data,o,centroids,m,metric){
  
  # change medioid
  centroids[m,] <- data[o,]
  
  if(!base::identical(centroids,dplyr::distinct(centroids))){
    return(Inf)
  }
  
  # calculate clustering
  clustering <- function(x) {base::which.min(sapply(1:base::nrow(centroids),function(k) metric(x,base::unlist(centroids[k,]))))}

  # return the costs
  return( inner_inequality(data,clustering,metric))
}




metric <- function(x,y){base::sum((x-y)^2)}

sumOfDistancestTo <- function(data,vector,metric){
  data |> dplyr::rowwise() |> dplyr::mutate(distance = metric(dplyr::c_across(all_of(1:ncol(data))) , vector)) |>
    dplyr::ungroup() |>
    dplyr::summarise(sum = sum(distance)) |>
    base::unlist()
}

dissimilarityMatrix <-function(data,metric){
  M <- base::matrix(base::rep(1:base::nrow(data),base::nrow(data)),ncol = base::nrow(data))
  for(i in 1:base::nrow(data)){
    M[,i] <- sapply(M[,i],function(x) metric(data[x,],data[i,]))
  }
  
  return(M)
}


## greedy search of medioids is part of PAM algorithm for finding a good enough
## starting cluster constellation for the k-medioids algorithm
greedySearchMedioids <- function(data,K,metric){
  
  base::stopifnot('K is not a whole number' = is.wholenumber(K))
  base::stopifnot('K must positive' = (0 < K))
  
  
  M <- dissimilarityMatrix(data,metric)
  
  medioid_index <- M |> base::apply(c(1),sum) |> which.min()
  medioids <- data[medioid_index,]
  M <- M[-medioid_index,-medioid_index]
  
  medioid_indeces <- medioid_index
  
  if(K>1){
    for(k in 2:K){
      
      # For all data points calculate their minimum distance to the chosen medioids
      metric <- sapply(1:base::nrow(data),function(o) min(sapply(1:(k-1), function(m) metric(data[o,],medioids[m,])))) 
        
      # Remove the already chosen medioids and format to a matrix (for later)
      metric <- metric[metric != 0] |> base::rep(base::nrow(data)-k+1) |>
        base::matrix(ncol = base::nrow(data)-k+1, byrow = TRUE)
      
      
      
      # score every data points j based on their distance to the existing medioids vs 
      # all other i 
      # BUT ignore all data point j whose delta is negative
      # aka the data points i that are closer to the existing medioids than the 
      # data point j
      M_k <- apply(metric-M, c(1,2), function(x) max(x,0))
      
      # we take the data point, which would be most advantageous to add to the medioids
      medioid_index <- M_k |> base::apply(c(1),sum) |> which.max()
      
      # remove this medioid from being chosen in the future
      M <- M[-medioid_index,-medioid_index]
      
      # account for the removed rows from M
      sapply(medioid_indeces,function(x) if(medioid_index>x){medioid_index<-medioid_index+1})
      
      # lastly: add the newly found medioid 
      medioid_indeces <- c(medioid_indeces,medioid_index)
      medioids <- dplyr::add_row(medioids, data[medioid_index,])
    }
  }
  return(medioids)
}



## Example
# generate some test data
data <- generateClusterTestDataSimple2D(n=50,5)

# lets view it
view_clusters(data)

##############################################################
# Apply the K-Medioids algortithm                           ##
clustering <- K_medioids(data,K=5)
# as K-medioids has O(n²) runtime this may take a while     ##
##############################################################

# is this any good? We can calculate the inner inequalty of this clustering
# here, lower is better
inner_inequality(data,clustering)

# or we can find the silhouette coefficient of the clustering
# the closer this is to 1, the better
meanSilhouette(data,clustering)




## We can also apply this to data, whose number of clusters is unknown
data <- generateClusterTestDataSimple2D(n=100, nclusters=base::floor(stats::runif(1,1,10)))

# this is what it looks like
view_clusters(data)

##############################################################
# We make a guess for K and apply the K-Medioids algortithm ##
clustering <- K_medioids(data,K=4)
# this may take a while                                     ##
##############################################################

# is it any good?
view_clusters(data,clustering)
meanSilhouette(data,clustering)

##############################################################
# Try a higher K and apply the K-Medioids algortithm        ##
clustering <- K_medioids(data,K=7)
# this may take a while                                     ##
##############################################################

# By comparing the mean Silhouette one can defer the number of clusters
view_clusters(data,clustering)
meanSilhouette(data,clustering)






## New Data
# The resulting function does take inputs not of the original data set
# note that the amount of unkown data is vastly greater than the training data
clusters <- list(tibble::tibble(X=0.05,Y=0.05),
                 tibble::tibble(X=0.03,Y=0.02),
                 tibble::tibble(X=0.03,Y=0.08),
                 tibble::tibble(X=0.07,Y=0.04))
training_data <- generateClusterTestData2DFromPaths(n=50, clusters)
unknown_data <- generateClusterTestData2DFromPaths(n=1000, clusters)

# derive a clustering using K-medioids
clustering <- K_medioids(training_data,4)
# this would take ages for 1000 data points

# lets take a look
view_clusters(training_data,clustering)
view_clusters(unknown_data,clustering)











## greedy searched medioids
# To start of with an already good choice of cluster medians, this algorithm uses
# greedy search. This can also be used, to give a fast, but unoptimized clustering:

# some data
data <- generateClusterTestDataSimple2D(n=100,5)


####################################################
# Use greedy search, to find medioids fast        ##
medioids <- greedySearchMedioids(data,K=5)
#                                                 ##
####################################################

# Defer a clustering
clustering <- clusteringFromCentroids(medioids)

# lets see the unoptimized clustering
view_clusters(data,clustering)

####################################################
# Lets see the real K-Medioids in action!         ##
clustering <- K_medioids(data,K=5)
# go grab a cup of tea, this takes a few minutes  ##
####################################################

# lets see the optimized clustering
view_clusters(data,clustering)


