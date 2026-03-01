#k-Means Algorithmus




#' K-Means
#'
#' The standard K-Means algorithm.
#' Tries fitting the data into K many clusters centered around so called
#' centroids which are derived from the mean value of guessed clusters.
#'
#' @param data      a tibble with with every row representing a data point.
#'            The number columns is therefore the dimensionality,
#'            The number of rows is the sample size and called n.
#' @param K          A whole number between 0 and n+1.
#'            The amount of Clusters the algorithm tries to find in the data
#' @param metric    A character. One of \code{'euclidean'}, \code{'maximum'},
#'   \code{'Lp'} or \code{'manhattan'}.
#' @param p         A numeric greater than or equal to 1. If \code{metric} was
#'   chosen to be \code{'Lp'}, this will be used as the p of the p-Metric.
#' @param custom_metric A semi definite and symmetric function whose inputs are
#'   two of the \code{data} row type.
#' @param tries A positive integer. The amount of times the algorithm should retry with new starting centroids
#' @param .print_info A logical. Prints some useful information for debugging.
#'
#' @returns A list of the class 'clustering'. Contains \itemize{
#'   \item{\strong{\code{'clustered_data'}}} a tibble of original data with a new column called \code{'cluster'}
#'   \item{\strong{\code{'clustering_function'}}} a function applicable to known and
#'   unknown data points. Returns the cluster the data point belongs to.
#'   \item{\strong{\code{'inner_inequality'}}} a numeric. The sum of all differences of the data points to their cluster centroid.
#'   }
#' @export
kMeans <- function(data,K,metric='euclidean',p=NULL,custom_metric=NULL,tries=K, .print_info = FALSE){
  ## some necessary variables
  n <- base::nrow(data)
  dim <- base::ncol(data)
  minimal_cost <- Inf
  old_centroids <- tibble::tibble()

  if(is.null(custom_metric)){
    if(metric=='euclidean')
      metric <- euclidean
    else if(metric=='maximum')
      metric <- maximumMetric
    else if(metric=='Lp'){
      stopifnot('If you chose the Lp metric, please provide a value for p' = !is.null(p))
      stopifnot('p must be a numeric greater than or equal to 1' = is.numeric(p) && p>=1 )
      metric <- pMetric(p)
    }
    else if(metric=='manhattan')
      metric <- pMetric(1)
    else stop('Unknown metric. Look up on the help page which metrics are available ore input a custum metric using the argument custom_metric.')
  }
  else metric <- custom_metric


  ## Invariants: test the input
  base::stopifnot('Data must have more than 0 rows' = n>0)
  base::stopifnot('tries must be an integer greater than 0' = is.wholenumber(tries) && 0 < tries )
  base::stopifnot('K must be an integer between 0 and n+1' = is.wholenumber(K) && (0 < K && K < n+1))
  base::stopifnot('data must have at least K unique data points' = K <= nrow(unique(data)))

  ## Start of actual algorithm
  ## Try multiple times, to minimize the dependency on random chance
  for(repeats in 1:tries){

    ## Define starting centroids
    centroids <- data[,1:dim] |>
      dplyr::ungroup() |>
      dplyr::slice_sample(n=K)



    ## Main loop: repeat iterating the cluster means, until no more change
    while(!base::identical(centroids, old_centroids)){

      ## calculate all the points distances to the centroids
      distances <- centroids |> apply(1,function(centroid){data[,1:dim] |> apply(1,function(x){metric(x,centroid)})}) |> t()

      ## Defer the clusters, every point to their nearest centroid
      cluster <- apply(distances,2,which.min)
      data$cluster <- cluster

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

    ## Calculate the cost of the found cluster,
    ## meaning the sum over all distances of the points to their cluster centroid
    cost <- apply(distances,2,min) |> sum()

    ## Check if the found cluster has minimal cost and if so,
    ## update the currently best clustering guess
    if(cost < minimal_cost){
      minimal_cost <- cost
      best_centroids <- centroids
      best_clustered_data <- data

      if(.print_info)
        base::print(base::paste0('Found a new best clustering with cost ',cost))
    }
  }



  ## returns clustered data and a function returning the cluster a datapoint (atomic vector) belongs to
  return(structure(
              list(
                clustered_data = best_clustered_data,
                clustering_function = function(x)
                  best_centroids |>
                  apply(1,function(centroid) metric(x,centroid)) |>
                  base::which.min(),
                inner_inequality = minimal_cost),
              description = 'Data clustered by K-Means algorithm',
              class= 'clustering'
             )
        )
}


#' Fining a best cluster amount using the 'Elbow-method'
#'
#' Tries finding a K-Means Clustering for increasing K and stops if the
#' improvement of the cost reduction is less then 1. Here we use the
#' innerInequality function to calculate costs.
#'
#' Be careful with its result, it is only heuristically optimal, as the cost
#' reduction bound can be chosen arbitrarily.
#'
#' @param data        a tibble with with every row representing a data point.
#' @param .print_info A logical. Prints some useful information for debugging.
#'
#' @returns a positive integer, the 'optimal' amount of clusters
#'
#' @export
findClusterAmountElbow <- function(data, .print_info = FALSE){
  inner_inequalities <- numeric()
  improvement <- Inf
  K <- 1

  while(improvement>1){

    if(.print_info)
      print(paste0('checking K = ',K))

    inner_inequalities[[K]] <- kMeans(data,K,tries = 10)$inner_inequality

    if(.print_info)
      plot(1:K,inner_inequalities,asp=1)

    if(K>1)improvement <- inner_inequalities[[K-1]] - inner_inequalities[[K]]



    if(.print_info)
      print(paste0('Improvement from K = ',K-1,' to K = ',K,' is ',improvement))

    K <- K+1

  }


  return(K-2)
}



#' Fining a best cluster amount using the 'Silhoutte coefficient'
#'
#' Tries finding a K-Medioids clustering for increasing K and stops if the
#' mean Silhouette is worse than the previously calculated one.
#'
#' Be careful with its result, it is only heuristically optimal.
#'
#' @param data      a tibble with with every row representing a data point.
#' @param metric    A character. One of \code{'euclidean'}, \code{'maximum'},
#'   \code{'Lp'} or \code{'manhattan'}.
#' @param p         A numeric greater than or equal to 1. If \code{metric} was
#'   chosen to be \code{'Lp'}, this will be used as the p of the p-Metric.
#' @param custom_metric A semi definite and symmetric function whose inputs are
#'   two of the \code{data} row type.
#' @param .print_info A logical. Prints some useful information for debugging.
#'
#' @returns a positive integer, the 'optimal' amount of clusters
#'
#' @export
findClusterAmountSilhouette <- function(data,metric='euclidean',p=NULL,custom_metric=NULL, .print_info = FALSE){
  clusterings <- list()
  fit <- list()
  improvement <- Inf
  K <- 1

  while(improvement>0){
    if(.print_info)
      print(paste0('checking K = ',K))
    print(metric)

    clusterings[[K]] <- kMedioids(data = data,K = K,metric = metric,p=p,custom_metric=custom_metric, .print_info = .print_info)

    fit[[K]] <- meanSilhouette(data,clusterings[[K]]$clustering_function,metric)

    plot(1:K,fit)

    if(K>1) improvement <- fit[[K]] - fit[[K-1]]


    if(.print_info)
      print(paste0('Improvement from K = ',K-1,' to K = ',K,' is ',improvement))

    K <- K+1

  }


  return(K-2)
}


### Example:
#
### Lets create some test data
#data <- generateClusterTestDataSimple2D(n=100, n_clusters = 7)
#
### This is what it looks like
#viewClusters(data)
#
###################################################
### Apply the K-means-algorithm with K = 4       ##
#clustering <- kMeans(data, K = 4)
###                                              ##
###################################################
#
#
### Lets look at the result
#clustering
#viewClusters(clustering$clustered_data)
#
### we also learn how good this minimizes the inner inequalities
#clustering$inner_inequality
#
#
###################################################
### Apply the K-means-algorithm with larger K    ##
#clustering <- kMeans(data, K = 7)
###                                              ##
###################################################
#
#
### Lets look at the result
#clustering
#viewClusters(clustering$clustered_data)
#
#
#
#
#
#
#
#
#
#
#
#
#
### 2nd Example: Vary the amount of tries
#
#data <- generateClusterTestDataSimple(dim = 3,cluster_amount = 10)
#
#######################################################
### Apply the K-means-algorithm ONCE                 ##
#clustering <- kMeans(data, K = 5, tries = 1)
### (This may take a while)                          ##
#######################################################
#
### Lets look at the results
#viewClusters(clustering$clustered_data)
#clustering$inner_inequality
#
#######################################################
### Apply the K-means-algorithm, retrying multiple times
### to improve chances on finding a globally best clustering
#clustering <- kMeans(data, K = 5, tries=10)
### (This may take a while)                      ##
###################################################
#
### Better!
#viewClusters(clustering$clustered_data)
#clustering$inner_inequality
#
#
#
#
#
#
### 3rd Example: New Data
## The resulting function does take inputs not of the original data set
## note that the amount of unkown data is vastly greater than the training data
#clusters <- list(tibble::tibble(X=0.05,Y=0.05),
#                 tibble::tibble(X=0.03,Y=0.02),
#                 tibble::tibble(X=0.03,Y=0.08),
#                 tibble::tibble(X=0.07,Y=0.04))
#training_data <- generateClusterTestData2DFromPaths(n=50, clusters)
#unknown_data <- generateClusterTestData2DFromPaths(n=1000, clusters)
#more <- tibble::as_tibble(matrix(runif(20000,min=0,max = 0.1),ncol = 2))
#
## derive a clustering using K-Means
#clustering <- kMeans(training_data,metric='maximum',4)
## this would take ages for 1000 data points
#
## lets take a look
#viewClusters(training_data,clustering$clustering_function)
#viewClusters(unknown_data,clustering$clustering_function)
#viewClusters(more_data,clustering$clustering_function)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
### 4th example: What if we dont know the amount of clusters?
###
#
#data <- generateClusterTestDataSimple2D(n=100, n_clusters=base::floor(stats::runif(1,1,10)))
#
#viewClusters(data)
#
### Use find_cluster_amount, to calculate K, using K_means an the 'elbow-graph-approach'
### be careful, this is a heuristic approach
#K <- findClusterAmountElbow(data, .print_info = TRUE)
#
#clustering <- kMeans(data, K, tries = 10)
#viewClusters(clustering$clustered_data)
#
#
#
#
#
### Another example: Nonspherical data.
### this is where this algorithm is rather bad at
#
### lets grab some test data
#list_of_paths <- list(tibble::tibble(X = c(0.2,0.2,0.6,0.6), Y = c(0.4,0.8,0.8,0.4)),tibble::tibble(X = c(0.4,0.4,0.8,0.8), Y = c(0.6,0.2,0.2,0.6)))
#data <- generateClusterTestData2DFromPaths(n=300,list_of_paths)
#
### there are, rather obviously, 2 clusters
#viewData(data)
#
### lets try  K-Means
#clustering <- kMeans(data,K = 2)
#
### not very succsessful
#viewClusters(clustering$clustered_data))
#
#
#
#
#
