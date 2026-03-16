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
#' @inheritParams getDistanceFunction
#' @param tries A positive integer. The amount of times the algorithm should retry with new starting centroids
#' @param .print_info A logical. Prints some useful information for debugging.
#'
#' @returns A list of the class 'clustering'. Contains \itemize{
#'   \item{\strong{\code{'clustered_data'}}} a tibble of original data with a new column called \code{'cluster'}
#'   \item{\strong{\code{'clustering_function'}}} a function applicable to known and
#'   unknown data points. Returns the cluster the data point belongs to.
#'   \item{\strong{\code{'inner_inequality'}}} a numeric. The sum of all differences of the data points to their cluster centroid.
#'   }
#' @references [Richter 9.1](https: //link.springer.com/book/10.1007/978-3-662-59354-7)
#' @export
kMeans <- function(data,K,distance_method='euclidean',p=NULL,custom_distance_function=NULL,tries=K, .print_info = FALSE){
  start <- Sys.time()
  ## some necessary variables
  n <- base::nrow(data)
  dim <- base::ncol(data)
  minimal_cost <- Inf
  old_centroids <- tibble::tibble

  # Invariant, check parameter tries
  base::stopifnot('tries must be an integer greater than 0' = is.wholenumber(tries) && 0 < tries )

  distance <- getDistanceFunction(distance_method = distance_method,
                                  p = p,
                                  custom_distance_function = custom_distance_function)


  # Special case: only 1 cluster. We want to allow it, to compare which cluster amount to choose
  if(K==1){
    centroid <- data |> dplyr::summarise(across(everything(),mean))
    distances <- data |> apply(1,function(x) distance(x,centroid))
    D <- dissimilarityMatrix(data,
                             distance_method = distance_method,
                             p = p,
                             custom_distance_function = custom_distance_function)

    return(structure(
      list(
        clustered_data = clustered_data(data, cluster=1),
        clustering_function = function(x) 1,
        centroids = data |> dplyr::summarise(across(everything(),mean)),
        inner_inequality = distances |> sum(),
        sum_of_squares = distances^2 |> sum(),
        mean_silhouette = 1:n |> sapply(function(index) silhouette_faster(dplyr::mutate(data, cluster=1),index,D)) |> mean()
      ),
      description = 'Data clustered by K-Means algorithm',
      class= c('K-Means-clustering',  'clustering')
    )
    )

  }

  ## Invariants: test the input
  base::stopifnot('Data must have more than 0 rows' = n>0)
  base::stopifnot('K must be an integer between 0 and n+1' = is.wholenumber(K) && (0 < K && K < n+1))
  base::stopifnot('data must have at least K unique data points' = K <= nrow(unique(data)))
  if(.print_info) print(Sys.time() - start)

  ## Start of actual algorithm
  ## Try multiple times, to minimize the dependency on random chance
  for(repeats in 1:tries){

    ## Define starting centroids
    centroids <- data[,1:dim] |>
      dplyr::ungroup() |>
      dplyr::slice_sample(n=K)

    ## Main loop: repeat iterating the cluster means, until no more change
    while(!base::identical(centroids, old_centroids)){
      if(.print_info) print(centroids)

      ## calculate all the points distances to the centroids
      distances <- centroids |> apply(1,function(centroid){data[,1:dim] |> apply(1,function(x){distance(x,centroid)})}) |> t()

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
      best_distances <- distances |> apply(2,min)
      best_centroids <- centroids
      best_clustered_data <- data

      if(.print_info)
        base::print(base::paste0('Found a new best clustering with cost ',cost))
    }
  }
  if(.print_info) print(Sys.time() - start)


  f <- function(x)
    best_centroids |>
    apply(1,function(centroid) distance(x,centroid)) |>
    base::which.min()
  if(.print_info) print(Sys.time() - start)

  D <- dissimilarityMatrix(data,
                           distance_method = distance_method,
                           p = p,
                           custom_distance_function = custom_distance_function)

  if(.print_info) print(Sys.time() - start)

  ## returns clustered data and a function returning the cluster a datapoint (atomic vector) belongs to
  return(structure(
              list(
                clustered_data = clustered_data(best_clustered_data),
                clustering_function = f,
                centroids = best_centroids,
                inner_inequality = minimal_cost,
                sum_of_squares = best_distances^2 |> sum(),
                mean_silhouette = 1:n |> sapply(function(index) silhouette_faster(best_clustered_data,index,D)) |> mean()
                ),
              description = 'Data clustered by K-Means algorithm',
              class= c('K-Means-clustering',  'clustering')
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
#' @param check_min   A positive Integer. The minimum amount of clusters to be checked for their inner Inequality.
#' @param .print_info A logical. Prints some useful information for debugging.
#'
#' @returns a positive integer, the 'optimal' amount of clusters
#'
#' @export
findClusterAmountElbow <- function(data, check_min = 1, .print_info = FALSE){
  inner_inequalities <- numeric()
  improvement <- list(Inf)
  K <- 1

  inner_inequalities[[1]] <- kMeans(data,1,tries = 10)$inner_inequality

  while(improvement[[K]]>1 || K <= check_min){
    K <- K+1

    if(.print_info)
      print(paste0('checking K = ',K))

    inner_inequalities[[K]] <- kMeans(data,K,tries = 10)$inner_inequality

    if(.print_info)
      plot(1:K,inner_inequalities)

    improvement[[K]] <- inner_inequalities[[K-1]] - inner_inequalities[[K]]

    if(.print_info)
      print(paste0('Improvement from K = ',K-1,' to K = ',K,' is ',improvement[[K]]))
  }

  improvement[improvement<1] <- Inf

  return(which.min(improvement))
}



#' Fining a best cluster amount using the 'Silhouette coefficient'
#'
#' Tries finding a K-Medoids clustering for increasing K and stops if the
#' mean Silhouette is worse than the previously calculated one.
#'
#' Be careful with its result, it is only heuristically optimal.
#'
#' @param data      a tibble with with every row representing a data point.
#' @inheritParams getDistanceFunction
#' @param check_min   A positive Integer. The minimum amount of clusters to be checked for their inner Inequality.
#' @param .print_info A logical. Prints some useful information for debugging.
#'
#' @returns a positive integer, the 'optimal' amount of clusters
#' @export
findClusterAmountSilhouette <- function(data,distance_method='euclidean',p=NULL,custom_distance_function=NULL ,check_min = 1, .print_info = FALSE){
  clusterings <- list()
  fit <- list()
  improvement <- Inf
  K <- 1


  while(improvement>0 || K <= check_min){
    if(.print_info)
      print(paste0('checking K = ',K))

    fit[[K]] <- kMedoids(data = data,K = K,distance_method = distance_method,p=p,custom_distance_function=custom_distance_function, .print_info = .print_info)$mean_silhouette

    if(K>1) improvement <- fit[[K]] - fit[[K-1]]

    if(.print_info)
      print(paste0('Improvement from K = ',K-1,' to K = ',K,' is ',improvement))

    K <- K+1

  }


  plot(1:(K-1),fit)

  return(which.max(fit))
}
