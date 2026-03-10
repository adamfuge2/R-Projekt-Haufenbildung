############ Test Data Generators ######################

#' spherical test data generator
#'
#' @param n           a positive integer. The number of total data points to be generated.
#' @param cluster_amount  a positive integer. The number of clusters to generate.
#'                    If none given, choose a random amount < sqrt(n).
#' @param dim A positive integer. The dimension of the data points to be generated
#'
#' @returns a tibble, every row representing a data point. The columns are named X_1 to X_n.
#' @export
generateClusterTestDataSimple <- function(n=100,
                                          cluster_amount = NULL,
                                          dim = 2,
                                          lower_bounds = c(0,0),
                                          upper_bounds = c(1,1),
                                          clusters_mean = NULL,
                                          clusters_sd = NULL,
                                          clusters_prob = NULL,
                                          colnames = NULL,
                                          include_cluster = FALSE,
                                          .print_info = FALSE){

  # defer dimensions and bounds from input. DEFAULT case: do nothing
  if(any(base::missing(dim),
         base::missing(lower_bounds),
         base::missing(upper_bounds))){
    # something got specified. lets check:

    if(base::missing(dim)){
      # we were not given the parameter dim, but something else

      # defer dim from the actually given parameters
      if(!base::missing(lower_bounds))
        dim <- length(lower_bounds)
      else if(!base::missing(upper_bounds))
        dim <- length(upper_bounds)
    }
    # from here on we are certain we have a value for dim

    # lets defer missing parameters
    if(base::missing(lower_bounds))
      lower_bounds <- rep(0,dim)
    if(base::missing(upper_bounds))
      upper_bounds <- rep(1,dim)
  }

  # if no colname got given, give the dimensions their standard names X_1 to X_n
  if(base::missing(colnames))
    colnames <- paste0('X_',1:dim)

  # invariants
  stopifnot('dim must be greater than 0' = dim > 0)
  stopifnot('lower_bounds and upper bounds must not be empty' = length(lower_bounds) > 0 && length(upper_bounds) > 0)
  stopifnot('lower_bounds and upper_bounds must have the same length'= length(lower_bounds) == length(upper_bounds))
  stopifnot('lower bounds must not be higher than upper bounds' = all(lower_bounds <= upper_bounds))
  stopifnot('the number of column names must match the dimension and length of bounds' = length(colnames) == dim)

  # if no cluster amount specified, determine a random cluster amount
  if(base::missing(cluster_amount)){
    cluster_amount <- base::floor(stats::runif(1,min = 1, max = (2^dim)*base::sqrt(n)))
  }

  # if no standard deviations for the clusters specified, randomize them here
  if(base::missing(clusters_sd))
    clusters_sd <- stats::runif(cluster_amount, min=0.001, max=0.5)


  if(.print_info) print(paste0('clusters_sd: ',paste0(clusters_sd)))
  stopifnot('length of cluster_sd does not match cluster amount' = length(clusters_sd) == cluster_amount)

  # if no means (centers) for the clusters specified, randomize them here
  if(base::missing(clusters_mean))
    clusters_mean <- 1:cluster_amount |>
                        sapply(function(k) stats::runif(dim,
                                                       min=lower_bounds,
                                                       max=upper_bounds)) |>
                        structure(dim = c(dim,cluster_amount)) |>
                        t()

  stopifnot('amount (rows) of cluster_means does not match cluster amount' = nrow(clusters_mean) == cluster_amount)

  # if no probability weights for the clusters got specified, set them all to 1
  if(base::missing(clusters_prob))
    clusters_prob <- rep(1,cluster_amount)

  # for every data point select a cluster according to their probability weights
  selected_clusters <- base::sample(1:cluster_amount, size = n, replace = TRUE, prob = clusters_prob)

  # using these selected clusters we relate the points with a cluster center
  points_origin <- clusters_mean[selected_clusters,]

  # using these selected clusters we relate the points with a standard deviation
  points_sd <-  rep(clusters_sd[selected_clusters],each=dim)
  # the deviation to add to the cluster centers
  epsilon <- t(structure(rnorm(n*dim, mean = 0, sd = points_sd),dim=c(dim,n)))

  # the points as matrix
  points <- points_origin + epsilon

  # format the data to be output as tibble
  colnames(points) <- colnames
  data <- tibble::as_tibble(points)

  # for checking purposes we may include the clusters in the data set.
  # Thus we basically return clustered data
  if(include_cluster) data <- data |> mutate(cluster = selected_clusters)

  return(data)
}



#' Nonspherical cluster data generator
#'
#'
#' @param n               a positive integer. The number of total data points to be generated.
#' @param list_of_paths   a list containing tibbles. Each tibble containing points (in rows)
#'                  to be interpreted as paths along which the data is generated
#'
#' @returns a tibble, every row representing a data point.
#' @export
generateClusterDataFromPaths <-  function(n=100,
                                          list_of_paths,
                                          clusters_sd = NULL,
                                          clusters_prob = rep(1,length(list_of_paths)),
                                          include_cluster = FALSE,
                                          .print_info = FALSE){


  stopifnot('list_of_paths must be a list of tibbles' = typeof(list_of_paths) == 'list')
  stopifnot('list_of_paths must be a list of tibbles' = all(list_of_paths |> sapply(class) > 1))
  cluster_amount <- length(list_of_paths)
  stopifnot('list_of_paths must not not be an empty list' = cluster_amount > 0)
  stopifnot('paths must not be empty' = all(list_of_paths |> sapply(nrow) > 0) )
  dim <- ncol(list_of_paths[[1]])
  col_names <- colnames(list_of_paths[[1]])
  stopifnot('all paths must feature data points of the same dimension' = all(list_of_paths |> sapply(ncol) == dim))
  stopifnot('all paths must have the same columnnames' = all(list_of_paths |> sapply(function(x) colnames(x)==col_names)))
  stopifnot('there must be as many clusters probability weights as there ar paths' = length(clusters_prob) == cluster_amount)

  # defer the mathematical paths from the tibbles
  paths <-lapply(list_of_paths,tibbleAsPath)


  # if no standard deviations for the clusters specified, randomize them here
  if(base::missing(clusters_sd))
    clusters_sd <- stats::runif(cluster_amount, min=0.001, max=0.01)

  if(.print_info) print(paste0('clusters_sd: ',paste0(clusters_sd)))
  stopifnot('length of clusters_sd does not match cluster amount' = length(clusters_sd) == cluster_amount)



  # for every data point select a cluster according to their probability weights
  selected_clusters <- base::sample(1:cluster_amount, size = n, replace = TRUE, prob = clusters_prob)

  # using the selected clusters we relate the points with an origin
  points_origin <- t(sapply(selected_clusters, function(which_cluster) unlist(paths[[which_cluster]](stats::runif(1,0,1)))))

  # using the selected clusters we relate the points with a standard deviation
  points_sd <-  rep(clusters_sd[selected_clusters],each=dim)
  # the deviation to add to the cluster centers
  epsilon <- t(structure(rnorm(n*dim, mean = 0, sd = points_sd),dim=c(dim,n)))

  # the points as matrix
  points <- points_origin + epsilon

  # for checking purposes we may include the clusters in the data set.
  # Thus we basically return clustered data
  if(include_cluster) data <- data |> mutate(cluster = selected_clusters)

  return(data)
}



#' Generate a full (rectangular) space of random (noise) data
#'
#' @export
generateFullTestData <- function(n=100,
                                 lower_bounds,
                                 upper_bounds,
                                 colnames=paste0('X_',1:length(lower_bounds))){

  dim <- length(lower_bounds)

  stopifnot('lower_bounds and upper bounds must not be empty' = length(dim) > 0)
  stopifnot('lower_bounds and upper_bounds must have the same length'= length(min) == length(max))
  stopifnot('lower bounds must not be higher than upper bounds' = all(lower_bounds <= upper_bounds))
  stopifnot('the number of column names must match the dimension of the bounds' = length(colnames) == dim)

  points <- 1:dim |> sapply(function(i) runif(n,min=lower_bounds[[i]],max = lower_bounds[[i]])) |>
    tibble::as_tibble(.name_repair = 'minimal')

  colnames(points) <- colnames

  return(points)
}

#################### Deprecated ######################

#' spherical test data generator
#'
#'
#'
#' @param n           a positive integer. The number of total data points to be generated.
#' @param n_clusters  a positive integer. The number of clusters to generate.
#'                    If none given, choose a random amount < sqrt(n).
#'
#' @returns a tibble, every row representing a data point.
#' @export
generateClusterTestDataSimple2D = function(n=100,n_clusters=NULL){
  warning('This function has been superseded by generateClusterTestDataSimple')

  if(base::missing(n_clusters)){
    n_clusters <- base::floor(stats::runif(1,min = 1, max = 2*base::sqrt(n)))
  }

  x_clusters <- stats::runif(n_clusters, min=0, max=1)
  y_clusters <- stats::runif(n_clusters, min=0, max=1)
  sd_clusters <- stats::runif(n_clusters, min=0.001, max=0.1)



  test_data = tibble::tibble(selected_clusters = base::floor(stats::runif(n,1,n_clusters+1))) |>
    dplyr::rowwise() |>
    dplyr::mutate(X = stats::rnorm(1,x_clusters[selected_clusters],sd_clusters[selected_clusters]),Y = stats::rnorm(1,y_clusters[selected_clusters],sd_clusters[selected_clusters])) |>
    dplyr::select(X,Y) |>
    dplyr::ungroup()


  return(test_data)
}


#' Nonspherical test data generator
#'
#' @param n               a positive integer. The number of total data points to be generated.
#' @param list_of_paths   a list containing tibbles. Each tibble containing points (in rows)
#'                  to be interpreted as paths along which the data is generated
#'
#' @returns a tibble, every row representing a data point.
#' @export
generateClusterTestData2DFromPaths <-  function(n=100,list_of_paths){
  warning('generateClusterTestData2DFromPaths has been depcated. Use generateClusterDataFromPaths')

  stopifnot('list_of_paths must be a list of tibbles' = typeof(list_of_paths) == 'list')
  stopifnot('list_of_paths must be a list of tibbles' = all(list_of_paths |> sapply(class) > 1))
  cluster_amount <- length(list_of_paths)
  stopifnot('list_of_paths must not not be an empty list' = cluster_amount > 0)
  stopifnot('paths must not be empty' = all(list_of_paths |> sapply(nrow) > 0) )
  dim <- ncol(list_of_paths[[1]])
  stopifnot('Paths must be 2D' = dim==2)
  col_names <- colnames(list_of_paths[[1]])
  stopifnot('all paths must feature data points of the same dimension' = all(list_of_paths |> sapply(ncol) == dim))
  stopifnot('all paths must have the same columnnames' = all(list_of_paths |> sapply(function(x) colnames(x)==col_names)))


  cluster_amount <- length(list_of_paths)
  paths <-lapply(list_of_paths,tibbleAsPath)
  cluster_variances <- stats::runif(cluster_amount, min=0.001, max=0.02)

  points <- base::floor(stats::runif(n,1,cluster_amount+1)) |>
    sapply(function(which_cluster) unlist(paths[[which_cluster]](stats::runif(1,0,1))+
                                            tibble::tibble(X=stats::rnorm(1,0,cluster_variances[which_cluster]),
                                                           Y=stats::rnorm(1,0,cluster_variances[which_cluster])))) |>
    t() |>
    tibble::as_tibble(.name_repair = 'minimal')

  colnames(points) <- col_names

  return(points)
}

#viewData(connected_circles_data)
#
#spectralReduction(concentric_circles_data,gamma=1,k=2)
#viewData(spectralProjection(two_concentric_circles,gamma=60,k=1)$projected_data)

#viewData(spectralProjection(three_concentric_circles,gamma=20,k=3)$projected_data)
#kMeans(spectralReduction(connected_circles_data,gamma=50,k=3)$reduced_data,K=2,tries=5)

#projected_data <- spectralProjection(three_concentric_circles,gamma=25,k=1)$projected_data
#kMeans(projected_data,K=3,tries=10)

#spectralClustering(three_connected_concentric_circles,k=1,gamma=50,cluster_algorithm = 'K-Means',K=3,tries=10)
