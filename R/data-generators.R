############ Test Data Generators ######################

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

#' spherical test data generator
#'
#' @param n           a positive integer. The number of total data points to be generated.
#' @param cluster_amount  a positive integer. The number of clusters to generate.
#'                    If none given, choose a random amount < sqrt(n).
#' @param dim A positive integer. The dimension of the data points to be generated
#'
#' @returns a tibble, every row representing a data point.
#' @export
generateClusterTestDataSimple <- function(n=100,cluster_amount=NULL,dim=2){
  if(base::missing(cluster_amount)){
    cluster_amount <- base::floor(stats::runif(1,min = 1, max = 2*base::sqrt(n)))
  }

  cluster_centers <- tibble::tibble(variances=stats::runif(cluster_amount, min=0.001, max=0.1))

  for(i in 1:dim){
    cluster_centers <- cluster_centers |> tibble::add_column(!!paste0('X_',i) := stats::runif(cluster_amount, min=0, max=1))
  }

  test_data <- tibble::tibble(selected_clusters = base::floor(stats::runif(n,1,cluster_amount+1)), variances = cluster_centers$variances[selected_clusters] ) |> dplyr::rowwise()

  for(i in 1:dim){
    test_data <- test_data |> dplyr::mutate(!!paste0('X_',i) := stats::rnorm(1,cluster_centers[[selected_clusters,i+1]],variances))
  }

  test_data <- test_data |> dplyr::select(c(-1,-2)) |> dplyr::ungroup()

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

#' Nonspherical cluster data generator
#'
#'
#' @param n               a positive integer. The number of total data points to be generated.
#' @param list_of_paths   a list containing tibbles. Each tibble containing points (in rows)
#'                  to be interpreted as paths along which the data is generated
#'
#' @returns a tibble, every row representing a data point.
#' @export
generateClusterDataFromPaths <-  function(n=100,list_of_paths){


  stopifnot('list_of_paths must be a list of tibbles' = typeof(list_of_paths) == 'list')
  stopifnot('list_of_paths must be a list of tibbles' = all(list_of_paths |> sapply(class) > 1))
  cluster_amount <- length(list_of_paths)
  stopifnot('list_of_paths must not not be an empty list' = cluster_amount > 0)
  stopifnot('paths must not be empty' = all(list_of_paths |> sapply(nrow) > 0) )
  dim <- ncol(list_of_paths[[1]])
  col_names <- colnames(list_of_paths[[1]])
  stopifnot('all paths must feature data points of the same dimension' = all(list_of_paths |> sapply(ncol) == dim))
  stopifnot('all paths must have the same columnnames' = all(list_of_paths |> sapply(function(x) colnames(x)==col_names)))


  cluster_amount <- length(list_of_paths)
  paths <-lapply(list_of_paths,tibbleAsPath)
  cluster_variances <- stats::runif(cluster_amount, min=0.001, max=0.02)

  points <- base::floor(stats::runif(n,1,cluster_amount+1)) |>
    sapply(function(which_cluster) unlist(paths[[which_cluster]](stats::runif(1,0,1))+
                                          stats::rnorm(dim,0,cluster_variances[which_cluster]))) |>
    t() |>
    tibble::as_tibble(.name_repair = 'minimal')

  colnames(points) <- col_names

  return(points)
}


#' Generate a full (rectangular) space of test data
#'
#' @export
generateFullTestData <- function(n=100,lower_bounds,upper_bounds,colnames=paste0('X_',1:length(lower_bounds))){
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


#viewData(connected_circles_data)
#
#spectralReduction(concentric_circles_data,gamma=1,k=2)
#viewData(spectralReduction(connected_circles_data,gamma=60,k=1)$reduced_data)
#kMeans(spectralReduction(connected_circles_data,gamma=50,k=3)$reduced_data,K=2,tries=5)

