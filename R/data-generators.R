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
                                   tibble::tibble(X=stats::rnorm(1,0,cluster_variances[which_cluster]),
                                    Y=stats::rnorm(1,0,cluster_variances[which_cluster])))) |>
    t() |>
    tibble::as_tibble(.name_repair = 'minimal')

  colnames(points) <- col_names

  return(points)
}


#' Generate a full (rectangular) space of test data
#'
#' @export
generateFullTestData <- function(n=100,min,max,colnames=paste0('X_',1:length(min))){
  dim <- length(min)

  stopifnot('min and max must have the same length'= length(min) == length(max))


  points <- 1:dim |> sapply(function(i) runif(n,min=min[[i]],max = max[[i]])) |>
    tibble::as_tibble(.name_repair = 'minimal')

  colnames(points) <- colnames

  return(points)
}

#circle <- function(r) tibble::tibble(X=c(r,r/sqrt(2),0,-r/sqrt(2),-r,-r/sqrt(2),0,r/sqrt(2),r),Y=c(0,r/sqrt(2),r,r/sqrt(2),0,-r/sqrt(2),-r,-r/sqrt(2),0))

#circles <- list(circle(0.5),circle(1),circle(1),circle(1))
#connected_circles <- list(circle(0.5),circle(0.5),circle(0.5),circle(1),circle(1),circle(1),circle(1),circle(1),circle(1),circle(1),circle(1),circle(1),tibble::tibble(X=c(0,0),Y=c(0.5,1)))

#circles_data <- generateClusterTestData2DFromPaths(n=500,list_of_paths =  circles)
#connected_circles_data <- generateClusterTestData2DFromPaths(n=500,list_of_paths =  connected_circles)

#viewData(connected_circles_data)
#
#spectralReduction(concentric_circles_data,gamma=1,k=2)
#viewData(spectralReduction(connected_circles_data,gamma=60,k=1)$reduced_data)
#kMeans(spectralReduction(connected_circles_data,gamma=50,k=3)$reduced_data,K=2,tries=5)

###################### premade data #####################

pacman_full_data <- generateFullTestData(n=100,min = c(0,0), max = c(26,30))


