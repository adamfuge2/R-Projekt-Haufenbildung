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

  n_clusters <- length(list_of_paths)
  paths <-lapply(list_of_paths,tibbleAsPath)
  sd_clusters <- stats::runif(n_clusters, min=0.001, max=0.02)
  points <- tibble::tibble(X=numeric(),Y=numeric())


  for(i in 1:n){
    which_cluster <- base::floor(stats::runif(1,1,n_clusters+1))
    points <- tibble::add_row(points,paths[[which_cluster]](stats::runif(1,0,1))+
                                tibble::tibble(X=stats::rnorm(1,0,sd_clusters[which_cluster]), Y=stats::rnorm(1,0,sd_clusters[which_cluster])))
  }


  return(points)
}

generateFullTestData <- function(n=100,min,max){
  stopifnot('min and max must have the same length'= length(min) == length(max))

  values <- numeric()
  for(i in 1:length(min)){
    values <- c(values,runif(n,min=min[i],max = max[i]))
  }

  tibble::as_tibble(matrix(values,ncol =length(min)))
}

#circle <- function(r) tibble::tibble(X=c(r,r/sqrt(2),0,-r/sqrt(2),-r,-r/sqrt(2),0,r/sqrt(2),r),Y=c(0,r/sqrt(2),r,r/sqrt(2),0,-r/sqrt(2),-r,-r/sqrt(2),0))
#
#circles <- list(circle(0.5),circle(1),circle(1),circle(1))
#connected_circles <- list(circle(0.5),circle(0.5),circle(0.5),circle(1),circle(1),circle(1),circle(1),circle(1),circle(1),circle(1),circle(1),circle(1),tibble::tibble(X=c(0,0),Y=c(0.5,1)))
#
#circles_data <- generateClusterTestData2DFromPaths(n=500,list_of_paths =  circles)
#connected_circles_data <- generateClusterTestData2DFromPaths(n=500,list_of_paths =  connected_circles)
#
#viewData(connected_circles_data)
#
#spectralReduction(concentric_circles_data,gamma=1,k=2)
#viewData(spectralReduction(connected_circles_data,gamma=60,k=1)$reduced_data)
#kMeans(spectralReduction(connected_circles_data,gamma=50,k=3)$reduced_data,K=2,tries=5)

###################### premade data #####################

hours_of_a_day <- tibble::as_tibble(0:23,.name_repair = 'minimal')
hours_data <- tibble::as_tibble((24*generateClusterTestDataSimple(dim=1,cluster_amount = 4,n=200))%%24,.name_repair = 'minimal')
more_hours_data <- generateFullTestData(n=100,min = c(-180,-90), max = c(180,  90))

oh_no_ive_spilled_my_pacman_dots <- dplyr::mutate(generateClusterTestDataSimple(dim=2,cluster_amount = 5,n=200), X_1=X_1%%1*26, X_2=X_2%%1*30)

more_pacman_dots <- generateFullTestData(n=100,min = c(0,0), max = c(26,30))
