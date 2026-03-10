############ Test Data Generators ######################

#' Spherical cluster data generator
#' @family cluster data generators
#'
#' @param n           A positive integer. The number of total data points to be
#'   generated.
#' @param cluster_amount  A positive integer. The number of clusters to
#'   generate. If none given, choose a random amount less than both \eqn{2^{dim}} and \eqn{\frac{n}{4}}.
#' @param dim A positive integer. The dimension of the data points to be
#'   generated
#' @param lower_bounds,upper_bounds numeric vectors with length equal dim (if
#'   given). Bound the cluster centers in a \code{dim}-dimensional rectangle defined
#'   by these parameters. Beware: These bound the cluster centers and \strong{not}
#'   the data points!
#' @param clusters_mean A tibble with every row representing a center of a
#'   cluster. The column amount must match the dimensions implied by \code{dim} or the
#'   bounds.
#' @param clusters_sd A numeric vector with length matching the \code{cluster_amount}
#'   or number of rows in \code{clusters_mean}. The standard deviations used in the
#'   generation of data points around the cluster centers (using a symmetrical
#'   normal distribution).
#' @param clusters_prob A numeric vector with length matching the cluster_amount
#'   or number of rows in \code{clusters_mean}. The probability weights used to choose
#'   which which cluster a generated data point belongs to.
#' @param colnames A character vector with length matching dim or the length of
#'   the bounds. Default is `'X_1'` to `'X_n'`
#' @param include_cluster A logical of length 1. If \code{TRUE} a column `'cluster'`
#'   will be included in the generated data which will also be assigned the class `'clustered_data'`.
#' @param .print_info A logical of length 1. If \code{TRUE} additional information
#'   will be displayed during runtime. Used in debugging.
#'
#' @returns a tibble, every row representing a data point. The columns are named using the parameter `colnames`.
#'
#' @examples
#' generateClusterData(cluster_amount = 4)
#' generateClusterData(n = 1000,
#'                     clusters_mean =
#'                        tibble::tibble(longitude = c(-100,-60,10,20,70,130),
#'                                       latitude =  c(  40,-10,50,20,50,-20)),
#'                     clusters_sd = c(8,8,3,6,15,5),
#'                     clusters_prob = c(20,20,10,100,100,10),
#'                     include_cluster = TRUE)
#' generateClusterData(n = 1,
#'                    lower_bounds = -(20:1),
#'                    upper_bounds = 1:20,
#'                    dim = 20,
#'                    .print_info = TRUE)
#'
#' @export
generateClusterData <- function(n=100,
                                cluster_amount = NULL,
                                dim = 2,
                                lower_bounds = c(0,0),
                                upper_bounds = c(1,5),
                                clusters_mean = NULL,
                                clusters_sd = NULL,
                                clusters_prob = NULL,
                                colnames = NULL,
                                include_cluster = FALSE,
                                .print_info = FALSE){
  if(.print_info) print('starting generator full of hope')

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
      else if(!base::missing(clusters_mean))
        dim <- ncol(clusters_mean)

      # reminder: if no other parameters provided then the default is still 2
    }
    # from here on we are certain we have a value for dim
    if(.print_info) print(paste0('got value for dim:', dim))



    # lets defer missing parameters
    if(base::missing(lower_bounds))
      lower_bounds <- rep(0,dim)
    if(base::missing(upper_bounds))
      upper_bounds <- rep(1,dim)
  }
  if(.print_info) {
    print('got value for lower_bounds (Only important, if clusters_mean not specified):')
    print(lower_bounds)
    print('got value for upper_bounds (Only important, if clusters_mean not specified):')
    print(upper_bounds)
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

  # if no cluster amount specified, determine a cluster amount based on other parameters
  if(base::missing(cluster_amount)){

    if(!base::missing(clusters_mean))
      cluster_amount <- base::nrow(clusters_mean)
    else if(!base::missing(clusters_sd))
      cluster_amount <- base::length(clusters_sd)
    else if(!base::missing(clusters_prob))
      cluster_amount <- base::length(clusters_prob)
    else{
      # if not deferable by other parameters: choose a random amount (dependent on the dimension and number of data points)
      cluster_amount <- base::floor(stats::runif(1,min = 1, max = min(ceiling(n/4) , (2^dim))))
    }
  }
  # from here on we are certain we have a value for cluster_amount
  if(.print_info) print(paste0('got value for cluster_amount:', cluster_amount))

  # if no standard deviations for the clusters specified, randomize them here
  if(base::missing(clusters_sd))
    clusters_sd <- stats::runif(cluster_amount, min=0.001, max=0.5)

  if(.print_info) {
    print('clusters_sd: ')
    print(clusters_sd)}
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

  if(.print_info) print('clusters selected!')

  # using these selected clusters we relate the points with a cluster center
  points_origin <- clusters_mean[selected_clusters,]

  # using these selected clusters we relate the points with a standard deviation
  points_sd <-  rep(clusters_sd[selected_clusters],each=dim)
  # the deviation to add to the cluster centers
  epsilon <- t(structure(rnorm(n*dim, mean = 0, sd = points_sd),dim=c(dim,n)))

  # the points as matrix
  data <- points_origin + epsilon

  if(.print_info) print('points calculated!')

  # format the data to be output as tibble
  colnames(data) <- colnames
  data <- tibble::as_tibble(data)

  # for checking purposes we may include the clusters in the data set.
  # Thus we basically return clustered data
  if(include_cluster){
    data <- data |> dplyr::mutate(cluster = selected_clusters)
    class(data) <- c('clustered_data',class(data))
  }

  if(.print_info) print('data formated! All Done!')

  return(data)
}


#' Nonspherical cluster data generator
#'
#' @family cluster data generators
#'
#' @param n               a positive integer. The number of total data points to
#'   be generated.
#' @param list_of_paths   a list containing tibbles. Each tibble containing
#'   points (in rows) to be interpreted as paths along which the data is
#'   generated
#' @param clusters_sd A numeric vector with the same length as \code{list_of_paths}.
#'   The standard deviations used in the generation of data points around the
#'   clusters (using a symmetrical normal distribution).
#' @param clusters_prob A numeric vector with the same length as \code{list_of_paths}.
#'   The probability weights used to choose which which cluster a generated data
#'   point belongs to.
#' @param include_cluster A logical of length 1. If \code{TRUE} a column `'cluster'`
#'   will be included in the generated data which will also be assigned the class `'clustered_data'`.
#' @param .print_info A logical of length 1. If \code{TRUE} additional information
#'   will be displayed during runtime. Used in debugging.
#'
#'
#' @returns a tibble, every row representing a data point. The columns are derived from the entries in `'list_of_paths'`
#'
#' @examples
#' generateClusterDataFromPaths(list_of_paths =
#'                                 list(tibble::tibble(X = c(0,1,1),
#'                                                     Y = c(0,0,1)),
#'                                      tibble::tibble(X = c(0,0.2),
#'                                                     Y = c(1,0.8))))
#' generateClusterDataFromPaths(n = 1000,
#'                              list_of_paths =
#'                                 list(tibble::tibble(X = c(0),
#'                                                     Y = c(0)),
#'                                      tibble::tibble(X = c(1),
#'                                                     Y = c(1))),
#'                              clusters_sd = c(1,10),
#'                              clusters_prob = c(1,100),
#'                              include_cluster = TRUE,
#'                              .print_info = TRUE)
#'
#'
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
  # from here on we are certain we have a value for cluster_amount
  if(.print_info) print(paste0('got value for cluster_amount:', cluster_amount))

  stopifnot('list_of_paths must not not be an empty list' = cluster_amount > 0)
  stopifnot('paths must not be empty' = all(list_of_paths |> sapply(nrow) > 0) )
  dim <- ncol(list_of_paths[[1]])
  col_names <- colnames(list_of_paths[[1]])
  if(.print_info) {
    print('column names: ')
    print(col_names)}

  stopifnot('all paths must feature data points of the same dimension' = all(list_of_paths |> sapply(ncol) == dim))
  stopifnot('all paths must have the same columnnames' = all(list_of_paths |> sapply(function(x) colnames(x)==col_names)))
  stopifnot('there must be as many clusters probability weights as there ar paths' = length(clusters_prob) == cluster_amount)

  # defer the mathematical paths from the tibbles
  paths <-lapply(list_of_paths,tibbleAsPath)


  # if no standard deviations for the clusters specified, randomize them here
  if(base::missing(clusters_sd))
    clusters_sd <- stats::runif(cluster_amount, min=0.001, max=0.01)

  if(.print_info) {
    print('clusters_sd: ')
    print(clusters_sd)}
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
  data <- points_origin + epsilon

  if(.print_info) print('points calculated!')

  # format the data to be output as tibble
  colnames(data) <- col_names
  data <- tibble::as_tibble(data)

  # for checking purposes we may include the clusters in the data set.
  # Thus we basically return clustered data
  if(include_cluster) {data <- data |> dplyr::mutate(cluster = selected_clusters)
  class(data) <- c('clustered_data',class(data))
  }

  return(data)
}


#' Generate a full (rectangular) space of random (noise) data
#' @family cluster data generators
#'
#' @param n           A positive integer. The number of total data points to be
#'   generated.
#' @param lower_bounds,upper_bounds numeric vectors with length equal dim (if
#'   given). Bound the cluster centers in a \code{dim}-dimensional rectangle
#'   defined by these parameters. Beware: These bound the cluster centers and
#'   \strong{not} the data points!
#' @param colnames A character vector with length matching dim or the length of
#'   the bounds. Default is `'X_1'` to `'X_n'`.
#' @param include_cluster A logical of length 1. If \code{TRUE} a column
#'   `'cluster'` will be included in the generated data which will also be
#'   assigned the class `'clustered_data'`. Because this is a noise generator
#'   all data points will be assigned cluster \code{0}.
#' @param .print_info A logical of length 1. If \code{TRUE} additional
#'   information will be displayed during runtime. Used in debugging.
#'
#' @returns a tibble, every row representing a data point. The columns are named
#'   using the parameter `colnames`.
#'
#' @examples
#' generateNoiseData(lower_bounds = c(-1,0),
#'                   upper_bounds = c(1,1))
#' generateNoiseData(n = 1000,
#'                   lower_bounds = c(-180,-90),
#'                   upper_bounds = c(180,90),
#'                   colnames = c('longitude','latitude'),
#'                   .print_info = TRUE)
#'
#'
#'
#' @export
generateNoiseData <- function(n=100,
                              lower_bounds,
                              upper_bounds,
                              colnames = paste0('X_',1:length(lower_bounds)),
                              include_cluster = FALSE,
                              .print_info = FALSE){

  dim <- length(lower_bounds)

  if(.print_info) print(paste0('got value for dim:', dim))


  stopifnot('lower_bounds and upper bounds must not be empty' = length(dim) > 0)
  stopifnot('lower_bounds and upper_bounds must have the same length'= length(min) == length(max))
  stopifnot('lower bounds must not be higher than upper bounds' = all(lower_bounds <= upper_bounds))
  stopifnot('the number of column names must match the dimension of the bounds' = length(colnames) == dim)

  if(.print_info) print('starting main loop')

  data <- 1:dim |> sapply(function(i) runif(n,min=lower_bounds[[i]],max = upper_bounds[[i]])) |>
    tibble::as_tibble(.name_repair = 'minimal')

  if(.print_info) print('formating output')

  colnames(data) <- colnames

  if(include_cluster) {data <- data |> dplyr::mutate(cluster = 0)
  class(data) <- c('clustered_data',class(data))
  }

  return(data)
}

#################### Deprecated ######################

#' spherical test data generator
#'
#' DEPRECATED in favor of generateClusterTestDataSimple()
#'
#' @family cluster data generators
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
#' DEPRECATED in favor of generateClusterDataFromPaths()
#'
#'
#' @family cluster data generators
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
