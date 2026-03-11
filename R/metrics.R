######################### metrics #################################
##
## A metric is a function measuring the
## distance/closeness/inequality/relatedness/etc...
## for every metric here the following must (approximately) hold:
## 1. metric(x,y) = 0  if and only if x = y
## 2. metric(x,y) = metric(y,x)
##
## Inputs:
## x,         an atomic vector or tibble row.
## y,         the same type as x.
##
## Returns:
## numeric,   a real number >= 0.



#' The standard euclidean distance
#'
#' @param x         an atomic vector or tibble row with only real numbers
#' @param y         an atomic vector or tibble row with only real numbers
#'
#' @returns numeric,   a real number >= 0.
#'
#' @examples
#' euclidean(3,5)
#' euclidean(c(3,0,34),c(-5,30,31))
#' euclidean(tibble::tibble(X =  3, Y =  0, Z = 34),
#'           tibble::tibble(X = -5, Y = 30, Z = 31))
#'
#' @export
euclidean <- function(x,y) sqrt(base::sum((x-y)^2))


#' The standard Manhattan, taxi or maximum metric
#'
#' @param x         an atomic vector or tibble row with only real numbers
#' @param y         an atomic vector or tibble row with only real numbers
#'
#' @returns numeric,   a real number >= 0.
#' @export
maximumMetric <- function(x,y) base::max(base::abs(x-y))



#' The standard Lp metric
#'
#'
#' @param p         a real numeric with 1 <= p < Inf
#'
#'
#' @returns a metric (function) with inputs \code{x,y} numerical vectors and a
#'   numerical output, a real number >= 0.
#'
#' @examples
#' pMetric(1)(3,5)
#' pMetric(5)(c(3,0,34),c(-5,30,31))
#' pMetric(1.5)(tibble::tibble(X =  3, Y =  0, Z = 34),
#'           tibble::tibble(X = -5, Y = 30, Z = 31))
#' @export
pMetric <- function(p) {function(x,y) base::sum(base::abs(x-y)^p)^(1/p)}

#' Manhattan/Taxi distance
#'
#' @param x         an atomic vector or tibble row with only real numbers
#' @param y         an atomic vector or tibble row with only real numbers
#'
#' @export
manhattan <- function(x,y) base::sum(base::abs(x-y))

##################### Study courses metric ####################
#' A distance function on study courses
#'
#' The data for this relies on literally nothing.
#' It is purely to serve as an example of an exotic distance function
#'
#' @param x,y   a character of length 1. A studies subject. One of TODO
#'
#' @examples
#' study_courses_distance('mathematics','physics')
#' study_courses_distance('law','geology')
#'
#' @export
study_courses_distance <- function(x,y) study_courses_dissimilarity_matrix[[unlist(x),unlist(y)]]

#viewData(students_data)
#
#
#clustering <- kMedioids(students_data,K=4,custom_metric=studies_difference,.print_info = TRUE)
#
#clustering$clustered_data
#
#data <- students_data
#custom_metric <- studies_difference
#
#silhouette(data,clustering$clustering_function,o=15,custom_metric)








################### pacman distance ##################

#' Pacman distance function
#'
#' a function factory for functions on multidimensional toruses. Meaning 'the edges loop'
#'
#' @param dim_lengths A numeric vector. Describes the widths of the space.
#' @param base_metric A distance function to be used as the base distance on the looped space.
#'
#' @returns A distance function for two data points on a looped multidimensional space
#'
#' @examples
#' pacman_distance(24)(x = 1, y = 23)
#' pacman_distance(dim_lengths = c(26,30),
#'                 base_metric = manhattan)(x = c(5,2), y= c(25,23))
#' @seealso
#'   [hours_distance()] made by `pacman_distance(25)`,
#'   [original_pacman_distance()] made by `pacman_distance(dim_lengths = c(26,30),base_metric = manhattan)`,
#'
#'
#' @export
pacman_distance <- function(dim_lengths,base_metric=euclidean){

  dim <- length(dim_lengths)


  M <- array(base::rep(dim_lengths,dim,each=3^dim),dim=c(3^dim,dim))
  M[row(M)%%(3^col(M)) > 3^(col(M)-1)-1 & row(M)%%(3^col(M)) < 2*3^(col(M)-1)] <- 0
  M[row(M)%%(3^col(M)) >= 2*3^(col(M)-1)] <- M[row(M)%%(3^col(M)) >= 2*3^(col(M)-1)]*-1

  function(x,y) min(apply(M,c(1),function(plus) base_metric(x,y+plus)))
}

#' The distance of hours
#'
#' Calculates the distance of two times of the day in hours. Be aware, this
#' means 23.9 and 0.1 are very close.
#'
#' @param x,y A numeric between 0 and 24, Hour of a time of day as. Fractional
#'   hours like 2.5 or 15.9 are allowed.
#'
#' @export
hours_distance <- pacman_distance(dim_lengths=24)


#' The distance of on a pacman board
#'
#' calculates the distance of two dots on a pacman board (which has dimensions 26*30)
#'
#' @param x,y A numeric vector of length 2. Between c(0,0) and c(26,30).
#'
#' @returns A non negative numeric of length 1
#' @export
original_pacman_distance <- pacman_distance(dim_lengths = c(26,30),base_metric = manhattan)

#clustering <- kMedioids(hours_data,custom_metric = hour_difference,K=3)
#viewClusters(more_hours_data,clustering$clustering_function)
#clustering
#
#clustering <- kMedioids(oh_no_ive_spilled_my_pacman_dots,custom_metric = original_pacman_distance,K=4)
#viewClusters(more_pacman_dots,clustering$clustering_function)
#clustering

#viewData(spectralReduction(oh_no_ive_spilled_my_pacman_dots,1,custom_mercian_kernel = gaussKernelByCustomMetric(original_pacman_distance,gamma = 1),k=3)$reduced_data)
#viewData(spectralReduction(hours_data,gamma=1,k=2,custom_mercian_kernel = gaussKernelByCustomMetric(hours_distance,gamma = 50))$reduced_data)


################## distance on the globe #################
## lets assume the earth was round
##

distanceByLongitudeAndLatitude <- function(x,y){
  x <- unlist(x,use.names = FALSE)[2:1] * 2*pi/360
  y <- unlist(y,use.names = FALSE)[2:1] * 2*pi/360
  round(2*6371000 * asin(sqrt(sin((y[1]-x[1])/2)^2 +
                       cos(x[1]) * cos(y[1]) *
                     sin((y[2]-x[2])/2)^2)),2)
}

#paris <- tibble::tibble(lattitude=48.8566,longitude=2.3522)
#y <- tibble::tibble(lattitude=50.0647,longitude=19.9450)
#
#distanceByLongitudeAndLatitude(x,y)
#
#viewData(dplyr::slice_sample(world.cities[,c('long','lat')],n=100))
#kMedioids(dplyr::slice_sample(world.cities[,c('long','lat')],n=1000),custom_metric = distanceByLongitudeAndLatitude,K=2,.print_info=TRUE)
#slice_of_the_world <- dplyr::slice_sample(world.cities[,c('long','lat')],n=1000)
#the_world <- tibble::as_tibble(matrix(c(runif(1000,min=-180,max = 180),runif(1000,min=-90,max = 90)),ncol = 2))


#clustering <- kMedioids(slice_of_the_world,custom_metric = distanceByLongitudeAndLatitude,K=6,.print_info=TRUE)
#viewClusters(world.cities[,c('long','lat')],clustering$clustering_function)

