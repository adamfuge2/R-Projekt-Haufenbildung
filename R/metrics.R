######################### metrics #################################
##
## A metric is a function measuring the
## distance/closeness/inequality/relatedness/etc...
## for every metric here the following must (approximately) hold:
## 1. metric(x,y) = 0  if and only if x = y
## 2. metric(x,y) = metric(y,x)
## 3. metric(x,z) <= metric(x,y) + metric(y,z)
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
#' @export
pMetric <- function(p) {function(x,y) base::sum(base::abs(x-y)^p)^(1/p)}

manhattan <- function(x,y) base::sum(base::abs(x-y))

##################### Study courses metric ####################

#' names for some study courses
#' @export
study_curses <- c('architecture',
                  'special education',
                  'english',
                  'physics',
                  'mathematics',
                  'computer science',
                  'biology',
                  'chemistry',
                  'geography',
                  'geology',
                  'greek history',
                  'economics',
                  'egyptoligy',
                  'medical studies',
                  'law',
                  'music',
                  'philosophy',
                  'translation',
                  'theater education')

#' The differences of study courses
#'
#' only upper matrix part. Do not use
studies_differences_upper <- matrix(c(00,15,10,03,03,08,12,13,07,02,03,03,03,16,11,07,20,20,05,
                                     00,00,19,13,09,13,12,19,13,19,21,11,22,20,09,03,05,06,04,
                                     00,00,00,10,16,07,10,04,11,20,19,17,18,04,20,05,08,01,06,
                                     00,00,00,00,02,04,06,03,04,06,16,08,15,06,16,02,15,16,10,
                                     00,00,00,00,00,01,05,03,07,06,15,03,14,06,04,08,03,05,18,
                                     00,00,00,00,00,00,04,05,07,08,16,04,18,05,08,05,15,06,04,
                                     00,00,00,00,00,00,00,05,08,08,19,18,19,01,14,18,08,16,18,
                                     00,00,00,00,00,00,00,00,17,05,10,18,11,03,14,17,20,21,15,
                                     00,00,00,00,00,00,00,00,00,01,10,09,09,12,10,20,09,08,16,
                                     00,00,00,00,00,00,00,00,00,00,02,07,02,19,22,20,24,05,19,
                                     00,00,00,00,00,00,00,00,00,00,00,22,01,18,06,14,01,03,07,
                                     00,00,00,00,00,00,00,00,00,00,00,00,21,08,02,21,15,06,13,
                                     00,00,00,00,00,00,00,00,00,00,00,00,00,10,23,19,20,04,17,
                                     00,00,00,00,00,00,00,00,00,00,00,00,00,00,17,12,04,14,12,
                                     00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,17,04,04,17,
                                     00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,03,06,01,
                                     00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,05,01,
                                     00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,05,
                                     00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00),
                                   ncol = length(study_curses),
                                   dimnames = list(study_curses,study_curses),
                                   byrow = TRUE)

#' Difference matrix of study courses
#'
studies_differences <- studies_differences_upper + t(studies_differences_upper)


#' A difference function on study courses
#'
#' The data for this relies on nothing. It is purely to serve as an example of
#' an exotic distance function
#'
#' @param x         a character. A studies subject. One of:
#' @param y         a character. A studies subject like \code{x}.
#'
#' @export
studies_difference <- function(x,y) studies_differences[[unlist(x),unlist(y)]]

#' Data of some subjects
#'
#' @export
subjects_data <- tibble::as_tibble(study_curses)


viewData(subjects_data)


clustering <- kMedioids(subjects_data,K=6,custom_metric=studies_difference,.print_info = TRUE)

clustering$clustered_data

data <- students_data
custom_metric <- studies_difference

silhouette(data,clustering$clustering_function,o=15,custom_metric)

NULL







################### Hour metric ##################

#' The difference of hours
#'
#' Calculates the difference of two times of the day in hours. Be aware, this
#' means 23.9 and 0.1 are very close.
#'
#' @param x,y A numeric between 0 and 24, Hour of a time of day as. Fractional
#'   hours like 2.5 or 15.9 are allowed.
#'
pacman_distance <- function(dim_lengths,base_metric=euclidean){

  dim <- length(dim_lengths)


  M <- array(base::rep(dim_lengths,dim,each=3^dim),dim=c(3^dim,dim))
  M[row(M)%%(3^col(M)) > 3^(col(M)-1)-1 & row(M)%%(3^col(M)) < 2*3^(col(M)-1)] <- 0
  M[row(M)%%(3^col(M)) >= 2*3^(col(M)-1)] <- M[row(M)%%(3^col(M)) >= 2*3^(col(M)-1)]*-1

  function(x,y) min(apply(M,c(1),function(plus) base_metric(x,y+plus)))
}

hours_distance <- pacman_distance(dim_lengths=24)

cylinder_distance <- function(radius,height) pacman_distance(dim_lengths = c(radius,height))

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

