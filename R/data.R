############# study courses ##############

#' Some names of study courses
#'
#' An atomic character vector containing names of study courses. Beware: The
#' order of the entries does matter, as study_courses_dissimilarity_matrix has
#' been constructed using this order.
'study_courses_names'

#' A dissimilarity matrix of study courses
#'
#' A matrix encoding the dissimilarties/distances of study courses.
#' The names are given by study_courses_names at construction.
'study_courses_dissimilarity_matrix'

#' Some Study courses
#'
#' A tibble with only one column. Its entries are unique study courses.
'study_courses_data'



################### hours #########################

#' Hours of a day
#'
#' A tibble with only one column. Its entries are the hours of a day: 0, 1, ... 22, 23.
'hours_of_a_day'

#' A full day of data points
#'
#' A tibble with only one column. Its entries are in hours: From 0 to 24. Fractions possible!
#' The data points values have been uniformly randomized
'hours_full_data'

#' clusters in hour data points
#'
#' A tibble with only one column. Its entries are in hours: From 0 to 24. Fractions possible!
#' The data points values have been determined according to randomly distributed clusters.
'hours_data'

############## pacman ####################

#' A pacman grid
#'
#' Data with all possible positions of dots on a pacman board.
#' @format A tibble with two columns
#' \describe{
#' \item{X}{The X axis: Integer values from 1 to 26}
#' \item{Y}{The Y axis: Integer values from 1 to 30}
#' }
'pacman_full_data'

#' Oh No! I've spilled my Pacman dots!
#'
#' Data with data points representing data points on a pacman grid.
#' The data points have been distributed randomly using clusters.
#'
#' @format A tibble with two columns
#' \describe{
#' \item{X}{The X axis: Integer values from 1 to 26}
#' \item{Y}{The Y axis: Integer values from 1 to 30}
#' }
'oh_no_ive_spilled_my_pacman_dots'


############## concentric circles #################

#' Two concentric Circles
#'
#' @family cluster data examples
#'
#' @format A tibble with two columns
#' \describe{
#' \item{X}{Numeric values on the X axis}
#' \item{Y}{Numeric values on the Y axis}
#' }
#'
'two_concentric_circles'

#' Two connected concentric circles
#'
#' Two concentric circles like [two_concentric_circles] but connected by a vertical line.
#' This example data set makes [optics()] and [dbscan()] \strong{fail},
#' where as they would succeed in clustering the [two_concentric_circles] data set.
#'
#' @family cluster data examples
#'
#' @format A tibble with two columns
#' \describe{
#' \item{X}{Numeric values on the X axis}
#' \item{Y}{Numeric values on the Y axis}
#' }
#'
'two_connected_concentric_circles'

#' Three concentric Circles
#'
#' @family cluster data examples
#'
#' @format A tibble with two columns
#' \describe{
#' \item{X}{Numeric values on the X axis}
#' \item{Y}{Numeric values on the Y axis}
#' }
#'
'three_concentric_circles'


#' Three connected concentric circles
#'
#' Three concentric circles like [three_concentric_circles] but connected by a vertical line.
#' This example data set makes [optics()] and [dbscan()] \strong{fail},
#' where as they would succeed in clustering the [three_concentric_circles] data set.
#'
#' @family cluster data examples
#'
#' @format A tibble with two columns
#' \describe{
#' \item{X}{Numeric values on the X axis}
#' \item{Y}{Numeric values on the Y axis}
#' }
#'
'three_connected_concentric_circles'



