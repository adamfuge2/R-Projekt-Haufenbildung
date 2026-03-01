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
pMetric <- function(p) function(x,y) base::sum(base::abs(x-y)^p)^(1/p)

