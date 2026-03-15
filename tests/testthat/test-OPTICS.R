test_that("OPTICS works", {

  set.seed(123)

  data <- generateClusterData(n = 100)

  result <- optics(data, epsilon = 0.1, min_Pts = 5)

  testthat::expect_s3_class(result,"optics")

  testthat::expect_equal(length(result$order), nrow(data))
  testthat::expect_equal(length(result$reachdist), nrow(data))
  testthat::expect_equal(length(result$coredist), nrow(data))

})

test_that("OPTICS accepts different parameters", {

  set.seed(123)

  data <- generateClusterData(n = 100)

  testthat::expect_no_error(optics(data, epsilon = 0.05, min_Pts = 3))
  testthat::expect_no_error(optics(data, epsilon = 0.2, min_Pts = 10))
  testthat::expect_no_error(optics(data, epsilon = 0.5, min_Pts = 2))

})


test_that("OPTICS handles wrong inputs",{

  data <- generateClusterData(n=100)

  expect_error(optics(data,epsilon=-1,min_Pts=5))
  expect_error(optics(data,epsilon=0.1,min_Pts=0))
  expect_error(optics(tibble::tibble(),epsilon=0.1,min_Pts=5))
  expect_error(optics(data,epsilon="abc",min_Pts=5))

})

test_that("OPTICS works with default and custom distance methods", {

  set.seed(123)

  data <- generateClusterData(n=200,
                              clusters_mean = tibble::tibble('X'=c(0.4,0.4,0.8),
                                                             'Y'=c(0.7,0.1,0.7)),
                              clusters_sd=c(0.01,0.02,0.01),
                              cluster_amount = 3)

  testthat::expect_no_error(optics(data, epsilon=0.3, min_Pts=3))
  testthat::expect_no_error(optics(data, epsilon=0.3, min_Pts=3, distance_method = 'euclidean'))
  testthat::expect_no_error(optics(data, epsilon=0.3, min_Pts=3, distance_method = 'maximum'))
  testthat::expect_no_error(optics(data, epsilon=0.3, min_Pts=3, distance_method = 'minkowski', p=1.5))
  testthat::expect_no_error(optics(data, epsilon=0.3, min_Pts=3, distance_method = 'minkowski', p=10))
  testthat::expect_no_error(optics(data, epsilon=0.3, min_Pts=3, distance_method = 'manhattan'))

  dist_fun <- function(a,b) 1
  testthat::expect_no_error(optics(data, epsilon=0.3, min_Pts=3, distance_method = 'custom', custom_distance_function = dist_fun))

})

test_that("OPTICS handles points without core distance", {

  set.seed(123)

  data <- generateClusterData(n = 20)

  result <- optics(data, epsilon = 0.0001, min_Pts = 10)
  testthat::expect_true(any(is.na(result$coredist)))

})

## Testing optics-reachability.R

test_that("as.reachability works", {

 data <- generateClusterData(n = 100)

  result <- optics(data, epsilon=0.1, min_Pts=3)
  testthat::expect_s3_class(result,"optics")

  res_reach <- as.reachability(result)
  testthat::expect_s3_class(res_reach, "reachability")
  testthat::expect_s3_class(as.reachability(result), "reachability")

})

test_that("extractClusters works for optics and reachability objects", {

  data <- generateClusterData(n = 100)

  result <- optics(data, epsilon=0.15, min_Pts=3)

  testthat::expect_s3_class(result,"optics")
  testthat::expect_no_error(extractClusters(result, epsilon=0.1))
  clusters_optics <- extractClusters(result, epsilon=0.1)
  testthat::expect_true(all(clusters_optics >= 0))

  res_reach <- as.reachability(result)
  testthat::expect_s3_class(res_reach, "reachability")
  testthat::expect_no_error(extractClusters(res_reach, epsilon=0.1))
  clusters_reach <- extractClusters(res_reach, epsilon=0.1)
  testthat::expect_true(all(clusters_reach >= 0))

})

test_that("plot works for optics and reachability objects", {

  data <- generateClusterData(n = 100)

  result <- optics(data, epsilon=0.2, min_Pts=3)

  testthat::expect_s3_class(result,"optics")
  testthat::expect_no_error(plot(result, epsilon=0.1))

  res_reach <- as.reachability(result)
  testthat::expect_s3_class(res_reach, "reachability")
  testthat::expect_no_error(plot(res_reach, epsilon=0.1))

  testthat::expect_no_error(plot(result))

})
