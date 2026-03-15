test_that("DBSCAN works", {
  set.seed(123)

  data <- generateClusterData(n = 100)

  clustering <- dbscan(data, epsilon=0.1, min_Pts=5)
  clustering_f <- clustering$clustering_function

  testthat::expect_s3_class(clustering, "dbscan")
  testthat::expect_true("cluster" %in% names(clustering$clustered_data))
  testthat::expect_equal(nrow(clustering$clustered_data), nrow(data))
  testthat::expect_true(is.numeric(clustering$clustered_data$cluster) || is.integer(clustering$cluster))
  testthat::expect_true(is.function(clustering_f))
  testthat::expect_true(is.numeric(clustering_f(c(0.4,0.7))))
})

test_that("DBSCAN accepts different parameters", {

  set.seed(123)

  data <- generateClusterData(n = 100)

  testthat::expect_no_error(dbscan(data, epsilon = 0.05, min_Pts = 3))
  testthat::expect_no_error(dbscan(data, epsilon = 0.2, min_Pts = 10))
  testthat::expect_no_error(dbscan(data, epsilon = 0.5, min_Pts = 2))
})

test_that("DBSCAN is deterministic", {

  set.seed(123)

  data <- generateClusterData(n=100)

  res1 <- dbscan(data, epsilon=0.1, min_Pts=5)
  res2 <- dbscan(data, epsilon=0.1, min_Pts=5)

  testthat::expect_equal(res1$cluster, res2$cluster)
})

test_that("DBSCAN handles wrong inputs", {

  data <- generateClusterData(n=100)

  expect_error(dbscan(data, epsilon=-1, min_Pts=5))
  expect_error(dbscan(data, epsilon=0.1, min_Pts=0))
  expect_error(dbscan(tibble::tibble(), epsilon=0.1, min_Pts=5))
  expect_error(dbscan(data, epsilon="abc", min_Pts=5))
})
