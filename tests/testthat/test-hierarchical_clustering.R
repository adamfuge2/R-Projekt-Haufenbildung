# Hierarchical Clustering
set.seed(13456)
data <- generateClusterData(n=16, cluster_amount = 3, dim = 3)
data_matrix <- as.matrix(data)


test_that("Hierarchical Clustering -> Test Tibbles and Matrices", {
  testthat::expect_no_error(tbl <- hierarchicalClustering(data, 2))
  testthat::expect_no_error(mat <- hierarchicalClustering(data_matrix, 2))
  testthat::expect_equal(tbl$clustered_data, mat$clustered_data)
})

test_that("Hierarchical Clustering -> Clustering function", {
  data <- generateClusterData(n=100,
                              clusters_mean = tibble::tibble('X'=c(0.4,0.4,0.8),
                                                             'Y'=c(0.7,0.1,0.7)),
                              clusters_sd=c(0.01,0.02,0.01),
                              cluster_amount = 3)

  clustering <- hierarchicalClustering(data,exact_cluster_amount =3)

  clustering_f <- clustering$clustering_function
  testthat::expect_false(clustering_f(c(0.4,0.7)) == clustering_f(c(0.8,0.7)))
  testthat::expect_false(clustering_f(c(0.4,0.1)) == clustering_f(c(0.8,0.7)))
  testthat::expect_false(clustering_f(c(0.4,0.7)) == clustering_f(c(0.4,0.1)))
})


test_that("Hierarchical Clustering -> Linkage Modes", {
  testthat::expect_no_error(hierarchicalClustering(data, 2, mode = "centroid"))
  testthat::expect_no_error(hierarchicalClustering(data, 2, mode = "single"))
  testthat::expect_no_error(hierarchicalClustering(data, 2, mode = "complete"))
  testthat::expect_no_error(hierarchicalClustering(data, 2, mode = "average"))
})


test_that("Hierarchical Clustering -> Distance Methods", {
  testthat::expect_no_error(hierarchicalClustering(data, 2, distance_method = "euclidean"))
  testthat::expect_no_error(hierarchicalClustering(data, 2, distance_method = "maximum"))
  # the next one fails as we have not provided a value for p
  testthat::expect_error(hierarchicalClustering(data, 2, distance_method = "minkowski"))
  testthat::expect_no_error(hierarchicalClustering(data, 2, distance_method = "maximum", p = 4))
  testthat::expect_no_error(hierarchicalClustering(data, 2, distance_method = "manhattan"))
  testthat::expect_no_error(hierarchicalClustering(data, 2, custom_distance_function=function(x,y) as.numeric(!identical(x,y))))
})


test_that("Hierarchical Clustering -> Different Amounts of Clusters", {
  testthat::expect_no_error(hierarchicalClustering(data, exact_cluster_amount = 1))
  testthat::expect_no_error(hierarchicalClustering(data, exact_cluster_amount = 5))
})


test_that("Hierarchical Clustering -> Too Many Clusters", {
  testthat::expect_error(hierarchicalClustering(data, 25))
  #print("Hierarchical Clustering has been tested!")
})

test_that("Hierarchical Clustering -> Different cluster amount methods", {
  testthat::expect_no_error(hierarchicalClustering(data))
  testthat::expect_no_error(hierarchicalClustering(data, exact_cluster_amount=5))
  testthat::expect_no_error(hierarchicalClustering(data, min_cluster_amount=5))
  testthat::expect_no_error(hierarchicalClustering(data, distance_limit=10))
})


test_that("Hierarchical Clustering -> misc inputs", {
  hierarchicalClustering(data) |>
    expect_no_error() |>
    capture_output(print=FALSE)
})
