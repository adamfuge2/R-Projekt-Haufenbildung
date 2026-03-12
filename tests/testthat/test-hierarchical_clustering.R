# Hierarchical Clustering
set.seed(13456)
data <- generateClusterData(n=16, cluster_amount = 3, dim = 3)
data_matrix <- as.matrix(data)


test_that("Hierarchical Clustering -> Test Tibbles and Matrices", {
  testthat::expect_no_error(tbl <- hierarchicalClustering(data, 2))
  testthat::expect_no_error(mat <- hierarchicalClustering(data_matrix, 2))
  testthat::expect_equal(tbl$clustered_data, mat$clustered_data)
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
  testthat::expect_error(hierarchicalClustering(data, 2, distance_method = "Lp"))
  testthat::expect_no_error(hierarchicalClustering(data, 2, distance_method = "maximum", p = 4))
  testthat::expect_no_error(hierarchicalClustering(data, 2, distance_method = "manhattan"))
  testthat::expect_no_error(hierarchicalClustering(data, 2, custom_distance_function=function(x,y) as.numeric(!identical(x,y))))
})


test_that("Hierarchical Clustering -> Different Amounts of Clusters", {
  testthat::expect_no_error(hierarchicalClustering(data, 1))
  testthat::expect_no_error(hierarchicalClustering(data, 5))
})


test_that("Hierarchical Clustering -> Too Many Clusters", {
  testthat::expect_error(hierarchicalClustering(data, 25))
  #print("Hierarchical Clustering has been tested!")
})
