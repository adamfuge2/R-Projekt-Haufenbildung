test_that("hierarchical_clustering", {
  set.seed(13456)

  data <- generateClusterTestDataSimple(n=16, cluster_amount = 3, dim = 3)
  data_matrix <- as.matrix(data)

  # test if both matrices and tibbles are fine
  testthat::expect_no_error(tbl <- hierarchical_clustering(data, 2))
  testthat::expect_no_error(mat <- hierarchical_clustering(data_matrix, 2))
  testthat::expect_equal(tbl$clustered_data, mat$clustered_data)

  # test all linkage modes
  testthat::expect_no_error(hierarchical_clustering(data, 2, mode = "centroid"))
  testthat::expect_no_error(hierarchical_clustering(data, 2, mode = "single"))
  testthat::expect_no_error(hierarchical_clustering(data, 2, mode = "complete"))
  testthat::expect_no_error(hierarchical_clustering(data, 2, mode = "average"))

  # test different metrics
  testthat::expect_no_error(hierarchical_clustering(data, 2, metric = "euclidean"))
  testthat::expect_no_error(hierarchical_clustering(data, 2, metric = "maximum"))
  testthat::expect_error(hierarchical_clustering(data, 2, metric = "Lp"))
  testthat::expect_no_error(hierarchical_clustering(data, 2, metric = "maximum", p = 4))
  testthat::expect_no_error(hierarchical_clustering(data, 2, metric = "manhattan"))
  testthat::expect_no_error(hierarchical_clustering(data, 2, custom_metric=function(x,y) as.numeric(!identical(x,y))))

  # test different numbers of expected clusters
  testthat::expect_no_error(hierarchical_clustering(data, 1))
  testthat::expect_no_error(hierarchical_clustering(data, 5))

  # demanding more clusters than datapoints should throw error
  testthat::expect_error(hierarchical_clustering(data, 25))

  print("Hierarchical Clustering has been tested!")
})
