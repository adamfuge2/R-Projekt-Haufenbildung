#test for spectral clustering

test_that("Gauss Kernel Weights", {
  data <- generateClusterData(n = 10)
  testthat::expect_no_error(gaussKernelWeights(data, 5))
})

test_that("Gauss Kernel", {
  gamma <- 1.2
  vec_1 <- c(23, -424.9, 98, -75)
  vec_2 <- c(19.8, -429, 90, -73)
  tbl_1 <- tibble::as_tibble_row(vec_1, .name_repair = "unique")
  tbl_2 <- tibble::as_tibble_row(vec_2, .name_repair = "unique")
  testthat::expect_equal(gaussKernel(vec_1, vec_2, gamma), gaussKernel(tbl_1, tbl_2, gamma))
})


test_that("Kernel by Custom Metric", {
  x <- c(2, 4, -8)
  y <- c(2.4, 5, 9.1)
  testthat::expect_no_error(kernelByCustomMetric(euclidean, 2)(x, y))
})


test_that("Spectral Projection", {
  data <- generateClusterData(n = 20, dim = 5)
  k <- 3
  gamma <- 1.1

  #simplest case
  testthat::expect_no_error(spectralProjection(data, k, gamma = gamma))

  #for special cases
  custum_kernel <- kernelByCustomMetric(maximumMetric, 1.4)
  testthat::expect_no_error(spectralProjection(data, k, gamma = gamma, custom_mercer_kernel = custum_kernel))
  testthat::expect_no_error(spectralProjection(data, k, gamma = gamma, metric = "Lp", p = 4))
  testthat::expect_equal(ncol(spectralProjection(data, k, gamma = gamma, metric = "Lp", p = 4)$projected_data), k)
})


test_that("Spectral Clustering", {
  data <- generateClusterData(n = 20, dim = 3)
  k <- 2
  gamma <- 1.4

  testthat::expect_no_error(spectralClustering(data, k, gamma = gamma, K = 4))

  testthat::expect_no_error(spectralClustering(data, k, gamma = gamma, cluster_algorithm = "K-Medioids", K = 4))
  testthat::expect_no_error(spectralClustering(data, k, gamma = gamma, cluster_algorithm = "hierarchical Clustering", n = 2))

  #fill with additional arguments if fully implemented
  testthat::expect_no_error(spectralClustering(data, k, gamma = gamma, cluster_algorithm = "DBSCAN"))
  testthat::expect_no_error(spectralClustering(data, k, gamma = gamma, cluster_algorithm = "OPTICS"))

})

