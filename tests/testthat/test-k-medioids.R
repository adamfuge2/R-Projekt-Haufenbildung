test_that("greedySearchMedioidIndeces", {
  set.seed(123)
  data <- generateClusterData(n=100,
                              clusters_mean = tibble::tibble(X=c(0.4,0.4,0.8),
                                                             Y=c(0.7,0.1,0.7)),
                              clusters_sd=c(0.01,0.02,0.01),
                              cluster_amount = 3)

  expect_equal(greedySearchMedioidIndeces(data,3),c(50,1,27))
})
