test_that("greedySearchMedioidIndeces", {
  set.seed(123)

  data <- generateClusterData(n=50,cluster_amount = 3)

  expect_equal(greedySearchMedioidIndeces(data,3),c(45,4,14))
})
