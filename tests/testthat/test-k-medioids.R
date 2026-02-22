test_that("greedySearchMedioids works", {
  set.seed(123)

  data <- generateClusterTestDataSimple2D(n=50,n_clusters = 3)

  expect_equal(greedySearchMedioids(data,3),
               tibble::tibble(X=c(0.321377884,0.401782714,0.773657988),Y=c(0.79948784,0.05025646,0.93627530)))
})
