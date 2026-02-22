test_that("K-Means works", {
  set.seed(13456)

  data <- generateClusterTestDataSimple2D(n=100,n_clusters = 3)

  clustering <- K_means_global(data,K=3,tries = 5)

  testthat::expect_false(clustering(c(0.1,0.46)) == clustering(c(0.1,0.8)))
  testthat::expect_false(clustering(c(0.8,0.46)) == clustering(c(0.1,0.8)))
  testthat::expect_false(clustering(c(0.1,0.46)) == clustering(c(0.8,0.46)))
})

test_that("findClusterAmountElbow works", {
  set.seed(123)

  data <- generateClusterTestDataSimple2D(n=50,n_clusters = 3)

  expect_equal(findClusterAmountElbow(data),3)
})


test_that("findClusterAmountSilhouette works", {
  set.seed(123)

  data <- generateClusterTestDataSimple2D(n=50,n_clusters = 3)

  expect_equal(findClusterAmountSilhouette(data),3)
})
