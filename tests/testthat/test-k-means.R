test_that("K-Means works", {
  set.seed(13456)

  data <- generateClusterTestDataSimple(n=100,cluster_amount = 3)

  clustering <- kMeans(data,K=3,tries = 5)

  clustering_f <- clustering$clustering_function
  testthat::expect_false(clustering_f(c(0.4,0.7)) == clustering_f(c(0.8,0.7)))
  testthat::expect_false(clustering_f(c(0.4,0.1)) == clustering_f(c(0.8,0.7)))
  testthat::expect_false(clustering_f(c(0.4,0.7)) == clustering_f(c(0.4,0.1)))

  testthat::expect_equal(nrow(clustering$clustered_data),nrow(data))
  testthat::expect_equal(ncol(clustering$clustered_data),ncol(data)+1)
  testthat::expect_equal(length(unique(clustering$clustered_data$cluster)),3)

  kmeans(tibble::tibble(X=c(0,0,0)),2)

  testthat::expect_no_error(kMeans(data,K=1,tries=1))
  testthat::expect_no_error(kMeans(data,K=10,tries=1))
  testthat::expect_no_error(kMeans(data,K=1,tries=10))
  testthat::expect_no_error(kMeans(data,K=10,tries=10))
  testthat::expect_no_error(kMeans(data,K=3,metric = 'euclidean'))
  testthat::expect_no_error(kMeans(data,K=3,metric = 'maximum'))
  testthat::expect_no_error(kMeans(data,K=3,metric = 'Lp', p=1.5))
  testthat::expect_no_error(kMeans(data,K=3,metric = 'Lp', p=10))
  testthat::expect_no_error(kMeans(data,K=3,metric = 'manhattan'))
  testthat::expect_no_error(kMeans(data,K=3,custom_metric=function(x,y) as.numeric(!identical(x,y))))

  testthat::expect_error(kMeans(data,K=1,tries=0))
  testthat::expect_error(kMeans(data,K=0,tries=2))
  testthat::expect_error(kMeans(tibble::tibble(),K=3,tries=2))
  testthat::expect_error(kMeans(tibble::tibble(X=0),K=3,tries=2))
  testthat::expect_error(kMeans(tibble::tibble(X=c(0,0,0)),K=2))
  testthat::expect_error(kMeans(data[101,],K=1,tries=2))
  testthat::expect_error(kMeans(data,K=3,metric='42'))
  print('kMeans has been tested')
})

test_that("findClusterAmountElbow works", {
  set.seed(123)

  data <- generateClusterTestDataSimple(n=100,cluster_amount = 3)

  expect_equal(findClusterAmountElbow(data),3)
})


test_that("findClusterAmountSilhouette works", {
  set.seed(123)

  data <- generateClusterTestDataSimple(n=100,cluster_amount = 3)

  expect_equal(findClusterAmountSilhouette(data),3)
})
