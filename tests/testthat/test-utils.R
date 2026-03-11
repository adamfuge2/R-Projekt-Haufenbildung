test_that("multiplication", {
  expect_equal(2 * 2, 4)
})

test_that('wholenumber check works',{
  expect_equal(is.wholenumber(1),TRUE)
  expect_equal(is.wholenumber(0.5),FALSE)
  expect_error(is.wholenumber('char'))
})

test_that('clusteringFromCentroids',{
  data <- tibble::tibble(X=c(0.1,0.1,0.8),Y=c(0.46,0.8,0.4))

  clustering <- clusteringFromCentroids(data)


  testthat::expect_false(clustering(c(0.1,0.46)) == clustering(c(0.1,0.8)))
  testthat::expect_false(clustering(c(0.8,0.4)) == clustering(c(0.1,0.8)))
  testthat::expect_false(clustering(c(0.1,0.46)) == clustering(c(0.8,0.4)))
})

test_that('InnerInequality',{

  set.seed(13456)

  data <- generateClusterData(n=100,cluster_amount = 3)

  clustering <- kMeans(data,K=3,tries = 10)

  testthat::expect_equal(clustering$inner_inequality,3.59440075)
})


test_that('silhouette',{
  data <- tibble::tibble(X=c(0.1,0.1,0.8),Y=c(0.46,0.8,0.4))

  clustering <- clusteringFromCentroids(data)

  testthat::expect_equal(silhouette(data,clustering,c(0.1,0.46)),0)

  print(silhouette(data,clustering,c(0.15,0.46)))

  testthat::expect_gt(silhouette(data,clustering,c(0.15,0.46)), silhouette(data,clustering,c(0.4,0.46)))
})

test_that('meanSilhouette',{
  data <- tibble::tibble(X=c(0.1,0.1,0.8),Y=c(0.46,0.8,0.4))

  clustering <- clusteringFromCentroids(data)

  testthat::expect_equal(meanSilhouette(data,clustering),0)

  data <- data |> dplyr::add_row(tibble::tibble(X=0.15,Y=0.46))

  testthat::expect_equal(meanSilhouette(data,clustering),0.4268618)
})

test_that('tibbleAsPath',{
  tibble <- data.frame(X=c(0.1,0.1,0.5,0.8),Y=c(0.46,0.9,0.5,0.4))
  path <- tibbleAsPath(tibble)

  for(i in 1:nrow(tibble)){
    t <- (i - 1)/(nrow(tibble)-1)
    expect_equal(path(t) , tibble[i,])
  }

  expect_equal(tibble::remove_rownames(path(0.5)),data.frame(X=c(0.3),Y=c(0.7)))

})

test_that('dissimilarityMatrix',{
  data <- generateClusterData(n=10)
  testthat::expect_equal(ignore_attr = TRUE,dissimilarityMatrix(data,euclidean), as.matrix(stats::dist(data)))
})

test_that('sumOfDistancestTo',{
  data <- generateClusterData(n=10)
  expect_equal(sumOfDistancestTo(data,data[1,],euclidean), sum(as.matrix(stats::dist(data))[1,]))
})



test_that('viewClusters',{

  data <- tibble::tibble(X=c(0.1,0.1,0.5,0.8),Y=c(0.46,0.9,0.5,0.4))
  clustering <- clusteringFromCentroids(data)



  viewClusters(data,clustering)

  testthat::expect_equal(3+3,6)

})

test_that('viewData',{
  data <- tibble::tibble(X=c(0.1,0.1,0.5,0.8),Y=c(0.46,0.9,0.5,0.4))

  testthat::expect_no_error(viewClusters(data))
  testthat::expect_no_error(viewData(data))
})


test_that('metrics',{
  data <- tibble::tibble(X=c(-5.2, -3), Y=c(-7.9, 8), Z=c(34, 0))
  data_atomic_1 <- c(-5.2, -7.9, 34)
  data_atomic_2 <- c(-3, 8, 0)

  testthat::expect_no_error(euclidean(data[1, ], data[2, ]))
  testthat::expect_no_error(maximumMetric(data[1, ], data[2, ]))
  testthat::expect_no_error(pMetric(6)(data[1, ], data[2, ]))

  testthat::expect_no_error(euclidean(data_atomic_1, data_atomic_2))
  testthat::expect_no_error(maximumMetric(data_atomic_1, data_atomic_2))
  testthat::expect_no_error(pMetric(6)(data_atomic_1, data_atomic_2))
})


test_that('centroid_det', {
  data <- tibble::tibble(X=c(-5.2, -3, 0, 7, 100), Y=c(-7.9, 8, 0, 0, -9), Z=c(0, 0, 0, 9, 65))

  testthat::expect_no_error(centroid_det(data))
})


test_that('Linkage mode: centroid', {
  data_1 <- tibble::tibble(X=c(-5.2, -3, 0, 7, 10), Y=c(-7.9, 8, 0, 0, -9), Z=c(0, 0, 0, 9, 6.5))
  data_2 <- tibble::tibble(X=c(9, 6.7, -7.6, -7.9, 0), Y=c(4.4, -6.5, 0, 1.2, -8.1), Z=c(8.8, 2.6, -2.1, 0, 6.8))

  testthat::expect_no_error(centroid(euclidean, data_1, data_2))
})


test_that('Linkage mode: average', {
  data_1 <- tibble::tibble(X=c(-5.2, -3, 0, 7, 10), Y=c(-7.9, 8, 0, 0, -9), Z=c(0, 0, 0, 9, 6.5))
  data_2 <- tibble::tibble(X=c(9, 6.7, -7.6, -7.9, 0), Y=c(4.4, -6.5, 0, 1.2, -8.1), Z=c(8.8, 2.6, -2.1, 0, 6.8))

  testthat::expect_no_error(average(euclidean, data_1, data_2))
})


test_that('Linkage mode: single', {
  data_1 <- tibble::tibble(X=c(-5.2, -3, 0, 7, 10), Y=c(-7.9, 8, 0, 0, -9), Z=c(0, 0, 0, 9, 6.5))
  data_2 <- tibble::tibble(X=c(9, 6.7, -7.6, -7.9, 0), Y=c(4.4, -6.5, 0, 1.2, -8.1), Z=c(8.8, 2.6, -2.1, 0, 6.8))

  testthat::expect_no_error(single(euclidean, data_1, data_2))
})


test_that('Linkage mode: complete', {
  data_1 <- tibble::tibble(X=c(-5.2, -3, 0, 7, 10), Y=c(-7.9, 8, 0, 0, -9), Z=c(0, 0, 0, 9, 6.5))
  data_2 <- tibble::tibble(X=c(9, 6.7, -7.6, -7.9, 0), Y=c(4.4, -6.5, 0, 1.2, -8.1), Z=c(8.8, 2.6, -2.1, 0, 6.8))

  testthat::expect_no_error(complete(euclidean, data_1, data_2))
})
