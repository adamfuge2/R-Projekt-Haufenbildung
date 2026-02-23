test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that('wholenumber check works',{
  expect_equal(is.wholenumber(1),TRUE)
  expect_equal(is.wholenumber(0.5),FALSE)
  expect_error(is.wholenumber('char'))
})

test_that('generators output right dimension',{
  expect_equal(ncol(generateClusterTestDataSimple2D(10)),2)
  expect_equal(ncol(generateClusterTestDataSimple(10,dim=3)),3)
  expect_equal(ncol(generateClusterTestData2DFromPaths(10,list(tibble::tibble(X=0.5,Y=0.5)))),2)
})

test_that('generators outputs right sample size',{
  expect_equal(nrow(generateClusterTestDataSimple2D(10)),10)
  expect_equal(nrow(generateClusterTestData2DFromPaths(10,list(tibble::tibble(X=0.5,Y=0.5)))),10)
})

test_that('clusteringFromCentroids works',{
  data <- tibble::tibble(X=c(0.1,0.1,0.8),Y=c(0.46,0.8,0.4))

  clustering <- clusteringFromCentroids(data)


  testthat::expect_false(clustering(c(0.1,0.46)) == clustering(c(0.1,0.8)))
  testthat::expect_false(clustering(c(0.8,0.4)) == clustering(c(0.1,0.8)))
  testthat::expect_false(clustering(c(0.1,0.46)) == clustering(c(0.8,0.4)))
})

test_that('InnerInequality works',{

  set.seed(13456)

  data <- generateClusterTestDataSimple2D(n=100,n_clusters = 3)

  clustering <- K_means_global(data,K=3,tries = 10)

  testthat::expect_equal(innerInequality(data,clustering),5.28324491312604)
})


test_that('silhouette works',{
  data <- tibble::tibble(X=c(0.1,0.1,0.8),Y=c(0.46,0.8,0.4))

  clustering <- clusteringFromCentroids(data)

  testthat::expect_equal(silhouette(data,clustering,c(0.1,0.46)),0)

  print(silhouette(data,clustering,c(0.15,0.46)))

  testthat::expect_gt(silhouette(data,clustering,c(0.15,0.46)), silhouette(data,clustering,c(0.4,0.46)))
})

test_that('meanSilhouette works',{
  data <- tibble::tibble(X=c(0.1,0.1,0.8),Y=c(0.46,0.8,0.4))

  clustering <- clusteringFromCentroids(data)

  testthat::expect_equal(meanSilhouette(data,clustering),0)

  data <- data |> dplyr::add_row(tibble::tibble(X=0.15,Y=0.46))

  testthat::expect_equal(meanSilhouette(data,clustering),0.4268618)
})

test_that('tibbleAsPath works',{
  tibble <- data.frame(X=c(0.1,0.1,0.5,0.8),Y=c(0.46,0.9,0.5,0.4))
  path <- tibbleAsPath(tibble)

  for(i in 1:nrow(tibble)){
    t <- (i - 1)/(nrow(tibble)-1)
    expect_equal(path(t) , tibble[i,])
  }

  expect_equal(tibble::remove_rownames(path(0.5)),data.frame(X=c(0.3),Y=c(0.7)))

})

test_that('dissimilarityMatrix works',{
  data <- generateClusterTestDataSimple2D(n=10)
  testthat::expect_equal(ignore_attr = TRUE,dissimilarityMatrix(data,euclidean), as.matrix(stats::dist(data)))
})

test_that('sumOfDistancestTo works',{
  data <- generateClusterTestDataSimple2D(n=10)
  expect_equal(sumOfDistancestTo(data,data[1,],euclidean), sum(as.matrix(stats::dist(data))[1,]))
})



test_that('viewClusters works',{
  data <- data.frame(X=c(0.1,0.1,0.5,0.8),Y=c(0.46,0.9,0.5,0.4))
  clustering <- clusteringFromCentroids(data)

  testthat::expect_no_error(viewClusters(data,clustering))

})

test_that('viewData works',{
  data <- data.frame(X=c(0.1,0.1,0.5,0.8),Y=c(0.46,0.9,0.5,0.4))

  testthat::expect_no_error(viewClusters(data))
  testthat::expect_no_error(viewData(data))
})

