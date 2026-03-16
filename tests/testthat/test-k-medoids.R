
data <- generateClusterData(n=10,
                            clusters_mean = tibble::tibble(X=c(0.4,0.4,0.8),
                                                           Y=c(0.7,0.1,0.7)),
                            clusters_sd=c(0.01,0.02,0.01),
                            cluster_amount = 3)

data_3D <- generateClusterData(n=10,dim=3)


test_that("greedySearchMedoidIndeces", {
  set.seed(123)
  data <- generateClusterData(n=100,
                              clusters_mean = tibble::tibble(X=c(0.4,0.4,0.8),
                                                             Y=c(0.7,0.1,0.7)),
                              clusters_sd=c(0.01,0.02,0.01),
                              cluster_amount = 3)

  expect_equal(greedySearchMedoidIndeces(data,3),c(50,1,27))
})


test_that('kMeans base output',{
  clustering <- kMedoids(data,K=3)

  clustering_f <- clustering$clustering_function
  testthat::expect_false(clustering_f(c(0.4,0.7)) == clustering_f(c(0.8,0.7)))
  testthat::expect_false(clustering_f(c(0.4,0.1)) == clustering_f(c(0.8,0.7)))
  testthat::expect_false(clustering_f(c(0.4,0.7)) == clustering_f(c(0.4,0.1)))

  testthat::expect_equal(nrow(clustering$clustered_data),nrow(data))
  testthat::expect_equal(ncol(clustering$clustered_data),ncol(data)+1)
  testthat::expect_equal(length(unique(clustering$clustered_data$cluster)),3)
})

test_that('kMedoids K input',{
  testthat::expect_no_error(kMedoids(data,K=1))
  testthat::expect_no_error(kMedoids(data,K=2))
  testthat::expect_no_error(kMedoids(data,K=5))


  testthat::expect_error(kMedoids(data,K=0))
  testthat::expect_error(kMedoids(data,K=101))
})

test_that('kMedoids different data type and custom dist function',{
  testthat::expect_no_error(kMedoids(study_courses_data,K=4,custom_distance_function = study_courses_distance))
})

test_that('kMedoids K input',{
  testthat::expect_no_error(kMedoids(data,K=1))
  testthat::expect_no_error(kMedoids(data,K=2))
  testthat::expect_no_error(kMedoids(data,K=5))

  testthat::expect_error(kMedoids(data,K=0))
  testthat::expect_error(kMedoids(data,K=101))
})


test_that('kMedoids distance_method input',{

  ## euclidean
  testthat::expect_no_error(kMedoids(data,K=1,distance_method = 'euclidean'))
  testthat::expect_no_error(kMedoids(data,K=5,distance_method = 'euclidean'))
  testthat::expect_no_error(kMedoids(data_3D,K=1,distance_method = 'euclidean'))
  testthat::expect_no_error(kMedoids(data[,1],K=5,distance_method = 'euclidean'))
  testthat::expect_no_error(kMedoids(data[1,1],K=1,distance_method = 'euclidean'))


  ## maximum
  testthat::expect_no_error(kMedoids(data,K=1,distance_method = 'maximum'))
  testthat::expect_no_error(kMedoids(data,K=5,distance_method = 'maximum'))
  testthat::expect_no_error(kMedoids(data_3D,K=1,distance_method = 'maximum'))
  testthat::expect_no_error(kMedoids(data[,1],K=5,distance_method = 'maximum'))
  testthat::expect_no_error(kMedoids(data[1,1],K=1,distance_method = 'maximum'))


  ## manhattan
  testthat::expect_no_error(kMedoids(data,K=1,distance_method = 'manhattan'))
  testthat::expect_no_error(kMedoids(data,K=5,distance_method = 'manhattan'))
  testthat::expect_no_error(kMedoids(data_3D,K=1,distance_method = 'manhattan'))
  testthat::expect_no_error(kMedoids(data[,1],K=5,distance_method = 'manhattan'))
  testthat::expect_no_error(kMedoids(data[1,1],K=1,distance_method = 'manhattan'))

  ## minkowski
  testthat::expect_no_error(kMedoids(data,K=1,distance_method = 'minkowski',p=1))
  testthat::expect_no_error(kMedoids(data,K=5,distance_method = 'minkowski',p=1))
  testthat::expect_no_error(kMedoids(data_3D,K=1,distance_method = 'minkowski',p=1))
  testthat::expect_no_error(kMedoids(data[,1],K=5,distance_method = 'minkowski',p=1))
  testthat::expect_no_error(kMedoids(data[1,1],K=1,distance_method = 'minkowski',p=1))

  ## binary
  testthat::expect_no_error(kMedoids(data,K=1,distance_method = 'binary'))
  testthat::expect_no_error(kMedoids(data,K=5,distance_method = 'binary'))
  testthat::expect_no_error(kMedoids(data_3D,K=1,distance_method = 'binary'))

  ## canberra
  testthat::expect_no_error(kMedoids(data,K=1,distance_method = 'canberra'))
  testthat::expect_no_error(kMedoids(data,K=5,distance_method = 'canberra'))
  testthat::expect_no_error(kMedoids(data_3D,K=1,distance_method = 'canberra'))

  ## unknown distance method
  testthat::expect_error(kMedoids(data,K=3,distance_method = 'fkalfldasha'))
})
