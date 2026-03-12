############# testing the data generators ###############

test_that('generators output right dimension',{
  expect_equal(ncol(generateClusterData(10,dim=3)),3)
  expect_equal(ncol(generateClusterDataFromPaths(10,list(tibble::tibble(X=0.5,Y=0.5,Z=0.8)))),3)
  expect_equal(ncol(generateNoiseData(10,lower_bounds=c(0,0,13,1),upper_bounds=c(1,0.5,100,3))),4)
})

test_that('generators outputs right sample size',{
  expect_equal(nrow(generateClusterData(10,dim=3)),10)
  expect_equal(nrow(generateClusterDataFromPaths(10,list(tibble::tibble(X=0.5,Y=0.5,Z=0.8)))),10)
  expect_equal(nrow(generateNoiseData(10,lower_bounds=c(0,0,13,1),upper_bounds=c(1,0.5,100,3))),10)
})

test_that('generators deprecation warnings',{
  expect_no_warning(generateClusterData(10,dim=3))
  expect_no_warning(generateClusterDataFromPaths(10,list(tibble::tibble(X=0.5,Y=0.5,Z=0.8))))
  expect_no_warning(generateNoiseData(10,lower_bounds=c(0,0,13,1),upper_bounds=c(1,0.5,100,3)))
})
