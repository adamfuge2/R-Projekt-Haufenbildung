############# testing the data generators ###############

test_that('generators output right dimension',{
  # some of these are deprecated, but we do not care for these warnings here

  suppressWarnings(expect_equal(ncol(generateClusterTestDataSimple2D(10)),2))
  suppressWarnings(expect_equal(ncol(generateClusterTestData2DFromPaths(10,list(tibble::tibble(X=0.5,Y=0.5)))),2))

  expect_equal(ncol(generateClusterTestDataSimple(10,dim=3)),3)
  expect_equal(ncol(generateClusterDataFromPaths(10,list(tibble::tibble(X=0.5,Y=0.5,Z=0.8)))),3)
  expect_equal(ncol(generateFullTestData(10,min=c(0,0,13,1),max=c(1,0.5,100,3))),4)
})

test_that('generators outputs right sample size',{
  # some of these are deprecated, but we do not care for these warnings here

  suppressWarnings(expect_equal(nrow(generateClusterTestDataSimple2D(10)),10))
  suppressWarnings(expect_equal(nrow(generateClusterTestData2DFromPaths(10,list(tibble::tibble(X=0.5,Y=0.5)))),10))

  expect_equal(nrow(generateClusterTestDataSimple(10,dim=3)),10)
  expect_equal(nrow(generateClusterDataFromPaths(10,list(tibble::tibble(X=0.5,Y=0.5,Z=0.8)))),10)
  expect_equal(nrow(generateFullTestData(10,min(0,0,13,1),max(1,0.5,100,3))),10)
})

test_that('generators deprecation warnings',{
  expect_warning(generateClusterTestDataSimple2D())
  expect_warning(generateClusterTestData2DFromPaths(10,list(tibble::tibble(X=0.5,Y=0.5))))

  expect_no_warning(generateClusterTestDataSimple(10,dim=3))
  expect_no_warning(generateClusterDataFromPaths(10,list(tibble::tibble(X=0.5,Y=0.5,Z=0.8))))
  expect_no_warning(generateFullTestData(10,min(0,0,13,1),max(1,0.5,100,3)))
})
