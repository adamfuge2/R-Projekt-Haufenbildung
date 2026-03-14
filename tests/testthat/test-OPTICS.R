test_that("OPTICS works", {

  set.seed(123)

  data <- generateClusterData(n = 100)

  result <- optics(data, epsilon = 0.1, min_Pts = 5)

  testthat::expect_s3_class(result,"optics")

  testthat::expect_equal(length(result$order), nrow(data))
  testthat::expect_equal(length(result$reachdist), nrow(data))
  testthat::expect_equal(length(result$coredist), nrow(data))

  print("OPTICS has been tested in general")

})

test_that("OPTICS accepts different parameters", {

  set.seed(123)

  data <- generateClusterData(n = 100)

  testthat::expect_no_error(optics(data, epsilon = 0.05, min_Pts = 3))
  testthat::expect_no_error(optics(data, epsilon = 0.2, min_Pts = 10))
  testthat::expect_no_error(optics(data, epsilon = 0.5, min_Pts = 2))
  print('OPTICS has been tested on different parameters')
})

test_that("OPTICS handles wrong inputs",{

  data <- generateClusterData(n=100)

  expect_error(optics(data,epsilon=-1,min_Pts=5))
  expect_error(optics(data,epsilon=0.1,min_Pts=0))
  expect_error(optics(tibble::tibble(),epsilon=0.1,min_Pts=5))
  expect_error(optics(data,epsilon="abc",min_Pts=5))
  print('OPTICS terminates on wrong inputs')
})
