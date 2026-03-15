############# testing the data generators ###############

test_that('generators output right dimension',{
  expect_equal(ncol(generateClusterData(10,
                                        dim=3)),3)
  expect_equal(ncol(generateClusterDataFromPaths(10,
                                                 list(tibble::tibble(X=0.5,Y=0.5,Z=0.8)))),3)
  expect_equal(ncol(generateNoiseData(10,
                                      lower_bounds=c(0,0,13,1),
                                      upper_bounds=c(1,0.5,100,3))),4)
})

test_that('generators outputs right sample size',{
  expect_equal(nrow(generateClusterData(10,dim=3)),10)
  expect_equal(nrow(generateClusterDataFromPaths(10,list(tibble::tibble(X=0.5,Y=0.5,Z=0.8)))),10)
  expect_equal(nrow(generateNoiseData(10,lower_bounds=c(0,0,13,1),upper_bounds=c(1,0.5,100,3))),10)
})

test_that('generators deprecation errors',{
  expect_no_error(generateClusterData(10,dim=3))
  expect_no_error(generateClusterDataFromPaths(10,list(tibble::tibble(X=0.5,Y=0.5,Z=0.8))))
  expect_no_error(generateNoiseData(10,lower_bounds=c(0,0,13,1),upper_bounds=c(1,0.5,100,3)))
})

test_that('generateClusterData cluster by cluster_amount',{
  expect_no_error(generateClusterData(10,
                                      cluster_amount = 5))
  expect_no_error(generateClusterData(10,
                                      cluster_amount = 1))
  expect_no_error(generateClusterData(1,
                                      cluster_amount = 1))
  expect_error(generateClusterData(10,
                                   cluster_amount = 15))
})

test_that('generateClusterData cluster by bounds',{
  expect_no_error(generateClusterData(10,
                                        lower_bounds = c(0,0)))
  expect_no_error(generateClusterData(10,
                                        upper_bounds = c(1,1)))
  expect_no_error(generateClusterData(10,
                                        lower_bounds = c(-5,0),
                                        upper_bounds = c(2,4)))
  expect_error(generateClusterData(10,
                                        dim=5,
                                        lower_bounds = c(-5,0),
                                        upper_bounds = c(2,4)))
})


test_that('generateClusterData cluster by means and sd',{
  expect_no_error(generateClusterData(10,
                                        clusters_mean = tibble::tibble(longitude = c(-100,-60,10,20,70,130),
                                                                        latitude =  c(  40,-10,50,20,50,-20))))
  expect_no_error(generateClusterData(10,
                                        clusters_sd = c(8,8,3,6,15,5)))
  expect_no_error(generateClusterData(10,
                                        clusters_prob = c(8,8,3,6,15,5)))

  # error by wrong dim
  expect_error(generateClusterData(10,
                                        dim=5,
                                        clusters_mean = tibble::tibble(longitude = c(-100,-60,10,20,70,130),
                                                                        latitude =  c(  40,-10,50,20,50,-20))))
  expect_error(generateClusterData(10,
                                        cluster_amount =5,
                                        clusters_sd = c(8,8,3,6,15,5)))
  expect_error(generateClusterData(10,
                                   cluster_amount =5,
                                        clusters_prob = c(8,8,3,6,15,5)))

  # all together
  expect_no_error(generateClusterData(10,
                                      clusters_mean = tibble::tibble(longitude = c(-100,-60,10,20,70,130),
                                                                      latitude =  c(  40,-10,50,20,50,-20)),
                                      clusters_sd = c(8,8,3,6,15,5),
                                      clusters_prob = c(8,8,3,6,15,5)))

  # ignore bounds
  expect_no_error(generateClusterData(10,
                                      lower_bounds = c(-5,0),
                                      upper_bounds = c(2,4),
                                      clusters_mean = tibble::tibble(longitude = c(-100,-60,10,20,70,130),
                                                                      latitude =  c(  40,-10,50,20,50,-20)),
                                      clusters_sd = c(8,8,3,6,15,5),
                                      clusters_prob = c(8,8,3,6,15,5)))
})

test_that('generateClusterData misc inputs',{
  capture_output(expect_no_error(invisible(generateClusterData(10, .print_info = TRUE))), print=FALSE)
  expect_no_error(generateClusterData(10, include_cluster = TRUE))
  expect_no_error(generateClusterData(10, dim=3,colnames = c('Y','Z','X')))
  expect_error(generateClusterData(10, dim=2,colnames = c('Y','Z','X')))
})

test_that('generateClusterDataFromPaths sd and prop',{
  lop = list(tibble::tibble(X = c(0,1,1),
                            Y = c(0,0,1)),
             tibble::tibble(X = c(0,0.2),
                            Y = c(1,0.8)))

  expect_no_error(generateClusterDataFromPaths(10,
                                               lop,
                                               clusters_sd = c(8,5)))

  expect_no_error(generateClusterDataFromPaths(10,
                                               lop,
                                               clusters_prob = c(8,15)))
  expect_no_error(generateClusterDataFromPaths(10,
                                               lop,
                                               clusters_sd = c(8,5),
                                               clusters_prob = c(8,15)))
  # wrong lengths:
  expect_error(generateClusterDataFromPaths(10,
                                               lop,
                                               clusters_sd = c(8,5,5),
                                               clusters_prob = c(8,15)))
  expect_error(generateClusterDataFromPaths(10,
                                               lop,
                                               clusters_sd = c(8,5),
                                               clusters_prob = c(8,15,5)))

})

test_that('generateClusterDataFromPaths misc inputs',{
  lop = list(tibble::tibble(X = c(0,1,1),
                                      Y = c(0,0,1)),
                       tibble::tibble(X = c(0,0.2),
                                      Y = c(1,0.8)))
  capture_output(expect_no_error(invisible(generateClusterDataFromPaths(10,lop, .print_info = TRUE))), print=FALSE)
  expect_no_error(generateClusterDataFromPaths(10, lop, include_cluster = TRUE))
})


test_that('generateNoiseData cluster by bounds',{
  expect_no_error(generateNoiseData(10,
                                      lower_bounds = c(-5,0),
                                      upper_bounds = c(2,4)))
  expect_error(generateNoiseData(10,
                                   lower_bounds = 1:2,
                                   upper_bounds = 1:3))
  expect_error(generateNoiseData(10,
                                 lower_bounds = 1:2,
                                 upper_bounds = 1:0))
})

test_that('generateNoiseData misc inputs',{
  capture_output(expect_no_error(invisible(generateNoiseData(10,
                                                             lower_bounds = c(-5,0),
                                                             upper_bounds = c(2,4),
                                                             .print_info = TRUE))),
                 print=FALSE)
  expect_no_error(generateNoiseData(10,
                                    lower_bounds = c(-5,0),
                                    upper_bounds = c(2,4),
                                    include_cluster = TRUE))
  expect_no_error(generateNoiseData(10,
                                    lower_bounds = c(-5,0),
                                    upper_bounds = c(2,4),
                                    colnames = c('Y','Z')))
  expect_error(generateNoiseData(10,
                                 lower_bounds = c(-5,0),
                                 upper_bounds = c(2,4),
                                 colnames = c('Y','Z','X')))
})

