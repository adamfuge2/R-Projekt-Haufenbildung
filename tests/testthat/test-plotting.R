test_that('Plotting Clusters 1D',{
  clustered_data(tibble::tibble(X=1:15,cluster=c(1:15))) |>
    viewClusters() |>
    expect_no_error()
})

test_that('Plotting Clusters 2D',{
  clustered_data(tibble::tibble(X=1:15,Y=15:1,cluster=c(1:15))) |>
    viewClusters() |>
    expect_no_error()
})

test_that('Plotting Clusters 3D',{
  clustered_data(tibble::tibble(X=1:15,Y=15:1,Z=1:15,cluster=c(1:15))) |>
    viewClusters() |>
    expect_no_error()
})

test_that('Plotting Data 1D',{
  tibble::tibble(X=1:15) |>
    viewClusters() |>
    expect_no_error()
})

test_that('Plotting Data 2D',{
  tibble::tibble(X=1:15,Y=15:1) |>
    viewClusters() |>
    expect_no_error()
})

test_that('Plotting Data 3D',{
  tibble::tibble(X=1:15,Y=15:1,Z=1:15) |>
    viewClusters() |>
    expect_no_error()
})


test_that('Plotting spectral clustering object',{
  data <- generateClusterData(n = 200, dim = 2)

  #simplest case
  testthat::expect_no_error(spectralClustering(data, k=3, gamma = 50, K=3))

})

test_that('Plotting with Noise',{
  clustered_data(tibble::tibble(X=1:20,cluster=c(0,0,0,0,0,1:15))) |>
    expect_no_error()
})

test_that('Plotting clustering object',{
  kMeans(generateClusterData(),K=4) |>
    print.clustering() |>
    invisible()|>
    expect_no_error()|>
    capture_output(print=FALSE)
})

test_that('Plotting spectral clustering object',{
  spectralClustering(generateClusterData(),k=2,gamma=10,K=4) |>
    print.spectral_clustering() |>
    expect_no_error() |>
    capture_output(print=FALSE)
  spectralClustering(generateClusterData(),k=5,gamma=10,K=4) |>
    print.spectral_clustering() |>
    expect_no_error() |>
    capture_output(print=FALSE)
})


test_that('Plotting empty Clusters',{
  clustered_data(tibble::tibble(X=1,cluster=0)) |>
    viewClusters() |>
    expect_no_error()

  clustered_data(tibble::tibble(X=1,Y=1,cluster=0)) |>
    viewClusters() |>
    expect_no_error()

  clustered_data(tibble::tibble(X=1,Y=1,Z=3,cluster=0)) |>
    viewClusters() |>
    expect_no_error()
})

test_that('Plotting not print directly',{
  clustered_data(tibble::tibble(X=1,cluster=0)) |>
    viewClusters(print_directly = FALSE) |>
    expect_no_error()

  clustered_data(tibble::tibble(X=1,Y=1,cluster=0)) |>
    viewClusters(print_directly = FALSE) |>
    expect_no_error()
})

test_that('Plotting from clustering function',{
  tibble::tibble(X=5) |>
    viewClusters(clustering = function(x) 0 ) |>
    expect_no_error()
})

