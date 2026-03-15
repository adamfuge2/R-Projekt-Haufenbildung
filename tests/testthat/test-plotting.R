test_that('Plotting Clusters 1D',{
  clustered_data(tibble::tibble(X=1:15,cluster=c(1:15))) |>
    expect_no_error()
})

test_that('Plotting Clusters 2D',{
  clustered_data(tibble::tibble(X=1:15,Y=15:1,cluster=c(1:15))) |>
    expect_no_error()
})

test_that('Plotting Clusters 3D',{
  clustered_data(tibble::tibble(X=1:15,Y=15:1,Z=1:15,cluster=c(1:15))) |>
    expect_no_error()
})

test_that('Plotting Data 1D',{
  tibble::tibble(X=1:15) |>
    expect_no_error()
})

test_that('Plotting Data 2D',{
  tibble::tibble(X=1:15,Y=15:1) |>
    expect_no_error()
})

test_that('Plotting Data 3D',{
  tibble::tibble(X=1:15,Y=15:1,Z=1:15) |>
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
