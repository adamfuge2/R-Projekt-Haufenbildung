test_that('Plotting with Noise',{
  clustered_data(tibble::tibble(X=1:20,cluster=c(0,0,0,0,0,1:15))) |>
    expect_no_error()
})
