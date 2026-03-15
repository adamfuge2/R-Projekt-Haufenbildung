set.seed(12345)
n <- 400
r_1 <- tibble::tibble('X' = stats::runif(n/2, min = 0, max = 3), 'Y' = runif(n/2, min = 0, max = 3),cluster=1)
r_2 <- tibble::tibble('X' = stats::runif(n/2, min = 1, max = 4), 'Y' = runif(n/2, min = -3, max = 0),cluster=2)

maximum_clustered_data <- clustered_data(dplyr::bind_rows(r_1, r_2))

usethis::use_data(maximum_clustered_data, overwrite = TRUE)
