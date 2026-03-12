## code to prepare `maximum_cluster_data` dataset goes here
n <- 200
r_1 <- tibble::tibble('X' = stats::runif(n, min = 0, max = 2), 'Y' = runif(n, min = 0, max = 3))
r_2 <- tibble::tibble('X' = stats::runif(n, min = 1, max = 3), 'Y' = runif(n, min = -4, max = 0))

maximum_cluster_data <- dplyr::bind_rows(r_1, r_2)

usethis::use_data(maximum_cluster_data, overwrite = TRUE)
