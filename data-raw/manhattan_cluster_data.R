## code to prepare `manhattan_cluster_data` dataset goes here

n <- 200
r_1 <- tibble::tibble('X' = stats::runif(n, min = 0, max = 2), 'Y' = runif(n, min = 0, max = 3))
r_2 <- tibble::tibble('X' = stats::runif(n, min = 1, max = 3), 'Y' = runif(n, min = -4, max = 0))

maximum_cluster_data <- dplyr::bind_rows(r_1, r_2)

rot <- sin(pi/4)
mat <- base::matrix(c(rot, rot, -rot, rot), ncol = 2)

manhattan_cluster_data <- base::as.matrix(maximum_cluster_data) %*% mat |> tibble::as_tibble()

usethis::use_data(manhattan_cluster_data, overwrite = TRUE)
