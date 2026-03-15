set.seed(12345)
n <- 400
r_1 <- tibble::tibble('X' = stats::runif(n/2, min = 0, max = 3), 'Y' = runif(n/2, min = 0, max = 3))
r_2 <- tibble::tibble('X' = stats::runif(n/2, min = 1, max = 4), 'Y' = runif(n/2, min = -3, max = 0))

maximum_cluster_data <- dplyr::bind_rows(r_1, r_2)

rot <- sin(pi/4)
mat <- base::matrix(c(rot, rot, -rot, rot), ncol = 2)

manhattan_clustered_data <- base::as.matrix(maximum_cluster_data) %*% mat |> tibble::as_tibble(.name_repair = 'minimal')

colnames(manhattan_clustered_data) <- c('X','Y')

manhattan_clustered_data <- clustered_data(manhattan_clustered_data,c(rep(1,n/2),rep(2,n/2)))

usethis::use_data(manhattan_clustered_data, overwrite = TRUE)

#viewClusters(manhattan_clustered_data)
