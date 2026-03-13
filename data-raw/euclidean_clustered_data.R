
set.seed(123)
euclidean_clustered_data <- generateClusterData(n = 400,
                                                dim = 2,
                                                cluster_amount = 3,
                                                clusters_mean = tibble::tibble('X'=c(-1,1,0), 'Y'=c(0,0,1.8)),
                                                clusters_sd = c(0.4,0.4,0.4),
                                                include_cluster = TRUE)

usethis::use_data(euclidean_clustered_data, overwrite = TRUE)
