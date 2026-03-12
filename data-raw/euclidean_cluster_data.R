## code to prepare `euclidean_cluster_data` dataset goes here

set.seed(123)
euclidean_cluster_data <- generateClusterTestDataSimple(n = 400, dim = 2, cluster_amount = 2)
usethis::use_data(euclidean_cluster_data, overwrite = TRUE)




