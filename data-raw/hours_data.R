
hours_data <- tibble::tibble(24*generateClusterTestDataSimple(dim=1,cluster_amount = 4,n=200)%%24)

usethis::use_data(hours_data, overwrite = TRUE)
