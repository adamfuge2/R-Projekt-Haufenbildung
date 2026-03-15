## code to prepare `optics_wikipedia_data`

path1 <- tibble::tibble(
  X = c(0.2,0.3,0.4,0.3,0.2),
  Y = c(0.3,0.4,0.3,0.2,0.3)
)

path2 <- tibble::tibble(
  X = c(0.45,0.5,0.45,0.4,0.45),
  Y = c(0.9,0.85,0.8,0.85,0.9)
)

path3 <- tibble::tibble(
  X = c(0.85,0.8,0.93,0.85),
  Y = c(0.5,0.28,0.28,0.5)
)

paths <- list(path1, path2, path3)

clusterpoints <- generateClusterDataFromPaths(n=1500, list_of_paths = paths, clusters_sd = rep(0.035, 3))

noise <- generateNoiseData(n=50, lower_bounds = c(0,0), upper_bounds = c(1,1), colnames = c("X","Y"))

optics_wikipedia_data <- dplyr::bind_rows(clusterpoints, noise)

usethis::use_data(optics_wikipedia_data, overwrite = TRUE)
