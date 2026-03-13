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

optics_wikipedia_data <- generate_optics_test_data(paths, n_points = 500, noise_points = 100, sd = 0.035 )

usethis::use_data(optics_wikipedia_data, overwrite = TRUE)
