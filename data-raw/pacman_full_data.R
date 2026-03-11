
pacman_full_data <- tibble::tibble(expand.grid(X=1:26,Y=1:30))
usethis::use_data(pacman_full_data, overwrite = TRUE)

viewData(pacman_full_data)
