
oh_no_ive_spilled_my_pacman_dots <- dplyr::mutate(generateClusterTestDataSimple(dim=2,cluster_amount = 5,n=200), X_1=ceiling(X_1*26)%%26+1, X_2=ceiling(X_2*30)%%30+1)

colnames(oh_no_ive_spilled_my_pacman_dots) <- c('X','Y')

usethis::use_data(oh_no_ive_spilled_my_pacman_dots, overwrite = TRUE)

#viewData(oh_no_ive_spilled_my_pacman_dots)
