numbers_and_characters <- tibble::tibble(numbers = c(1:length(study_courses_names),length(study_courses_names):1),
                                         characters = rep(study_courses_names,2))

usethis::use_data(numbers_and_characters, overwrite = TRUE)
