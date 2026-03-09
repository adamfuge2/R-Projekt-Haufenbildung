study_courses_data <- tibble::tibble(study_courses = study_courses_names)

usethis::use_data(study_courses_data, overwrite = TRUE)
