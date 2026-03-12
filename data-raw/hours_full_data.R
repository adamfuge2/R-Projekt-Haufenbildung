hours_full_data <- generateFullTestData(n=100,min = c(-180,-90), max = c(180,  90))

usethis::use_data(hours_full_data, overwrite = TRUE)
