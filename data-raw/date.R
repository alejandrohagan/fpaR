## code to prepare `date` dataset goes here

date <- readr::read_csv("data-raw/date.csv",name_repair = janitor::make_clean_names)
usethis::use_data(date, overwrite = TRUE)
