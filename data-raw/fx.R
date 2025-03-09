## code to prepare `fx` dataset goes here

fx <- readr::read_csv("data-raw/currencyexchange.csv",name_repair = janitor::make_clean_names)

usethis::use_data(fx, overwrite = TRUE)
