## code to prepare `sales` dataset goes here



sales <- readr::read_csv("data-raw/sales.csv",name_repair = janitor::make_clean_names)

usethis::use_data(sales, overwrite = TRUE)
