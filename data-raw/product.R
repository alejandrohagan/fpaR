## code to prepare `product` dataset goes here

product <- readr::read_csv("data-raw/product.csv",name_repair = janitor::make_clean_names)

usethis::use_data(product, overwrite = TRUE)
