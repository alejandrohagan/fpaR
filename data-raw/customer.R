
customer <- readr::read_csv("data-raw/customer.csv",name_repair = janitor::make_clean_names)

usethis::use_data(customer, overwrite = TRUE)
