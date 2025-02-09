## code to prepare `store` dataset goes here

store <- readr::read_csv("data-raw/store.csv",name_repair = janitor::make_clean_names)


usethis::use_data(store, overwrite = TRUE)
