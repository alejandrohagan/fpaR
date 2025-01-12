## code to prepare `functions` dataset goes here

library(tidyverse)


file <- here::here("data-raw","functions.ods")

functions <- readODS::read_ods(file,skip = 1) |>
  select(fn_name,short_name,description,method) |>
  mutate(
    fn_name_lower=stringr::str_to_lower(fn_name)
  )


usethis::use_data(functions,overwrite = TRUE)
