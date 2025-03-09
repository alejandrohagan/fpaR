


sales <- readr::read_csv("data-raw/sales.csv",name_repair = janitor::make_clean_names,show_col_types = FALSE) |>
  dplyr::mutate(
    gross_revenue=unit_price*quantity
    ,net_revenue=net_price*quantity
    ,cogs=unit_cost*quantity
    ,margin=net_revenue-cogs
  )



usethis::use_data(sales, overwrite = TRUE)
