library(tidyverse)
devtools::document()
devtools::load_all()

library(glue)


db <- fpaR::create_contonso_duckdb()

sales_db <- db$sales

con <- dbplyr::remote_con(sales_db)

.data <- sales_db

totalatd_dbi(sales_db,date_var=order_date,value_var=quantity)

totalatd(sales,date_var=order_date,value_var=quantity)

full_dbi <- sales_db |>
  filter(year(order_date) %in% c("2015","2016")) |>
  make_aggregation_dbi(customer_key,date_var=order_date,value_var=quantity,time_unit="day") |>
  pluck("dbi")


value_var <- "quantity"

full_dbi |>
  dplyr::mutate(
    date_lag=dplyr::sql("date + INTERVAL 1 WEEK")
    ,"{{noquote(value_var)}}_wow":=quantity
  )
  dplyr::select(-c(date,{{value_var}})) |>
  dplyr::ungroup()


out_tbl <-  dplyr::left_join(
  full_tbl
  ,lag_table
  ,by=dplyr::join_by(date==date_lag,...)
) |>
  mutate(
    "{{value_var}}_wow" := dplyr::coalesce(.data[[rlang::englue("{{value_var}}_wow")]],0)
  )
  ) |>
  dplyr::ungroup()



cout |>
  pluck("dbi") |>
  dplyr::mutate(
    year=lubridate::year(date)
    ,month=lubridate::quarter(date)
  ) |>
  dbplyr::window_order(year,month) |>
  dplyr::mutate(
    mtd=base::cumsum(quantity)
  ) |>
  dplyr::ungroup()




