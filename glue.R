library(tidyverse)
devtools::document()
devtools::load_all()
library(glue)
rm(wow_dbi)

db <- fpaR::create_contonso_duckdb()
rm(list = c("wow_dbi"))
sales_db <- db$sales
usethis::use_r("factor")
con <- dbplyr::remote_con(sales_db)

.data <- sales_db |>
  filter(
    year(order_date)<=2016
  )

dbi_tbl <- make_aggregation_dbi(.data,customer_key,date_var=order_date,value_var =quantity,time_unit="day") |> pluck("dbi")
df_tbl <- make_aggregation_tbl(sales |> filter(year(order_date)<=2016),customer_key,date_var=order_date,value_var =quantity,time_unit="day")

totalytd_dbi(.data,customer_key,date_var=order_date,value_var=quantity)

totalatd_dbi(.data,customer_key,date_var=order_date,value_var=quantity)
totalmtd_dbi(.data,customer_key,date_var=order_date,value_var=quantity)
totalqtd_dbi(.data,customer_key,date_var=order_date,value_var=quantity)
totalwtd_dbi(.data,customer_key,date_var=order_date,value_var=quantity)

wow(fpaR::sales,date_var = order_date,value_var = quantity,lag_n = 1)

wow_dbi(sales_db,date_var = order_date,value_var=quantity,lag_n = 1) |>
  arrange(date)


mom_dbi(sales_db,date_var = order_date,value_var=quantity,lag_n = 4)
dod_dbi(sales_db,customer_key,date_var = order_date,value_var=quantity,lag_n = 1)
yoy_dbi(sales_db,customer_key,date_var = order_date,value_var=quantity,lag_n = 1)

