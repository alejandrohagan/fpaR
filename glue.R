library(tidyverse)
devtools::document()
devtools::load_all()

library(glue)


db <- fpaR::create_contonso_duckdb()

sales_db <- db$sales

con <- dbplyr::remote_con(sales_db)

.data <- sales_db |>
  filter(
    year(order_date)<=2016
  )

dbi_tbl <- make_aggregation_dbi(.data,customer_key,date_var=order_date,value_var =quantity,time_unit="day") |> pluck("dbi")
df_tbl <- make_aggregation_tbl(sales |> filter(year(order_date)<=2016),customer_key,date_var=order_date,value_var =quantity,time_unit="day")


totalytd_dbi(.data,customer_key,date_var=order_date,value_var=quantity) |>
  filter(
  customer_key=="210071"
  # customer_key=="1979682"
) |> view()
  group_by(customer_key,year) |>
  summarise(
    max=max(ytd)
    ,sum=sum(ytd)
  )


full_dbi <- dbi_tbl |>
dplyr::mutate(
  year=lubridate::year(date)
  ,.before = 1
)


full_dbi |>
  dbplyr::window_order(customer_key,date) |>
  arrange(customer_key,date) |>
  dplyr::mutate(
    ytd=base::cumsum(quantity)
  ) |>

  dplyr::ungroup()


all.equal(dbi_tbl,df_tbl)


full_join(
  dbi_tbl
  ,df_tbl
  ,by=join_by(date,customer_key)
) |>
  group_by(
    customer_key
  ) |>
  summarise(
    dbi=sum(quantity.x)
    ,df=sum(quantity.y)
    ,n=n()
    ,.groups = "drop"
  ) |>
  mutate(
    delta=dbi-df
  ) |>
  summarise(
    sum_delta=sum(delta)
  )
  # filter(customer_key=="2")

  count(customer_key,wt="quantity.x")
totalytd_dbi(.data,customer_key,date_var=order_date,value_var=quantity)
totalatd_dbi(.data,customer_key,date_var=order_date,value_var=quantity)
totalmtd_dbi(.data,customer_key,date_var=order_date,value_var=quantity)
totalqtd_dbi(.data,customer_key,date_var=order_date,value_var=quantity) |>
  group_by(customer_key,quarter,year) |>
  summarise(
    qtd=max(qtd)
    ,sum=sum(quantity)
  ) |> view()
totalwtd_dbi(.data,customer_key,date_var=order_date,value_var=quantity)


