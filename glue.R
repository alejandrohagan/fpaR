library(tidyverse)
devtools::document()
devtools::load_all()
library(glue)

usethis::use_test("clean_file_names")
devtools::test()
list(a=1) |> class()
fs::file_exists("/tmp/Rtmp64LHmP/file47a176b5d839b.csv")

system2("xdg-open","/tmp/Rtmp64LHmP/file47a176b5d839b.csv")
c("1")|> class()
adb$sales |> head(100) |> show_in_excel()
mtcars|> show_in_excel()

adb <- fpaR::create_contonso_duckdb()
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


## hello


library(tidyverse)


tibble::tbl_sum(diamonds)

tibble::deframe(mtcars[,c(1,3)])

mtcars2 <- mtcars |> tibble::rownames_to_column()
  deframe(
    mtcars2[c(1,7)]
  )

sales_db
