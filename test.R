library(tidyverse)
devtools::load_all()
devtools::document()

drv <- duckdb::duckdb(dbdir="data/duckdb.db")

con <- DBI::dbConnect(drv)
DBI::dbListTables(con)

diamonds_db <- tbl(con,"diamonds.db")

diamonds_db |> class()

fpaR::count_plus(diamonds_db,cut,.wt = price,.sort = 0)

fpaR::count_plus(diamonds,cut,.wt = price, .sort=0)



diamonds_db |>
  # group_by(ca) |>
  summarise(
    intercept=REGR_INTERCEPT(price, carat)
    ,int=REGR_INTERCEPT(price, x)
    ,carat=REGR_SLOPE(price, carat)
    ,x=REGR_SLOPE(price, x)
  )



lm(price~carat+x,data=diamonds)

mean(c(2256,14094))

10126-7756
3145-1027
