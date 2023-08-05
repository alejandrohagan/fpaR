library(tidyverse)
library(dbplyr)
con <- DBI::dbConnect(duckdb::duckdb())

con2 <- DBI::dbConnect(odbc::odbc(), "datawarehouse")
duckdb::duckdb_register(con,"diamonds",diamonds)
diamonds_db <- tbl(con,"diamonds")

DBI::dbListObjects(con)

DBI::dbListTables(con)


DBI::dbListFields(con,"diamonds")
odbc::dbListFields(conn = con,name = "diamonds")

odbc::odbcListColumns(con,diamonds_db)


arrow::to_arrow(diamonds_db)

usethis::use_r("time_intelligence")
