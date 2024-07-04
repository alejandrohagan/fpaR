library(tidyverse)
devtools::document()
devtools::load_all()

drv <- duckdb::duckdb(dbdir="data/duckdb.db")

con <- DBI::dbConnect(drv)

DBI::dbWriteTable(con,"contoso_fact_sales",fpaR::contoso_fact_sales |> mutate(DateKey=mdy(DateKey)),overwrite=TRUE)

contoso_fact_sales_db <- tbl(con,"contoso_fact_sales")

contoso_fact_sales_db |> pluck("src") |> pluck("con")




contoso_fact_sales_db |>
  make_cohort_tbl(id_var="StoreKey",date = "DateKey",time_unit = "month",period_label = FALSE)

library(cohorts)
enc2utf8("test")

online_cohorts %>%
  cohort_table_month(CustomerID, InvoiceDate)


time_unit <- "month"
.data <- "contoso_fact_sales"
id_var <- "StoreKey"
date <- "DateKey"


test_sql <- glue::glue_sql(

"WITH date_var_calculated AS (
    SELECT
        {id_var}
        ,DATE_TRUNC({time_unit}, {date}) AS date_var
        ,MIN(DATE_TRUNC({time_unit}, {date})) OVER (PARTITION BY {id_var}) AS cohort

    FROM  {.data}

),

summary_calculated AS (
    SELECT
        cohort
        ,date_var
        ,COUNT(DISTINCT {id_var}) AS users

    FROM
        date_var_calculated

    GROUP BY
        cohort
        ,date_var)

SELECT
    *,
    DENSE_RANK() OVER (ORDER BY cohort, date_var) AS group_id
FROM
    summary_calculated;"
,.con=con
)





DBI::dbGetInfo(contoso_fact_sales_db)
DBI::dbGetQuery(con,test_sql)

DBI::dbGetInfo(con)
 |> summary()
