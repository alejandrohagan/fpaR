library(tidyverse)

devtools::document()

devtools::load_all()
devtools::test()
# load table

drv <- duckdb::duckdb(dbdir="data/duckdb.db")

con <- DBI::dbConnect(drv)

contoso_fact_sales_db <- tbl(con,"contoso_fact_sales")


## sd

diamonds |>
  summarise(
  overall_sd=sd(price)
  )


diamonds |>
  filter(
    cut=="Fair"
  ) |>
  summarise(
    overall_sd=sd(price)
  )

diamonds |>
  filter(
    cut %in% c("Fair","Good")
  ) |>
  summarise(
    overall_sd=sd(price)
  )

diamonds |>
  filter(
    cut %in% c("Fair","Good","Ideal")
  ) |>
  summarise(
    overall_sd=sd(price)
  )

diamonds |>
  filter(
    cut %in% c("Fair","Good","Ideal","Very Good")
  ) |>
  summarise(
    overall_sd=sd(price)
  )

diamonds |>
  summarise(
    overall_sd=sd(price)
  )



diamonds |>
  group_by(cut) |>
  summarize(
    individual_sd=sd(price)
    ) |>
  arrange(individual_sd)


## abs revamp

test_db <- contoso_fact_sales_db |> glimpse()

test_db |> glimpse() |> pluck(1)

ts <- glimpse(mtcars)

contoso_fact_sales_db |>
  summarise(
    # test2=dplyr::sql("MEDIAN(UnitCost)")
    # test3=mad(UnitCost)
    # ,median_vec=UnitCost-median(UnitCost)
    test=dplyr::sql("quantile_cont(UnitCost,.5)")
  )


mtcars |>
  as_tibble() |>
  mutate(
    test=median(abs(median(mpg)-mpg))
    ,test2=mad(mpg,constant = 1)
  )
DBI::dbListTables(con)
# Define start and end dates
start_date <- as.Date("2023-01-01")
end_date <- as.Date("2023-12-31")

# Generate the date sequence
date_seq <- seq.Date(start_date, end_date, by = "day")

# Create the data frame
date_df <- data.frame(
  date_key = date_seq,
  day_of_year = yday(date_seq),
  week_key = format(date_seq, "%Y%U"),  # ISO week
  week_of_year = isoweek(date_seq),
  day_of_week = wday(date_seq),
  iso_day_of_week = wday(date_seq, week_start = 1),
  day_name = weekdays(date_seq),
  first_day_of_week = floor_date(date_seq, "week"),
  last_day_of_week = ceiling_date(date_seq, "week") - days(1),
  month_key = format(date_seq, "%Y%m"),
  month_of_year = month(date_seq),
  day_of_month = mday(date_seq),
  month_name_short = substr(month(date_seq, label = TRUE, abbr = TRUE), 1, 3),
  month_name = month(date_seq, label = TRUE),
  first_day_of_month = floor_date(date_seq, "month"),
  last_day_of_month = ceiling_date(date_seq, "month") - days(1),
  quarter_key = paste0(year(date_seq), quarter(date_seq)),
  quarter_of_year = quarter(date_seq),
  day_of_quarter = date_seq - floor_date(date_seq, "quarter") + 1,
  quarter_desc_short = paste0("Q", quarter(date_seq)),
  quarter_desc = paste0("Quarter ", quarter(date_seq)),
  first_day_of_quarter = floor_date(date_seq, "quarter"),
  last_day_of_quarter = ceiling_date(date_seq + months(2), "quarter") - days(1),
  year_key = year(date_seq),
  first_day_of_year = floor_date(date_seq, "year"),
  last_day_of_year = ceiling_date(date_seq, "year") - days(1),
  ordinal_weekday_of_month = ave(date_seq, year(date_seq), month(date_seq), wday(date_seq), FUN = seq_along)
)

## practice sql functions
dbplyr::remote_con(contoso_fact_sales_db)
dbplyr::remote_table(contoso_fact_sales_db)

dbplyr::remote_query(contoso_fact_sales_db)

## practice fpaR unctions

out <- create_date_db(contoso_fact_sales_db,start_date = "2021-01-01",end_date = "2023-03-01")


DBI::dbGetQuery(conn = con,statement = out)


contoso_fact_sales_db |>
  group_by(SalesKey) |>
  mutate(
   id=(SalesKey)
  )


contoso_fact_sales_db |> pluck("src") |> pluck("con")


usethis::use_package(c("cli"))

contoso_fact_sales_db |>
  mutate(
    test=SalesKey/2
  ) |>
  relocate(test) |> show_query()
  make_cohort_tbl(id_var="StoreKey",date = "DateKey",time_unit = "month",period_label = FALSE)


vec <- c("one.md","two.md","three.md")

dir.create("test")
fpaR::clean_file_names("teset")
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
DBI::dbGetQuery(con,test)

DBI::dbGetInfo(con)
 |> summary()




postgres_agg <- sql_translator(.parent = base_agg,
                               cor = sql_aggregate_2("CORR"),
                               cov = sql_aggregate_2("COVAR_SAMP"),
                               sd =  sql_aggregate("STDDEV_SAMP", "sd"),
                               var = sql_aggregate("VAR_SAMP", "var")
)

# Next we have to simulate a connection that uses this variant
con <- simulate_dbi("TestCon")
sql_translation.TestCon <- function(x) {
  sql_variant(
    base_scalar,
    postgres_agg,
    base_no_win
  )
}

translate_sql(cor(x, y), con = con, window = FALSE)
translate_sql(sd(income / years), con = con, window = FALSE)

# Any functions not explicitly listed in the converter will be translated
# to sql as is, so you don't need to convert all functions.
translate_sql(??regr_intercept(y, x), con = con)


lm(price~0+carat+cut,data=diamonds) |>
  summary()

usethis::use_r("create_date_tbl")
test <- sql("


 WITH generate_date AS (
        SELECT CAST(RANGE AS DATE) AS date_key
          FROM RANGE(DATE '2009-01-01', DATE '2013-12-31', INTERVAL 1 DAY)
          )
   SELECT date_key AS date_key,
          DAYOFYEAR(date_key) AS day_of_year,
          YEARWEEK(date_key) AS week_key,
          WEEKOFYEAR(date_key) AS week_of_year,
          DAYOFWEEK(date_key) AS day_of_week,
          ISODOW(date_key) AS iso_day_of_week,
          DAYNAME(date_key) AS day_name,
          DATE_TRUNC('week', date_key) AS first_day_of_week,
          DATE_TRUNC('week', date_key) + 6 AS last_day_of_week,
          YEAR(date_key) || RIGHT('0' || MONTH(date_key), 2) AS month_key,
          MONTH(date_key) AS month_of_year,
          DAYOFMONTH(date_key) AS day_of_month,
          LEFT(MONTHNAME(date_key), 3) AS month_name_short,
          MONTHNAME(date_key) AS month_name,
          DATE_TRUNC('month', date_key) AS first_day_of_month,
          LAST_DAY(date_key) AS last_day_of_month,
          CAST(YEAR(date_key) || QUARTER(date_key) AS INT) AS quarter_key,
          QUARTER(date_key) AS quarter_of_year,
          CAST(date_key - DATE_TRUNC('Quarter', date_key) + 1 AS INT) AS day_of_quarter,
          ('Q' || QUARTER(date_key)) AS quarter_desc_short,
          ('Quarter ' || QUARTER(date_key)) AS quarter_desc,
          DATE_TRUNC('quarter', date_key) AS first_day_of_quarter,
          LAST_DAY(DATE_TRUNC('quarter', date_key) + INTERVAL 2 MONTH) as last_day_of_quarter,
          CAST(YEAR(date_key) AS INT) AS year_key,
          DATE_TRUNC('Year', date_key) AS first_day_of_year,
          DATE_TRUNC('Year', date_key) - 1 + INTERVAL 1 YEAR AS last_day_of_year,
          ROW_NUMBER() OVER (PARTITION BY YEAR(date_key), MONTH(date_key), DAYOFWEEK(date_key) ORDER BY date_key) AS ordinal_weekday_of_month
     FROM generate_date

    ")

out
DBI::dbGetQuery(conn = con,test)

sql1 <- sql("SELECT * FROM table1")
sql2 <- sql("SELECT * FROM table2 WHERE column1 > 100")
sql3 <- sql("SELECT column2, column3 FROM table3")


combined_sql <- sql(paste(sql1, sql2, sql3, sep = "; "))
