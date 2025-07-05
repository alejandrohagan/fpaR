

# put indicator for imbalanced periods, missing dates indicator
## looks like we are doing days in current period and days in prior period as an easier indicator
## logic created -- but check functionality and put it into functions
## add labels to out output tables?
# find sql logic for 554, 445, 13 month calendars

# put print method for imbalanced period for comparison functions
# Put description in print method of calendar description
# update readme examples
# balance_indicator="balance","as-is"

## two period imbalanc etypes -- either previous period has more days than current comparison period (length) or previous period has less comparions
## period days (short)
## complete_calendar() -- augment calendar attributes (Eg. days in month, week, start date end date, year start date, end date, etc)
## summary method -- summary calendar attributes (number of missing dates?)


sql <- "
WITH calendar_dates AS (
    -- Generate all dates of the year starting from January 1st
    SELECT
        generate_series(
            '2025-01-01'::date,         -- Start date (January 1st of the given year)
            '2025-12-31'::date,         -- End date (December 31st of the given year)
            '1 day'::interval           -- Interval of one day
        ) AS calendar_date
),

weeks AS (
    -- Calculate week number (ISO 8601 standard) for each date
    SELECT
        calendar_date,
        EXTRACT(week FROM calendar_date) AS week_number,
        EXTRACT(year FROM calendar_date) AS year,
        CASE
            WHEN EXTRACT(week FROM calendar_date) <= 5 THEN 1
            WHEN EXTRACT(week FROM calendar_date) <= 10 THEN 2
            ELSE 3
        END AS quarter,
        CASE
            WHEN EXTRACT(week FROM calendar_date) % 4 = 0 THEN 4
            WHEN EXTRACT(week FROM calendar_date) % 4 = 1 THEN 5
            WHEN EXTRACT(week FROM calendar_date) % 4 = 2 THEN 1
            ELSE 2
        END AS month,
    FROM
        calendar_dates
),

calender_summary AS (
    -- Generate results
    SELECT
        calendar_date,
        quarter,
        month,
        EXTRACT(week FROM calendar_date) AS week,
        CASE
            WHEN month = 1 AND EXTRACT(week FROM calendar_date) < 5 THEN '5'
            WHEN month = 2 THEN '5'
            ELSE '4'
        END AS week_structure
    FROM
        weeks
)

SELECT *
FROM calendar_dates
"


con <- DBI::dbConnect(duckdb::duckdb())

tbl(
  con
  ,dplyr::sql(sql)
  )

library(tidyverse)
devtools::load_all()
devtools::document()
x <- mtd(sales,order_date,margin,"standard")
y <- pmtd(sales,order_date,margin,"standard",1) |> calculate() |> collect()


x |>
  calculate() |> collect() |>
  select(-missing_date_indicator) |>
  full_join(
    y
    ,by=join_by(date,year,month)
  ) |>
  arrange(date) |> view()



devtools::test()
