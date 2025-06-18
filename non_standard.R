

# put indicator for imbalanced periods, missing dates indicator
## looks like we are doing days in current period and days in prior period as an easier indicator
## logic created -- but check functionality and put it into functions
## add labels to out output tables?
## also fixed error for period imbalance where it takes the sum instead of the max
# find sql logic for 554, 445, 13 month calendars

# put print method for imbalanced period for comparison functions
# Put description in print method of calendar description
# update readme examples
# balance_indicator="balance","as-is"

## two period imbalanc etypes -- either previous period has more days than current comparison period (length) or previous period has less comparions
## period days (short)
## complete_calendar() -- augment calendar attributes (Eg. days in month, week, start date end date, year start date, end date, etc)
## summary method -- summary calendar attributes (number of missing dates?)

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




