# unit test for cohort

## have a quosure issue

## non-standard calendar

# - generate a non-standard calendar table
# - understnadn

## factors


## sql-lm



library(tidyverse)
library(devtools)
load_all()
.data <- cohorts::online_cohorts |> janitor::clean_names()

document()
cohort_control_tbl <- cohorts::cohort_table_day(.data,id_var = customer_id,date = invoice_date) |>
  mutate(
    across(
      everything(),~replace_na(.,0)
    )
  ) |>
  janitor::clean_names() |>
  pivot_longer(-1,values_to = "control_value")

cohort_test_tbl <- cohort(.data,.date=invoice_date,calendar_type = "standard",.value = customer_id,time_unit = "day",period_label = FALSE) |>
  calculate() |>
  collect() |>
  select(
    cohort=cohort_id
    ,everything()
    ,-c(cohort_date)
  ) |>
  pivot_longer(-1,values_to = "test_value")



cohort_test_tbl |>
full_join(
  cohort_control_tbl
  ,by=join_by(cohort,name)
) |>
  mutate(
    delta=test_value-control_value
  ) |>
  summarise(
    sum_errors=sum(abs(delta))
  ) |>
  pull(sum_errors)

cohort_test_tbl |>
  pivot_longer(-1)

identical(cohort_control_tbl,cohort_test_tbl)
