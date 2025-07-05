# unit test for cohort

## non-standard calendar

# - generate a non-standard calendar table
# - understnadn

## factors


## sql-lm



.data <- cohorts::online_cohorts |> janitor::clean_names()

document()

x <- cohort(.data,.date=invoice_date,calendar_type = "standard",.value = customer_id,time_unit = "day",period_label = TRUE)

x |> calculate()

