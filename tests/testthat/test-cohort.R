

## create tests

library(dplyr)
library(testthat)
# test data set
dat <- tidyr::crossing(
  date = seq.Date(from = lubridate::ymd("2020-01-01"), to = lubridate::ymd("2024-01-01"), by = "day"),
  customer = rep(letters[1:17], 86)
) |>
  dplyr::mutate(purchases = runif(24854))

testthat::test_that("Tibble input returns tibble output", {
  testthat::expect_s3_class(
    dat |> make_cohort_tbl(id_var = customer, date_var = date, time_unit = "week", period_label = TRUE),
    "data.frame"
  )
})


testthat::test_that("time_unit validate", {
  testthat::expect_error(
    dat |> fpaR::make_cohort_tbl(id_var = customer, date_var = date, time_unit = "weekly", period_label = TRUE)
  )
})


testthat::test_that("date_var validation", {
  testthat::expect_error(
    dat |> fpaR::make_cohort_tbl(id_var = customer, date_var = purchases, time_unit = "week", period_label = TRUE)
  )
})

testthat::test_that("Period label validation", {
  testthat::expect_error(
    dat |> fpaR::make_cohort_tbl(id_var = customer, date_var = date, time_unit = "week", period_label = 0)
  )
})


library(testthat)
library(dplyr)
library(lubridate)
library(tidyr)
library(assertthat)

test_that("make_cohort_tbl works correctly", {
  # Create a sample tibble
  sample_data <- tibble::tibble(
    id = c(1, 1, 2, 2, 3, 3, 4, 4),
    date = as.Date(c('2023-01-01', '2023-01-02', '2023-01-01', '2023-01-03', '2023-01-01', '2023-01-04', '2023-01-01', '2023-01-05'))
  )

  # Test for default parameters
  result <- make_cohort_tbl(sample_data, id, date)
  expect_true(is_tibble(result))
  expect_equal(nrow(result), 4)
  expect_equal(colnames(result)[1], "cohort_id")

  # Test for period_label = TRUE
  result <- make_cohort_tbl(sample_data, id, date, time_unit = "week", period_label = TRUE)
  expect_true(is_tibble(result))
  expect_equal(nrow(result), 4)
  expect_equal(colnames(result)[1], "cohort_id")

  # Test for invalid date_var
  expect_error(make_cohort_tbl(sample_data, id, "invalid_date"), "Please ensure date_var column is in Date format")

  # Test for invalid period_label
  expect_error(make_cohort_tbl(sample_data, id, date, period_label = "invalid"), "Please ensure period_label is TRUE or FALSE")

  # Test for invalid time_unit
  expect_error(make_cohort_tbl(sample_data, id, date, time_unit = "invalid"), "Please ensure time_unit matches 'day', 'week', 'month', 'year', 'halfyear', 'quarter', 'second', 'minute', 'hour', 'bimonth', 'season'")
})

