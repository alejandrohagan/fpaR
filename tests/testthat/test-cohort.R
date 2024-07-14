

## create tests


# test data set
dat <- crossing(
  date = seq.Date(from = ymd("2020-01-01"), to = ymd("2024-01-01"), by = "day"),
  customer = rep(letters[1:17], 86)
) |>
  mutate(purchases = runif(24854))

test_that("Tibble input returns tibble output", {
  testthat::expect_s3_class(
    dat |> make_cohort_tbl(id_var = customer, date_var = date, time_unit = "week", period_label = TRUE),
    "data.frame"
  )
})


test_that("time_unit validate", {
  testthat::expect_error(
    dat |> fpaR::make_cohort_tbl(id_var = customer, date_var = date, time_unit = "weekly", period_label = TRUE)
  )
})


test_that("date_var validation", {
  testthat::expect_error(
    dat |> fpaR::make_cohort_tbl(id_var = customer, date_var = purchases, time_unit = "week", period_label = TRUE)
  )
})

test_that("Period label validation", {
  testthat::expect_error(
    dat |> fpaR::make_cohort_tbl(id_var = customer, date_var = date, time_unit = "week", period_label = 0)
  )
})
