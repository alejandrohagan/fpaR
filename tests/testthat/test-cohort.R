test_that("sum errors should equal 0", {

  # Cleaned data
  .data <- cohorts::online_cohorts |> janitor::clean_names()

  # Cohort Control Table
  cohort_control_tbl <- cohorts::cohort_table_day(.data, id_var = customer_id, date = invoice_date) |>
    dplyr::mutate(across(dplyr::everything(), ~tidyr::replace_na(., 0))) |>
    janitor::clean_names() |>
    tidyr::pivot_longer(-1, values_to = "control_value")

  # Cohort Test Table
  cohort_test_tbl <- cohort(.data, .date = invoice_date, calendar_type = "standard", .value = customer_id, time_unit = "day", period_label = FALSE) |>
    calculate() |>
    dplyr::collect() |>
    dplyr::select(cohort = cohort_id, dplyr::everything(), -c(cohort_date)) |>
    tidyr::pivot_longer(-1, values_to = "test_value")

  # Join and calculate delta
  result <- cohort_test_tbl |>
    dplyr::full_join(cohort_control_tbl, by = join_by(cohort, name)) |>
    dplyr::mutate(delta = test_value - control_value) |>
    dplyr::summarise(sum_errors = sum(abs(delta))) |>
    dplyr::pull(sum_errors)

  # Assert that sum of errors equals 0
  expect_equal(result, 0, tolerance = 1e-6)
})
