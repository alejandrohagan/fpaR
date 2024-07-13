

## create tests

test_that("Tibble input returns tibble output", {

  testthat::expect_s3_class(

    fpaR::contoso_fact_sales |>
      mutate(DateKey=lubridate::mdy(DateKey)) |>
      make_cohort_tbl(id_var=ProductKey,date_var=DateKey,time_unit = 'week',period_label =TRUE)


    ,"data.frame")
})


test_that("time_unit validate", {
  testthat::expect_error(

    fpaR::contoso_fact_sales |>
      dplyr::mutate(DateKey=lubridate::mdy(DateKey)) |>
      fpaR::make_cohort_tbl(id_var=ProductKey,date_var=DateKey,time_unit = 'weekly',period_label =TRUE)

  )
})


test_that("date_var validation", {
  testthat::expect_error(

    fpaR::contoso_fact_sales |>
      dplyr::mutate(DateKey=lubridate::mdy(DateKey)) |>
      fpaR::make_cohort_tbl(id_var=ProductKey,date_var=SalesKey,time_unit = 'week',period_label =TRUE)

  )
})

test_that("Period label validation", {
  testthat::expect_error(

    fpaR::contoso_fact_sales |>
      dplyr::mutate(DateKey=lubridate::mdy(DateKey)) |>
      fpaR::make_cohort_tbl(id_var=ProductKey,date_var=SalesKey,time_unit = 'week',period_label ="0")

  )
})

