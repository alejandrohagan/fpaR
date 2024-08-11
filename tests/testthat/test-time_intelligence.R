
test_that("yoy - day success", {

  testthat::expect_no_error(
  fpaR::sales |> yoy(date_var = order_date,value_var=quantity,lag_n=1,time_unit="day")
  )
})


test_that("yoy - month success", {

  testthat::expect_no_error(
    fpaR::sales |> fpaR::yoy(date_var = order_date,value_var=quantity,lag_n=1,time_unit="month")
  )
})


test_that("yoy - year success", {

  testthat::expect_no_error(
    fpaR::sales |> fpaR::yoy(date_var = order_date,value_var=quantity,lag_n=1,time_unit="year")
  )
})


test_that("yoy - week fail ", {

  testthat::expect_error(
    fpaR::sales |> fpaR::yoy(date_var = order_date,value_var=quantity,lag_n=1,time_unit="week")
  )
})

