

test_that("ABC: Returns segment class", {

  out <- abc(sales |> dplyr::group_by(customer_key),category_values = c(.1,.4,.6,1))

  testthat::expect_s7_class(out,segment)

})




test_that("ABC: Expect failure - no grouped object", {



  testthat::expect_error(abc(sales,c(.1,.4,.6,1)))

})


