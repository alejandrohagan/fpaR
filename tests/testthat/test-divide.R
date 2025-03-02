# Load necessary libraries
library(testthat)
library(dplyr)

# Define the test context
test_that("divide function works as expected", {

  # Sample data for testing
  data <- tibble(
    id = 1:5,
    sales = c(100, 200, NA, 400, 500),
    quantity = c(5, 10, 0, 20, 25)
  )

  # Test that the result is correct
  result <- divide(data, new_col_name =average_sale, numerator = sales, denominator = quantity)
  expected <- tibble(
    id = 1:5,
    sales = c(100, 200, NA, 400, 500),
    quantity = c(5, 10, 0, 20, 25),
    average_sale = c(20, 20, NA, 20, 20)
  )

  expect_equal(result, expected, tolerance = 1e-8)

  # Test that it handles NA and 0 in denominator properly
  result_na <- divide(data, new_col_name = average_sale, numerator = sales, denominator = quantity, alternative_result = -1)
  expected_na <- tibble(
    id = 1:5,
    sales = c(100, 200, NA, 400, 500),
    quantity = c(5, 10, 0, 20, 25),
    average_sale = c(20, 20, -1, 20, 20)
  )
  expect_equal(result_na, expected_na)


  # Test that it errors if alternative_result is not numeric
  expect_error(
    divide(data, new_col_name = average_sale, numerator = sales, denominator = quantity, alternative_result = "non-numeric"),
    "Alternative result must be numeric"
  )
})
