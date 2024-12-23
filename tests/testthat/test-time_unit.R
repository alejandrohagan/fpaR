
# create inputs
valid_input <- "day"
valid_input2 <- "DAY"
invalid_input <- c("day","week")
invalid_input2 <- "biweekly"
day_unit <-  time_unit(value=valid_input)
value <- day_unit@value
day_unit2 <-  time_unit(value=valid_input2)
value2 <- day_unit@value



# create tests
test_that(
  "Returns valid class",{
    testthat::expect_equal(
      time_unit(value=valid_input) |> class()
      ,c("time_unit","fpa","S7_object")
    )
  }
)


test_that(
  "valiation of input works",{
    testthat::expect_equal(
     value
      ,c("day")
    )
  }
)

test_that(
  "valiation of setter str_to_lower works",{
    testthat::expect_equal(
      value2
      ,c("day")
    )
  }
)

test_that(
  "error with valid multiple values",{
    testthat::expect_error(
      time_unit(value=invalid_input)
    )
  }
)




test_that(
  "error with invalid values",{
    testthat::expect_error(
      time_unit(value=invalid_input2)
    )
  }
)


