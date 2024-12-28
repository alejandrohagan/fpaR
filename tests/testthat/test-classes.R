
# create inputs
valid_input <- "day"
valid_input2 <- "DAY"
invalid_input <- c("day","week")
invalid_input2 <- "biweekly"
day_unit <-  time_unit(value=valid_input)
value <- day_unit@value
day_unit2 <-  time_unit(value=valid_input2)
value2 <- day_unit@value

ti_tbl_object <- ti_tbl(
  data = sales |> group_by(store_key)
  ,value_vec= "unit_price"
  ,time_unit=time_unit(value = "day")
  ,new_column = NA_character_
  ,sort_logic = TRUE
  ,fn=mean
  ,action=action(value = c("shift"))
  ,date_vec = "order_date"
  ,type = "standard"
)

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
     value,c("day")
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


test_that(
  "calendar_tbl returns valid class",{
    testthat::expect_equal(
      ti_tbl_object|> class()
      ,c("ti_tbl","calendar_tbl","fpa","S7_object")
    )
  }
)




