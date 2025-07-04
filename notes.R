

### add addition action methods to action method
### have abc use value class

## complete tests for ti class (2 hours)

## make non-standard calendar () 8 hours

## convert website to bookdown 4 hours

## factor analysis??

## lm sql

rm(list = c("print_fn_info", "print_next_steps"))
#

devtools::test()

devtools::document()
devtools::load_all()
library(tidyverse)

.data <-  sales |>
  group_by(customer_key)


segment()
devtools::document()

segment(
  data =data(data=sales,calendar_type = "standard",date_vec = "order_date")
  ,category_values = c(.7,.9,1)
  ,fn = fn(
    fn_exec               = abc_fn
    ,fn_name              = "ABC"
    ,fn_long_name         = "ABC Classification"
    ,lag_n                = NA_integer_
    ,new_date_column_name = NA_character_
  )
  ,action=action(
    method= "This calculates the cumulative distribution of {.field !!x@value_vec} by the {.field group_vec}.
      It then classifies the distribution by the {.field category_value} into {.field category_name}"
  )
  ,value=value(value_vec="margin")
)
print_fn_info(x)

out <- segment(
  datum=datum(sales,,calendar_type = "standard")
  ,category_values = c(.1,.2,.7,1)
  ,fn = fn(fn_exec=\(x) mean(x))
  ,action = action()
  ,value=value(value_vec="margin")
)



devtools::document()




sales |>
  wtd(.date=order_date,.value=margin,calendar_type="standard")
  group_by(customer_key) |>
  abc(category_values = c(.7,.8,.96,1))
  calculate()
  count(category_name,category_value) |>
  arrange(category_value)



##

time_unit <- S7::new_class(

    ,name="time_unit"
    ,package = "fpaR"
    ,properties = list(
      value=S7::new_property(
        class=S7::class_character
        ,default = "day"
        ,setter=setter_str_to_lower(self,value)
        ,validator =validator_length_one(value)
        )
      ,validator =validator_time_units(self)
      )
    )

ti()

devtools::document()
)
ti(
  # datum(
  #   data                       = sales
  #   ,calendar_type             = "standard"
  #   ,date_vec                  =  "order_date"
  # )
  time_unit                   = time_unit("day")
  ,action = action(
    value                      = c("aggregate")
    ,method                    = "This creates a daily {.code cumsum()} of the {cli::col_cyan('current quarter')}
                                   {.field {value_chr}} from the start of the {cli::col_yellow({calendar_type})} calendar
                                   quarter to the end of the quarter"
  )
  ,value = value(
    value_vec                  = "margin"
    ,new_column_name_vec           = "qtd"
  )
  ,fn=fn(
    new_date_column_name       = c("year","quarter")
    ,lag_n                     = NA_integer_
    ,fn_exec                   = mean
    ,fn_name                   = "qtd"
    ,fn_long_name              = "Quarter-to-date"
  )
)

