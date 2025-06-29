

## make print class for abc object (2 hours)

### rename calendar class to data class

### add addition action methods to action method
### have abc use value class
###

## complete tests for ti class (2 hours)

## make non-standard calendar () 4 hours

## convert website to bookdown 4 hours

## factor analysis??

## lm sql

rm(list = c("print_fn_info", "print_next_steps"))
#

devtools::document()
devtools::load_all()
library(tidyverse)

.data <-  sales |>
  group_by(customer_key)


x <- segment(
  data =.data
  ,category_values = c(.7,.9,1)
  ,value_vec = "margin"
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
)
print_fn_info(x)


devtools::document()




sales |>
  group_by(customer_key) |>
  abc(category_values = c(.7,.8,.96,1))
  calculate()
  count(category_name,category_value) |>
  arrange(category_value)



##



