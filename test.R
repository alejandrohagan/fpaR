library(tidyverse)
library(assertthat)
library(S7)
devtools::document()
devtools::load_all()

options(error=NULL)

# sales |> # dataframe
#   target(value) |> # an function that retuns a factor_tbl class
#   factor(price~lag(net_price)*quantity) |> # a method that returns a factor_tlb class and prints what it is doing
#   factor(price~lag(net_price)*quantity) |> # a method that returns a factor_tlb class and prints what it is doing
#   factor(price~lag(net_price)*quantity) |> # a method that returns a factor_tlb class and prints what it is doing
#   calculate() # returns the ouput



factor_tbl <- new_class(
  "factor_tbl"
  ,properties = list(
    data=new_property(
      class=class_data.frame
    )
    ,target=class_character
    ,formula=class_list

    )
  )


target <- function(data,value){

  value_chr <- rlang::as_label(enquo(value))

 out <-  factor_tbl(
    data=data
    ,target = value_chr
    ,formula = rlang::list2()
  )
  return(out)

}



sales |>
  target(store_key)


factor <- new_generic("factor","x")

method(factor,factor_tbl) <- function(x,formula){

# extract formula lhs name
lhs_chr <-  rlang::f_lhs(formula) |> as_label()

# extract previous names
names_lst <- names(x@formula)
# combine names
new_names_lst <- c(names_lst,lhs_chr)

# combine formulas in new list
x@formula <- append(x@formula,formula)

# set names to formulas
x@formula <- set_names(x@formula,new_names_lst)

# add inputs to factor_tbl class
out <-   factor_tbl(data=x@data,target=x@target,formula=x@formula)
out
}

test <- sales |>
  target(store_key) |>
  factor(vs~mpg+help) |>
  factor(mpg~vs+help)



totalqtd_tbl <- S7::new_class(
  "totalqtd_tbl"
  ,parent = ti_tbl
  )





totalqtd <- function(.data,...,date,value,type){

  # Validate inputs
  assertthat::assert_that(base::is.data.frame(.data), msg = "data must be a data frame")

  # Aggregate data based on provided time unit


  out <- totalqtd_tbl(
    calendar_tbl(
      data=.data
      ,type =type
      ,date_vec = rlang::as_label(rlang::enquo(date))
    )
    ,time_unit = time_unit("day")
    ,action=action("aggregate")
    ,value_vec = rlang::as_label(rlang::enquo(value))
    ,new_column_name = "qtd"
    ,sort_logic = TRUE
    ,fn="totalqtd"
  )

  return(out)

}


