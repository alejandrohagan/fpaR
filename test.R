library(tidyverse)
library(assertthat)
library(S7)
devtools::document()
devtools::load_all()

options(error=NULL)

sales |> # dataframe
  target(value) |> # an function that retuns a factor_tbl class
  factor(price~lag(net_price)*quantity) |> # a method that returns a factor_tlb class and prints what it is doing
  factor(price~lag(net_price)*quantity) |> # a method that returns a factor_tlb class and prints what it is doing
  factor(price~lag(net_price)*quantity) |> # a method that returns a factor_tlb class and prints what it is doing
  calculate() # returns the ouput



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



totalytd_tbl <- new_class("totalytd_tbl"
          ,parent = ti_tbl)




totalytd <- function(.data,date,value,type){

  # Validate inputs
  assertthat::assert_that(base::is.data.frame(.data), msg = "data must be a data frame")



  out <- totalytd_tbl(
    data = .data
    ,date_vec = rlang::as_label(enquo(date))
    ,time_unit = time_unit("day")
    ,type=type
    ,value_vec = rlang::as_label(enquo(value))
    ,action=action("aggregate")
    ,new_column_name = "ytd"
    ,sort_logic = TRUE
  )

  return(out)
}


x |> class()


calculate <- new_generic("calculate","x")

method(calculate,totalytd_tbl) <- function(x){

  # Aggregate data based on provided time unit

  full_tbl <-  x@data |>
    make_aggregation_tbl(date_var=!!x@date_quo,value_var=!!x@value_quo,time_unit=x@time_unit@value) |>
    dplyr::mutate(
      year=lubridate::year(date)
      ,.before = 1
    )



  out_tbl <- full_tbl |>
    dplyr::group_by(year) |>
    dplyr::arrange(date,.by_group = TRUE) |>
    dplyr::mutate(
      !!x@new_column_name:=base::cumsum(!!x@value_quo)
    ) |>
    dplyr::ungroup()

  return(out_tbl)

}


method(print,totalytd_tbl) <- function(x){

  cli::cli_h1("Total Year To Date: totalytd")
  cli::cli_h2("Description:")
  cli::cat_bullet((paste("This will create a rolling sum of",x@value_vec,"from the beginning to the end of the year")))
  cli::builtin_theme()
  cli::cli_h2("Calendar:")
  cli::cat_bullet(paste("The calendar was aggregated to the",x@time_unit@value,"time unit"))
  cli::cat_bullet(paste("A",x@type,"calendar is used with 0 groups"))
  cli::cat_bullet(paste("Calendar ranges from",x@min_date,"to",x@max_date))
  cli::cli_h2("Actions:")

  out <- tribble(
    ~name,~shift,~aggregate,~compare,
    "totalytd",NA,x@value_vec,NA
  )
  print(out)

}




sales|>
  totalytd(date = order_date,value = unit_price,type="standard") |>
  calculate()





