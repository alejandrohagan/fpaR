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




#### time intelligence functions



time_unit <- S7::new_class(
  ,name="time_unit"
  ,properties = list(
    value=S7::new_property(
      class=S7::class_character
      ,default = "day"
      ,setter=\(self,value){
        value <- stringr::str_to_lower(value)
        self@value <- value
        self
      }
      ,validator = \(value){
        if(length(value)!=1) cli::format_error("Please enter only one time unit")
      }
    )
  )
  ,validator = \(self){
    valid_units <- c("day","week","month","quarter","year")
    if(!any(self@value %in% valid_units))  return(cli::format_error("Please only enter {valid_units}"))
  }
)




# action class

action <- S7::new_class(
  name="action"
  ,properties=list(
    value=S7::new_property(
      class=S7::class_character
      ,setter = \(self,value){
        value <- stringr::str_to_lower(value)
        self@value <- value
        self
      }
    )
  )
  ,validator = \(self){
    if(!any(self@value %in% c("shift","compare","aggregate"))) return('Action must return "shift","compare" or "aggregate"')
  }
)


## calendar class

calendar_tbl <- new_class(
  name="calendar_tbl"
  ,properties =
    list(
      data=S7::new_property(
        class=class_data.frame
        )
      ,type=S7::new_property(
        class=S7::class_character
        ,validator = \(value){
          if(!any(value %in% c("standard","554"))) return(cli::format_error("Please return either 'standard' or '554'"))
          }
        ,setter=\(self,value){
          value <- stringr::str_to_lower(value)
          self@type <- value
          self
          }
        )
      ,date_vec=S7::new_property(
        class=class_any
        )
      ,date_quo=S7::new_property(
        class=S7::class_any
        ,getter=\(self){
          x <- rlang::parse_expr(self@date_vec)
          x
          }
        )
      ,min_date=S7::new_property(
        class=S7::class_numeric
        ,getter=\(self){
          x <-  self@data |>
            dplyr::pull(dplyr::any_of(self@date_vec)) |>
            min(na.rm=TRUE)
          x
          }
        )
      ,max_date=S7::new_property(
        class=S7::class_numeric
        ,getter=\(self){
          x <-  self@data |>
            dplyr::pull(dplyr::any_of(self@date_vec)) |>
            max(na.rm=TRUE)
          x
          }
        )
      ,date_range=S7::new_property(
        class=S7::class_numeric
        ,getter =\(self){
          x <- as.numeric(self@max_date-self@min_date)
          x
          }
        )
      ,date_count=S7::new_property(
        class=S7::class_numeric
        ,getter=\(self){
          x <- self@data |>
            dplyr::pull(self@date_quo) |>
            unique() |>
            length()
          x
          }
        )
      ,date_missing=S7::new_property(
        class=S7::class_numeric
        ,getter=\(self){
          x <- as.numeric(self@date_range-self@date_count)
          x
          }
        )
      ,group_indicator=S7::new_property(
        class=S7::class_logical
        ,getter=\(self){
          x <- dplyr::if_else(!purrr::is_empty(dplyr::groups(self@data)),TRUE,FALSE)
          x
          }
        )
      ,group_quo=S7::new_property(
        class=S7::class_any
        ,getter = \(self){
          x <- dplyr::groups(self@data)
          x
          }
        )
      ,group_vec=S7::new_property(
        class=S7::class_any
        ,getter = \(self){
          x <-  as.character(unlist(dplyr::groups(self@data)))
          x
          }
        )
      ,group_count=S7::new_property(
        class=S7::class_numeric
        ,getter=\(self){
          if(!self@group_indicator){

            x <- 0
            x

            }else{
              x <-   length(self@group_vec)
              x
            }
          }
        )
      )
  )






### ti table

ti_tbl <- S7::new_class(
  name="ti_tbl"
  ,properties = list(
   calendar_tbl=calendar_tbl
   ,time_unit=time_unit
    ,value_vec=S7::new_property(class=class_character)
    ,value_quo=S7::new_property(
      class=S7::class_any
      ,getter=\(self){
        x <- rlang::parse_expr(self@value_vec)
        x
      }
    )
    ,new_column_name=S7::new_property(class=S7::class_character)
    ,sort_logic=S7::new_property(class=S7::class_logical)
    #custom classes
    ,action=S7::new_property(class=action)
  )
  ,validator = \(self){

    if(!any(self@calendar_tbl@data |>  dplyr::pull(self@calendar_tbl@date_vec) |> class() %in% c("Date"))){

      return(cli::format_error("'{self@date_vec}' is not in Date format"))
    }
  }
)


totalytd_tbl <- new_class("totalytd_tbl"
                          ,parent = ti_tbl)



totalytd <- function(.data,date,value,type){

  # Validate inputs
  assertthat::assert_that(base::is.data.frame(.data), msg = "data must be a data frame")



  out <- totalytd_tbl(
    calendar_tbl(
      data=.data
      ,type =type
      ,date_vec = rlang::as_label(rlang::enquo(date))
    )
    ,time_unit = time_unit("day")
    ,action=action("aggregate")
    ,value_vec = rlang::as_label(rlang::enquo(value))
    ,new_column_name = "ytd"
    ,sort_logic = TRUE
  )

  return(out)
}


x <- totalytd(
    .data=sales |> group_by(store_key,currency_code,product_key)
    ,value = unit_price
    ,date = order_date
    ,type="standard"
  )

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

  group_count <- x@calendar_tbl@group_count
  value_chr <- x@value_vec
  show <- cli::cli_div(theme = cli::simple_theme())

  cli::cli_h1("Total Year To Date: totalytd")
  cli::cli_h2("Description:")
  cli::cli_par()
  cli::cli_text("This will create a rolling sum of {.field {value_chr}}, from the beginning to the end of the year")

  cli::builtin_theme()

  cli::cli_h2("Calendar:")
  cli::cat_bullet(paste("The calendar was aggregated to the",cli::col_yellow(x@time_unit@value),"time unit"))
  cli::cat_bullet(cli::cli_text("A ",cli::bg_br_white(cli::col_br_red(x@calendar_tbl@type))," calendar is created with {group_count} group{?s}"))
  cli::cat_bullet(paste("Calendar ranges from",cli::col_br_green(x@calendar_tbl@min_date),"to",cli::col_br_green(x@calendar_tbl@max_date)))
  cli::cat_bullet(paste(cli::col_blue(x@calendar_tbl@date_missing),"days are missing and replaced"))

  cli::cli_h2("Actions:")


  cat(cli::col_green(symbol$tick), " Aggregate\n", cli::col_red(symbol$cross), " Shift\n", cli::col_red(symbol$cross), " Compare\n", sep = "")


  cli::cli_blockquote("this is a block quote -- what else can it do?")
  cli::cli_code("totalytd()")
  # cli::cli_par()
  cli::cli_rule()
    cli::cli_text("Use {.fn calculate} to return results")
  cli::cli_end(show)
}



sales|>
  group_by(store_key) |>
  totalytd(date = order_date,value = unit_price,type="standard")
  calculate()

?cli::spark_bar(c(1,2,5,1,10))
  x <- seq(0, 1, length = 6)
  spark_bar(x)

  previous_count <- x@data |> pull(x@date_vec) |> unique() |> length()

  current_count <- full_tbl |> pull(date) |> unique() |> length()

  missing_dates <- current_count-previous_count




  full_tbl <-
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

  names(letters) <- 1:26

