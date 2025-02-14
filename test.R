library(tidyverse)
library(assertthat)
library(S7)
library(rlang)
library(dbplyr)
devtools::document()

x <- fpaR::ytd(.data = fpaR::sales,.date = order_date,.value = margin,calendar_type = "standard")
y <- fpaR::pytd(.data = fpaR::sales,.date = order_date,.value = margin,calendar_type = "standard",lag_n = 1)
z <- fpaR::yoytd(fpaR::sales,.date = order_date,.value = margin,calendar_type = "standard",lag_n = 1)
a <- fpaR::yoy(fpaR::sales,.date = order_date,.value = margin,calendar_type = "standard",lag_n = 1)

b <- fpaR::ytdopy(fpaR::sales,.date = order_date,.value = margin,calendar_type = "standard",lag_n = 1)

x@fn@fn_exec

x |> calculate()



sales_db |>
  dplyr::pull(dplyr::any_of(date_vec)) |>
  max(na.rm=TRUE)

db$sales |>
dplyr::pull(dplyr::any_of(date_vec)) |>
  unique() |>
  length()






























#factor----------------------
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



sales |>
  totalwtd(date = order_date,quantity,type = "standard")
  calculate()


#'
#' #' Augment time attributes
#' #'
#' #' @param .data
#' #' @param date_var
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' augment_time_attributes <- function(.data,date_var){
#'
#'
#'   # set up variables
#'
#'
#'   date_var <- enquo(date_var)
#'
#'   #create new vars
#'
#'   .data <-   .data %>%
#'     mutate(
#'       year=lubridate::year(!!date_var)
#'       ,year_abb=stringr::str_sub(as.character(year),start=3,end=4)
#'       ,quarter=lubridate::quarter(!!date_var)
#'       ,month_number=lubridate::month(!!date_var)
#'       ,month_number_padded=if_else(str_length(month_number)<2,paste0("0",month_number),as.character(month_number))
#'       ,month_name_short=lubridate::month(!!date_var,abbr = TRUE,label=TRUE)
#'       ,month_name_long=lubridate::month(!!date_var,label=TRUE,abbr = FALSE)
#'       ,day_of_week_number=wday(!!date_var,label=FALSE)
#'       ,day_of_week_label=lubridate::wday(!!date_var,label=TRUE)
#'       ,day_of_month=lubridate::day(!!date_var)
#'       ,days_in_month=lubridate::days_in_month(!!date_var)
#'       ,week_number_of_year=lubridate::week(!!date_var)
#'       ,leap_year_indicator=lubridate::leap_year(!!date_var)
#'       ,semester=lubridate::semester(!!date_var)
#'       ,semester_year=lubridate::semester(!!date_var,with_year=TRUE)
#'       ,year_month=paste0(year,"_",month_number)
#'       ,year_month_padded=paste0(year,"_",month_number_padded)
#'       ,year_wk=base::paste0(year,"_",1)
#'       ,year_quarter=lubridate::quarter(!!date_var,with_year = TRUE)
#'       ,quarter_year_full=base::paste0(quarter,"Q",year)
#'       ,quarter_year_abb=base::paste0(quarter,"Q",year_abb)
#'      )
#'   #returning object
#' return(.data)
#'
#' }
#'
#'
#' #' create 554 calendar
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' make_554_cal <- function(start_date,end_date){
#'
#' calendar_raw <- tibble(date=seq.Date(from=ymd("2018-02-04"),to=ymd("2023-12-31"),by="days"))
#'
#' calendar_tbl <- calendar_raw %>%
#'   mutate(
#'     date_id=row_number()
#'    ,year_id=
#'
#'   case_when(
#'     date < ymd("2019-02-03") ~ tibble(yr_key=1,yr_label=2018)
#'     ,date < ymd("2020-02-02") ~ tibble(yr_key=2,yr_label=2019)
#'     ,date < ymd("2021-01-31") ~ tibble(yr_key=3,yr_label=2020)
#'     ,date < ymd("2022-01-30") ~ tibble(yr_key=4,yr_label=2021)
#'     ,date < ymd("2023-01-29") ~ tibble(yr_key=5,yr_label=2022)
#'     ,TRUE ~ tibble(yr_key=6,yr_label=2023)
#'   )
#' ) %>%
#'   group_by(
#'     year_id
#'   ) %>%
#'   mutate(
#'     day_id=row_number()
#'     ,wk_period=
#'       # cumsum(
#'         case_when(
#'           day_id%%7==0 ~ day_id
#'           ,TRUE ~NA_integer_
#'           )
#'       # )+1
#'   ) %>%
#'   fill(wk_period,.direction = "up") %>%
#'   mutate(
#'     wk_period=wk_period/7
#'   ) %>%
#'   ungroup()
#' return(calendar_tbl)
#' }
#
