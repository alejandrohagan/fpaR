#' Calculate to filter, summarize and pivot data
#'
#' @param .data dataframe or tibble
#' @param .fn quote aggregation funtion
#' @param rows category in rows
#' @param cols cateogry in cols
#' @param filter filter category, use & instead of ,
#'
#' @return
#' @export
#'
#' @examples
#' calculate(ggplot2::diamonds,.fn="min(price)",rows=cut,cols=color,filter='color=="D"&cut=="Fair"')

calculate <- function(.data,.fn,rows,cols,filter){

  expr_fn=rlang::expr(.fn)
 filter_exp <- rlang::expr(filter)

.data %>%
    eval(filter_exp) %>%
    group_by(pick({{rows}}),pick({{cols}}))%>%
    summarise(out=eval(expr_fn)({{var}}))%>%
    pivot_wider(names_from={{cols}},values_from = out)


}
# library(tidyverse)
#
# calculate <- function(.data,.fn,rows,cols,filter){
#
#   expr_fn=rlang::parse_expr(.fn)
#   filter_exp <- rlang::parse_expr(filter)
#
#   .data %>%
#     filter(eval(filter_exp)) %>%
#     group_by(pick({{rows}}),pick({{cols}}))%>%
#     summarise(out=base::eval(expr_fn))%>%
#     pivot_wider(names_from={{cols}},values_from = out)
# }
#
# test <- sum(diamonds$price[diamonds$color=="D"])
#
# function(row,col,var,fn.,row=(color,color==D))
# diamonds %>%
#   group_by(color,cut) %>%
#   summarise(
#     sum=sum(price)
#     ,opt=sum(diamonds$price[diamonds$color=="D"])
#
#     )

usethis::use_r("wci")

#
# summary <- function(.data,x) {
#
#   funs <- c("mean"=mean, "median"=median, "sd"=sd, "mad"=mad, "IQR"=IQR)
#   map(funs, function(f) f(.data[[x]], na.rm = TRUE))
# }
#
# mtcars %>%
#   summary("mpg")
# enframe() %>%
#   unnest(value)
library(tidyverse)
sales <- fpaR::contoso_fact_sales %>%
  janitor::clean_names() %>%

  mutate(date_key=mdy(date_key))

## month over month
mom <- function(.data,var,fn,n=1){

 .data <-  .data  %>%
    mutate(
      month=lubridate::floor_date(date_key,unit="month")
    ) %>%
    group_by(month)

var <-  enquo(var)
var <- rlang::as_label(var)

fn <- enquo(fn)
fn <- rlang::as_label(fn)

fn_var_raw <- paste0(fn,"(",var,",na.rm=TRUE)")

fn_var <-  rlang::parse_expr(fn_var_raw)

    .data %>%
    summarise(
      fn_var=eval(fn_var)
      ,.groups = "drop"
    ) %>%
    mutate(
      lag_var=lag(fn_var,n={{n}})
      ,delta=fn_var-lag_var
    ) %>%
    select(month,delta)


}

mom(sales,var=sales_quantity,fn=sum)

## year over year
