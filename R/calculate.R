#' Calculate to filter, summarize and pivot data
#'
#' @param .data dataframe or tibble
#' @param .fn quote aggregation function
#' @param rows category in rows
#' @param cols category in cols
#' @param filter filter category, use & instead of ,
#'
#' @return
#' @export
#'
#' @examples
#' calculate(ggplot2::diamonds,.fn="min(price)",rows=cut,cols=color,filter='color=="D"&cut=="Fair"')

calculate <- function(.data,.fn,rows,cols,filter){

  # capture funcitons

  expr_fn <- rlang::expr(.fn)

  # capture filter args
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


pop <- function(.data,date_var,var,fn=c("sum","mean","min","max","quantile"),period=c("day","week","month","bimonth","quarter","halfyear","year"),n=1,na.rm=TRUE,probs=.5){

  # data check

date_var <- dplyr::enquo(date_var)


lubridate::is.Date(.data[[rlang::as_label(date_var)]])



# match args of period
 period <- match.arg(period)

 .data <-  .data  %>%
    mutate(
      period=lubridate::floor_date(!!date_var,unit=period)
    ) %>%
    group_by(dplyr::pick(period),.add = TRUE)

# match args
fn <- match.arg(fn)

var <- enquo(var)



# match functions
fn_exec <- switch(
  fn
  ,sum=sum
  ,mean=mean
  ,min=min
  ,max=max
  ,quantile=quantile
)

fn <- rlang::sym(fn)
    .data %>%
    summarise(
      "{{fn}}_{{var}}":=eval(fn_exec({{var}},na.rm=na.rm,probs=probs))
      ,.groups = "drop"
    ) %>%
    mutate(
      lag_var=lag(.data[[rlang::englue("{{fn}}_{{var}}")]],n={{n}})
      ,delta=.data[[rlang::englue("{{fn}}_{{var}}")]]-lag_var
    ) %>%
    select(period,lag_var,delta,.data[[rlang::englue("{{fn}}_{{var}}")]])


}




pp <- function(.data,date_var,var,fn=c("sum","mean","min","max","quantile"),period=c("day","week","month","bimonth","quarter","halfyear","year"),n=1,na.rm=TRUE,probs=.5){

  # data check

  date_var <- dplyr::enquo(date_var)


  lubridate::is.Date(.data[[rlang::as_label(date_var)]])



  # match args of period
  period <- match.arg(period)

  .data <-  .data  %>%
    mutate(
      period=lubridate::floor_date(!!date_var,unit=period)
    ) %>%
    group_by(dplyr::pick(period),.add = TRUE)

  # match args
  fn <- match.arg(fn)

  var <- enquo(var)



  # match functions
  fn_exec <- switch(
    fn
    ,sum=sum
    ,mean=mean
    ,min=min
    ,max=max
    ,quantile=quantile
  )

  fn <- rlang::sym(fn)
  .data %>%
    summarise(
      "{{fn}}_{{var}}":=eval(fn_exec({{var}},na.rm=na.rm,probs=probs))
      ,.groups = "drop"
    ) %>%
    mutate(
      lag_var=lag(.data[[rlang::englue("{{fn}}_{{var}}")]],n={{n}})
    ) %>%
    select(period,lag_var)


}



# note add comments and weiting
pop(sales,date_var = date_key,var = unit_price,fn = "sum",period = "quarter",n = 1,na.rm = TRUE,probs = .99)

# note add comments and weiting
pp(sales,date_var = date_key,var = unit_price,fn = "sum",period = "quarter",n = 1,na.rm = TRUE,probs = .99)


sales %>%
  group_by(date_key) %>%
  summarise(
    qnty=sum(sales_quantity)
    ,.groups="drop"
  ) %>%
  group_by(year=lubridate::week(date_key)) %>%
  mutate(
    ytd=cumsum(qnty)
  )

# make utilities that make

