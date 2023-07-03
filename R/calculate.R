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
library(tidyverse)

calculate <- function(.data,.fn,rows,cols,filter){

  expr_fn=rlang::parse_expr(.fn)
  filter_exp <- rlang::parse_expr(filter)

  .data %>%
    filter(eval(filter_exp)) %>%
    group_by(pick({{rows}}),pick({{cols}}))%>%
    summarise(out=base::eval(expr_fn))%>%
    pivot_wider(names_from={{cols}},values_from = out)

}

