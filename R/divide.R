
#' Divide function with error handling for divide by zero or NA
#' @description
#' A safe divide function that will catch info or NA values and return an alternative result
#' This is tibble or DBI friendly and will return same class as input
#'
#' @param .data a tibble or DBI object
#' @param new_col_name new column name for result
#' @param numerator column for numerator
#' @param denominator column for denominator
#' @param alternative_result alternative results if divide results are inf or NA, must be numeric
#'
#' @return tibble or dbi object
#' @export
#'
#' @examples
#' divide(mtcars,new_col_name=div_col,numerator=mpg,denominator=0,alternative_result=10)
divide <- function(.data,new_col_name,numerator,denominator,alternative_result=NA_integer_){



  out <- .data |>
    dplyr::mutate(
      "{{new_col_name}}":=dplyr::if_else(base::is.na({{numerator}}/{{denominator}})|base::is.infinite({{numerator}}/{{denominator}}),alternative_result,{{numerator}}/{{denominator}})
    )

  base::return(out)

}
