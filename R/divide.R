
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
#' data(mtcars)
#' mtcars |> divide(new_col_name=div_col,numerator=mpg,denominator=0,alternative_result=10)
divide <- function(.data,new_col_name,numerator_col,denominator_col,alternative_result=NA_integer_){


  assertthat::assert_that(
    is.numeric(alternative_result),
    msg = cli::format_error(c(
      "x" = "Alternative result must be numeric.",
      "!" = "You provided a value of class {.cls {class(alternative_result)}}."
    ))
  )

  out <- .data |>
    dplyr::mutate(
      "{{new_col_name}}":=dplyr::if_else(base::is.na({{numerator_col}}/{{denominator_col}})|base::is.infinite({{numerator_col}}/{{denominator_col}}),alternative_result,{{numerator_col}}/{{denominator_col}})
    )

  base::return(out)

}
