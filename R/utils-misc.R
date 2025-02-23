#' Validate an input is YYYY-MM-DD format
#'
#' @param x date column
#'
#' @return logical
#'
#' @examples
#' is_yyyy_mm_dd("2024-01-01")
is_yyyy_mm_dd <- function(x) {

out <-   suppressWarnings(!is.na(lubridate::ymd(x)))

return(out)


}

#' Convert quoted or unquoted input to string
#'
#' @param x quoted or unquoted input
#'
#' @return string
#'
#' @examples
#' convert_input_to_string(hello)
convert_input_to_string <- function(x) {


  var_quo <- rlang::enquo(x)
  var_expr <- rlang::quo_get_expr(var_quo)



    if (rlang::is_symbolic(var_expr) || rlang::is_call(var_expr)) {

    out <- as_label(var_expr)

    return(out)

  } else {
    out <- x

    return(out)
  }

}



#' Capture dot arguments and turn into strings
#'
#' @param ... dot args
#'
#' @return strings
#'
#' @examples
#' convert_dots_to_string(hello,how,are,you)
convert_dots_to_string <- function(...){

  args <- rlang::enquos(...)

  group_var <- map_chr(args,\(x) convert_input_to_string({{x}}))

  return(group_var)

}



#' Converts tibble to csv file and opens up in excel
#'
#' @param .data tibble of data
#'
#' @return a temp csv file
#' @export
#'
#' @examples
#' mtcars |>  show_in_excel()
show_in_excel <- function(.data){

  # supporting arguments
  rows      <- base::dim(.data)[1]
  cols      <- base::dim(.data)[2]

  # send message based on table row size

  cli::cli_alert("data contains {rows} row(s) and {cols} column(s)")

  # create temp file path to be used later
  temp <- base::paste0(base::tempfile(),".csv")

  # save the table as a csv file
  utils::write.csv(.data,file = temp)

  # open up the csv file
  fs::file_show(path=temp)

  # alert that successful
  cli::cli_alert_success("success: temp file {temp}")

}


#' Make Action field CLI args
#'
#' @param x
#'
#' @returns list
#'
#' @examples
#'x <- ytd(fpaR::sales,.date=order_date,.value=quantity,calendar_type="standard")
#'make_action_cli(x@action@value)
make_action_cli <- function(x){

  out <- list()

  if(any(x %in% c("aggregate"))){
    out$aggregate <- c(cli::col_green(cli::symbol$tick)," Aggregate")
  }else{
    out$aggregate <- c(cli::col_red(cli::symbol$cross),"Aggregate")
  }

  if(any(x %in% c("shift"))){
    out$shift <- c(cli::col_green(cli::symbol$tick)," Shift")
  }else{
    out$shift <- c(cli::col_red(cli::symbol$cross)," Shift")
  }

  if(any(x %in% c("compare"))){
    out$compare <- c(cli::col_green(cli::symbol$tick)," Compare")
  }else{
    out$compare <- c(cli::col_red(cli::symbol$cross)," Compare")
  }

  return(out)

}

#' Print message
#'
#' @param x ti class object
#'
#' @returns print message
#'
#' @examples
#'  x <- fpaR::ytd(fpaR::sales,date = order_date,value=quantity,calendar_type = "standard")
#'  make_print_message(x)

make_print_message <- function(x){

  x <- fpaR::ytd(fpaR::sales,date = order_date,value=quantity,calendar_type = "standard")

  ## subset function descriptions from table

  function_tbl <- fpaR::functions |>
    dplyr::filter(
      fn_name_lower==!!x@fn
    )

  ## prep variables for use later on

  value_chr <- x@value_vec
  current_date <- "January 1st"
  end_date <- "December 31"
  group_count <- x@calendar_tbl@group_count
  current_calendar_year <- "Current calendar year"


  ## start print message

  ### generatl informatoin

  cli::cli_h1(function_tbl$short_name)
  cli::cli_code(function_tbl$fn_name_lower)
  cli::cli_h2("Description:")
  cli::cli_par()

  cli::cli_text(function_tbl$method)



  cli::cli_text("This will create a sum of quantity, from the beginning of the calendar year to the {cli::col_green('current date')}")


  cli::cli_text(test)

  cli::builtin_theme()

  ### Calendar information


  cli::cli_h2("Calendar:")
  cli::cat_bullet(paste("The calendar was aggregated to the",cli::col_yellow(x@time_unit@value),"time unit"))

  cli::cli_text("A ",cli::col_br_red(x@calendar_tbl@calendar_type)," calendar is created with ",cli::col_green("{group_count} group{?s}"))

  cli::cat_bullet(paste("Calendar ranges from",cli::col_br_green(x@calendar_tbl@min_date),"to",cli::col_br_green(x@calendar_tbl@max_date)))
  cli::cat_bullet(paste(cli::col_blue(x@calendar_tbl@date_missing),"days were missing and replaced with 0"))
  cli::cli_text("New date column ",cli::col_br_red(x@new_date_column_name)," was created")

  ## Action information

  cli::cli_h2("Actions:")

  cli::cli_text(x@action@value[1])
  cli::cli_text(x@action@value[2])
  cli::cli_text(x@action@value[3])

 ## print groups if groups exist


  if(x@calendar_tbl@group_indicator){

    cli::cli_text("{stringr::str_flatten_comma(x@calendar_tbl@group_vec,last = ' and ')} groups are in the table")

  }

  ## Next Steps information

  cli::cli_h2("Next Steps:")

  cli::cli_rule()

  cli::cli_li("Use {.code calculate()} to return the results")

  cli::cli_end()

}



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


  # Validate alternative_result is numeric
  assertthat::assert_that(
    is.numeric(alternative_result),
    msg = cli::format_error(c(
      "x" = "Alternative result must be numeric.",
      "!" = "You provided a value of class {.cls {class(alternative_result)}}."
    ))
  )

  # If .data is a remote (DBI) table, build a safe SQL expression
  if (inherits(.data, "tbl_dbi")) {

    # Convert column names to symbols then to strings
    num_str <- rlang::as_string(rlang::ensym(numerator_col))
    denom_str <- rlang::as_string(rlang::ensym(denominator_col))

    # Create SQL identifiers (this handles quoting as needed)
    num_id <- dbplyr::ident(num_str)
    denom_id <- dbplyr::ident(denom_str)

    # Build a SQL CASE expression:
    # CASE WHEN denominator IS NULL OR denominator = 0 THEN alternative_result ELSE numerator/denom END
    safe_divide_expr <- dplyr::sql(sprintf(
      "CASE WHEN %s IS NULL OR %s = 0 THEN %s ELSE %s/%s END",
      as.character(denom_id),
      as.character(denom_id),
      alternative_result,
      as.character(num_id),
      as.character(denom_id)
    ))

    out <- .data %>%
      dplyr::mutate({{ new_col_name }} := safe_divide_expr)

    return(out)

  } else {
    # For local data (e.g., a tibble in memory), use R's evaluation
    out <- .data %>%
      dplyr::mutate(
        {{ new_col_name }} := dplyr::if_else(
          base::is.na({{ numerator_col }} / {{ denominator_col }}) | base::is.infinite({{ numerator_col }} / {{ denominator_col }}),
          alternative_result,
          {{ numerator_col }} / {{ denominator_col }}
        )
      )

    return(out)
  }

}



utils::globalVariables(
  c(
  "pick", "desc", "var", "cum_sum", "prop_total", "row_id", "max_row_id",
  "dim_category", "cum_prop_total", "cum_unit_prop", "dim_threshold",
  ":=", "out_tbl", "dir_path", "files", "str_extract", "if_else", "str_remove",
  "map_chr", "as_label", "n", "prop_n", "lead", "pull", "relocate", "select",
  "order_date", "quantity", "fn_name_lower", "test", ".cluster", "centers_input",
  "kmeans_models", "kmeans_results", "tot.withinss", "sql", "quarter", "quater",
  "date_lag", "month", "week", "year")
  )


