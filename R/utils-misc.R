#' Validate an input is YYYY-MM-DD format
#'
#' @param x date column
#'
#' @return logical
#' @export
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
#' @export
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
#' @export
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
#' @returns
#'
#' @examples
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
