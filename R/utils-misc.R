#' Validate an input is YYYY-MM-DD format
#'
#' @param x date column
#'
#' @return logical
#'
is_yyyy_mm_dd <- function(x) {

out <-   suppressWarnings(!is.na(lubridate::ymd(x)))

return(out)


}






generate_cli_action <- function(x,word){

  out <- list()
  if(any(x %in% stringr::str_to_lower(word))){

    out$word <- c(cli::col_green(cli::symbol$tick),stringr::str_to_title(word))

  }else{

    out$aggregate <- c(cli::col_red(cli::symbol$cross),stringr::str_to_title(word))
  }

  return(out)
}



#' Make Action field CLI args
#'
#' @param x action class
#'
#' @returns list
#'
make_action_cli <- function(x){

  out <- list()

  # if(any(x %in% c("aggregate"))){
  #
  #   out$aggregate <- paste0(cli::col_green(cli::symbol$tick)," Aggregate")
  #
  # }else{
  #
  #   out$aggregate <- c(cli::col_red(cli::symbol$cross),"Aggregate")
  # }

  out <- generate_cli_action(x,"aggregate")

  out <- generate_cli_action(x,"shift")

  out <- generate_cli_action(x,"compare")





  # if(any(x %in% c("shift"))){
  #
  #   out$shift <- paste0(cli::col_green(cli::symbol$tick)," Shift")
  #
  # }else{
  #   out$shift <- c(cli::col_red(cli::symbol$cross)," Shift")
  # }

  # if(any(x %in% c("compare"))){
  #
  #   out$compare <- paste0(cli::col_green(cli::symbol$tick)," Compare")
  #
  # }else{
  #
  #   out$compare <- c(cli::col_red(cli::symbol$cross)," Compare")
  # }
#
#   if(any(x %in% c("% of total"))){
#
#     out$compare <- paste0(cli::col_green(cli::symbol$tick)," % of total")
#
#   }else{
#
#     out$compare <- c(cli::col_red(cli::symbol$cross)," % of total")
#   }



  return(out)

}



#' Prints function header info
#'
#' @param x ti or segment obj
#'
#' @returns print
#' @export
#'
print_fn_info <- function(x) {

  cli::cli_h1(x@fn@fn_long_name)
  cli::cli_text("Function: {.code {x@fn@fn_name}} was executed")
  cli::cli_h2("Description:")
  cli::cli_par()
  cli::cli_text(x@action@method)
}

#' Prints functions next steps
#'
#' @returns print
#' @export
#'
#' @examples
#' print_next_steps()
print_next_steps <- function(){

  cli::cli_h2("Next Steps:")

  cli::cli_li("Use {.code calculate()} to return the results")

  cli::cli_rule()
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


