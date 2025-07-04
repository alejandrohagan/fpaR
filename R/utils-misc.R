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






#' Generate CLI actions
#'
#' @param x input to test against
#' @param word the key word to validate
#'
#' @returns list
#'
generate_cli_action <- function(x,word){


  # x <- "test"
  # word <- "test"
  out <- list()

  if(any(x %in% stringr::str_to_lower(word))){

    out[[word]] <- c(cli::col_green(cli::symbol$tick),stringr::str_to_title(word))

  }else{

    out[[word]] <- c(cli::col_red(cli::symbol$cross),stringr::str_to_title(word))
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

  out[1] <- generate_cli_action(x,"aggregate")

  out[2] <- generate_cli_action(x,"shift")

  out[3] <- generate_cli_action(x,"compare")

  out[4] <- generate_cli_action(x,"proportion of total")

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


print_actions_steps <- function(x){

  cli::cli_h2("Actions:")


  cli::cli_text(x@action@value[[1]]," ",cli::col_blue(x@value@value_vec))

  cli::cli_text(x@action@value[[2]]," ",cli::col_green(na.omit(x@fn@lag_n))," ",cli::col_green(na.omit(x@fn@shift)))

  cli::cli_text(x@action@value[[3]]," ",cli::col_br_magenta(na.omit(x@fn@compare)))

  cli::cli_text(x@action@value[[4]]," ",cli::col_br_magenta(na.omit(x@fn@compare)))


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


