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


#' Make Action field CLI args
#'
#' @param x action class
#'
#' @returns list
#'
make_action_cli <- function(x){

  out <- list()

  if(any(x %in% c("aggregate"))){

    out$aggregate <- paste0(cli::col_green(cli::symbol$tick)," Aggregate")

  }else{

    out$aggregate <- c(cli::col_red(cli::symbol$cross),"Aggregate")
  }

  if(any(x %in% c("shift"))){

    out$shift <- paste0(cli::col_green(cli::symbol$tick)," Shift")

  }else{
    out$shift <- c(cli::col_red(cli::symbol$cross)," Shift")
  }

  if(any(x %in% c("compare"))){

    out$compare <- paste0(cli::col_green(cli::symbol$tick)," Compare")

  }else{

    out$compare <- c(cli::col_red(cli::symbol$cross)," Compare")
  }

  return(out)

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


