

# create generics -----------

create_calendar <- S7::new_generic("create_calendar","x")

calculate <- S7::new_generic("calculate","x")

# define methods

#' Create Calendar Table
#' @name create_calendar
#' @param x ti_tbl object
#'
#' @returns dbi object
#' @export
#' @seealso [fpaR::make_aggregation_dbi()] which is the DBI equivalent of this function
#' @description
#' `create_calendar()` summarizes a tibble to target time unit and completes the calendar to ensure
#' no missing days, month, quarter or years. If a grouped tibble is passed through it will complete the calendar
#' for each combination of the group
#' @details
#' This is in internal function to make it easier to ensure data has no missing dates to
#'  simplify the use of time intelligence functions downstream of the application.
#' If you want to summarize to a particular group, simply pass the tibble through to the `group_by()` argument
#' prior to function and the function will make summarize and make a complete calendar for each group item.
#'
S7::method(create_calendar,ti) <- function(x){

  ## summarize data table
  summary_dbi <- x@calendar@data |>
    dplyr::ungroup() |>
    make_db_tbl() |>
    dplyr::mutate(
      date = lubridate::floor_date(!!x@calendar@date_quo,unit = !!x@time_unit@value)
      ,time_unit=!!x@time_unit@value
    ) |>
    dplyr::summarise(
      !!x@value@value_vec:= sum(!!x@value@value_quo,na.rm=TRUE)
      ,.by=c(date,!!!x@calendar@group_quo)
    )

  #create calendar table

  calendar_dbi <- fpaR::seq_date_sql(start_date = x@calendar@min_date,end_date = x@calendar@max_date,time_unit = x@time_unit@value,con=dbplyr::remote_con(x@calendar@data))


  # Expand calendar table with cross join of groups
  if(x@calendar@group_indicator){

    calendar_dbi <- calendar_dbi |>
      dplyr::cross_join(
        summary_dbi |>
          dplyr::distinct(!!!x@calendar@group_quo)
      )
  }

  # Perform a full join to ensure all time frames are represented
  full_dbi <- dplyr::full_join(
    calendar_dbi
    ,summary_dbi
    ,by = dplyr::join_by(date,!!!x@calendar@group_quo)
  ) |>
    dplyr::mutate(
      !!x@value@value_vec:= dplyr::coalesce(!!x@value@value_quo, 0)
    )

  return(full_dbi)
}

#' Calculate
#' @name calculate
#' @param x ti object
#'
#' @returns dbi object
#' @export
#' @examples
#' ytd(fpaR::sales,.date=date,.value=quantity,calendar_type="standard") |>
#' calculate()
S7::method(calculate,ti) <- function(x){


    out <- x@fn@fn_exec(x)

  return(out)

}


#' method(print,ytd_tbl) <- function(x){
#'
#'   x@fn
#'
#'   group_count <- x@calendar@group_count
#'   value_chr <- x@value_vec
#'   show <- cli::cli_div(theme = cli::simple_theme())
#'
#'
#'
#'   cli::cli_h1("Year To Date:")
#'   cli::cli_code("ytd()")
#'   cli::cli_h2("Description:")
#'   cli::cli_par()
#'   cli::cli_text("This will create a cumulative sum of {.field {value_chr}}, from ",cli::col_blue("January 1st")," till ",cli::col_blue("December 31st"))
#'
#'   cli::builtin_theme()
#'
#'   cli::cli_h2("Calendar:")
#'   cli::cat_bullet(paste("The calendar was aggregated to the",cli::col_yellow(x@time_unit@value),"time unit"))
#'
#'   cli::cli_text("A ",cli::col_br_red(x@calendar@calendar_type)," calendar is created with ",cli::col_green("{group_count} group{?s}"))
#'
#'   cli::cat_bullet(paste("Calendar ranges from",cli::col_br_green(x@calendar@min_date),"to",cli::col_br_green(x@calendar@max_date)))
#'   cli::cat_bullet(paste(cli::col_blue(x@calendar@date_missing),"days were missing and replaced with 0"))
#'   cli::cli_text("New date column ",cli::col_br_red(x@new_date_column_name)," was created")
#'   cli::cli_h2("Actions:")
#'
#'   cli::cli_text(x@action@value[1])
#'   cli::cli_text(x@action@value[2])
#'   cli::cli_text(x@action@value[3])
#'
#'   if(x@calendar@group_indicator){
#'
#'   cli::cli_text("{stringr::str_flatten_comma(x@calendar@group_vec,last = ' and ')} groups are in the table")
#'
#'   }
#'
#'   cli::cli_rule()
#'
#'   cli::cli_li("Use {.code calculate()} to return the results")
#'
#'   cli::cli_end(show)
#' }
#'
#' method(print,dod_tbl) <- function(x){
#'
#'   make_print_message(x)
#'
#'
#' }
