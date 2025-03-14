

# create generics -----------

create_calendar <- S7::new_generic("create_calendar","x")


calculate <- S7::new_generic("calculate","x")


complete_calendar <- S7::new_generic("calculate","x")


#' Create Calendar Table
#' @name create_calendar
#' @param x ti object
#'
#' @returns dbi object
#' @export
#' @description
#' `create_calendar()` summarizes a tibble to target time unit and completes the calendar to ensure
#' no missing days, month, quarter or years. If a grouped tibble is passed through it will complete the calendar
#' for each combination of the group
#' @details
#' This is in internal function to make it easier to ensure data has no missing dates to
#'  simplify the use of time intelligence functions downstream of the application.
#' If you want to summarize to a particular group, simply pass the tibble through to the [dplyr::group_by()] argument
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
      ) |>
      dplyr::mutate(
        missing_date_indicator=dplyr::if_else(is.na(!!x@value@value_quo),1,0)
        ,!!x@value@value_vec:= dplyr::coalesce(!!x@value@value_quo, 0)
      )

  }

  # Perform a full join to ensure all time frames are represented
  full_dbi <- dplyr::full_join(
    calendar_dbi
    ,summary_dbi
    ,by = dplyr::join_by(date,!!!x@calendar@group_quo)
  ) |>
    dplyr::mutate(
      missing_date_indicator=dplyr::if_else(is.na(!!x@value@value_quo),1,0)
      ,!!x@value@value_vec:= dplyr::coalesce(!!x@value@value_quo, 0)
    )

  return(full_dbi)
}



#' @title Calculate
#' @name calculate
#' @param x ti object
#'
#' @returns dbi object
#' @export
#' @examples
#' x <- ytd(sales,.date=order_date,.value=quantity,calendar_type="standard")
#' calculate(x)
S7::method(calculate,ti) <- function(x){


    out <- x@fn@fn_exec(x) |>
      dbplyr::window_order(date)

  return(out)

}




#' @title complete_calendar
#' @name complete_calendar
#' @param x ti object
#'
#' @returns dbi object
#' @export
S7::method(complete_calendar,ti) <- function(x){



x <- pmtd(sales,order_date,margin,"standard",1)

out <- x |>
  calculate() |>
  mutate(
    year_start=1
    ,year_end=1
    ,quarter_start=1
    ,quarter_end=1
    ,month_start=1
    ,month_end=1
    ,week_start=1
    ,week_end=1
    ,day_of_week=1
    ,days_in_year=1
    ,days_in_quarter=1
    ,days_in_month=1
    ,days_in_week=1
    ,days_remaining_in_year=1
    ,days_remaining_in_quarter=1
    ,days_remaining_in_month=1
    ,days_remaining_in_week=1
    ,days_passed_in_year=1
    ,days_passed_in_quarter=1
    ,days_passed_in_month=1
    ,days_passed_in_year=1
    ,weekend_indicator=1
  )
  return(out)

}


#' @title Print ti objects
#' @name print
#'
#' @param x ti object
#' @param ... additional arguments
#'
#' @returns ti object
#' @export
#'
#' @examples
#' x <- ytd(sales,.date=order_date,.value=quantity,calendar_type="standard")
#' x
S7::method(print,ti) <- function(x,...){


  ## subset function descriptions from table


  value_chr <- x@value@value_vec
  group_count <- x@calendar@group_count
  calendar_type <-   x@calendar@calendar_type



  ## start print message


  ### general information

  cli::cli_h1(x@fn@fn_long_name)
  cli::cli_text("Function: {.code { x@fn@fn_name}} was executed")
  cli::cli_h2("Description:")
  cli::cli_par()

  cli::cli_text(x@action@method)

  cli::builtin_theme()

  ### Calendar information


  cli::cli_h2("Calendar:")
  cli::cat_bullet(paste("The calendar aggregated",cli::col_br_magenta(x@calendar@date_vec),"to the",cli::col_yellow(x@time_unit@value),"time unit"))
  cli::cat_bullet("A ",cli::col_br_red(x@calendar@calendar_type)," calendar is created with ",cli::col_green(x@calendar@group_count," groups"))
  cli::cat_bullet(paste("Calendar ranges from",cli::col_br_green(x@calendar@min_date),"to",cli::col_br_green(x@calendar@max_date)))
  cli::cat_bullet(paste(cli::col_blue(x@calendar@date_missing),"days were missing and replaced with 0"))
  cli::cat_bullet("New date column ",stringr::str_flatten_comma(cli::col_br_red(x@fn@new_date_column_name),last = " and ")," was created from ",cli::col_br_magenta(x@calendar@date_vec))
  cli::cat_line("")

  ## Action information

  cli::cli_h2("Actions:")

  cli::cli_text(paste0(x@action@value[[1]]," ",cli::col_blue(x@value@value_vec)))

  cli::cli_text(paste0(x@action@value[[2]]," ",cli::col_green(na.omit(x@fn@lag_n))," ",cli::col_green(na.omit(x@fn@shift))))

  cli::cli_text(paste0(x@action@value[[3]]," ",cli::col_br_magenta(na.omit(x@fn@compare))))

  cli::cat_line("")
  ## print groups if groups exist

  if(x@calendar@group_indicator){

  cli::cli_text("{stringr::str_flatten_comma(x@calendar@group_vec,last = ' and ')} groups are in the table")
  cli::cat_line("")
  }

  ## Next Steps information

  cli::cli_h2("Next Steps:")

  cli::cli_li("Use {.code calculate()} to return the results")

  cli::cli_rule()

  cli::cli_end()


}

