

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
  summary_dbi <- x@data@data |>
    dplyr::ungroup() |>
    make_db_tbl() |>
    dplyr::mutate(
      date = lubridate::floor_date(!!x@data@date_quo,unit = !!x@time_unit@value)
      ,time_unit=!!x@time_unit@value
    ) |>
    dplyr::summarise(
      !!x@value@value_vec:= sum(!!x@value@value_quo,na.rm=TRUE)
      ,.by=c(date,!!!x@data@group_quo)
    )

  #create calendar table

  calendar_dbi <- fpaR::seq_date_sql(start_date = x@data@min_date,end_date = x@data@max_date,time_unit = x@time_unit@value,con=dbplyr::remote_con(x@data@data))


  # Expand calendar table with cross join of groups
  if(x@data@group_indicator){

    calendar_dbi <- calendar_dbi |>
      dplyr::cross_join(
        summary_dbi |>
          dplyr::distinct(!!!x@data@group_quo)
      )
      # dplyr::mutate(
      #   missing_date_indicator=dplyr::if_else(is.na(!!x@value@value_quo),1,0)
      #   ,!!x@value@value_vec:= dplyr::coalesce(!!x@value@value_quo, 0)
      # )

  }

  # Perform a full join to ensure all time frames are represented
  full_dbi <- dplyr::full_join(
    calendar_dbi
    ,summary_dbi
    ,by = dplyr::join_by(date,!!!x@data@group_quo)
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

  out <-   x@fn@fn_exec(x)|>
      dbplyr::window_order(date)

  return(out)

}


#' @title Calculate
#' @name calculate
#' @param x segment object
#'
#' @returns dbi object
#' @export
#' @examples
#' sales |>
#'     group_by(store_key) |>
#'     abc(.value = margin,category_values = c(.3,.5,.75,.85),type = "n") |>
#'     calculate() |>
S7::method(calculate,segment) <- function(x){


  out <- x@fn@fn_exec(x) |>
    arrange(row_id)

  return(out)

}



#' @title complete_calendar
#' @name complete_calendar
#' @param x ti object
#'
#' @returns dbi object
#' @export
S7::method(complete_calendar,ti) <- function(x){



# x <- pmtd(sales,order_date,margin,"standard",1)

out <- x |>
  calculate() |>
  dplyr::mutate(
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
  group_count <- x@data@group_count
  calendar_type <-   x@data@calendar_type



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
  cli::cat_bullet(paste("The calendar aggregated",cli::col_br_magenta(x@data@date_vec),"to the",cli::col_yellow(x@time_unit@value),"time unit"))
  cli::cat_bullet("A ",cli::col_br_red(x@data@calendar_type)," calendar is created with ",cli::col_green(x@data@group_count," groups"))
  cli::cat_bullet(paste("Calendar ranges from",cli::col_br_green(x@data@min_date),"to",cli::col_br_green(x@data@max_date)))
  cli::cat_bullet(paste(cli::col_blue(x@data@date_missing),"days were missing and replaced with 0"))
  cli::cat_bullet("New date column ",stringr::str_flatten_comma(cli::col_br_red(x@fn@new_date_column_name),last = " and ")," was created from ",cli::col_br_magenta(x@data@date_vec))
  cli::cat_line("")

  ## Action information

  print_actions_steps(x)


  cli::cat_line("")
  ## print groups if groups exist

  if(x@data@group_indicator){

  cli::cli_text("{stringr::str_flatten_comma(x@data@group_vec,last = ' and ')} groups are in the table")
  cli::cat_line("")
  }

  ## Next Steps information

fpaR::print_next_steps()


}


#' Print segment objects
#' @name print
#' @param x segment object
#' @param ...
#'
#' @returns segment object
#' @export
#'
S7::method(print,segment) <- function(x,...){


  n_values_len <- length(x@category_values)

  print_fn_info(x)

  ### Category Values information
  cli::cli_h2("Category Information")

  if(x@value@value_vec=="n"){

    cli::cat_bullet(

      paste(
        "The data set is summarized by"
        ,cli::col_br_magenta(stringr::str_flatten_comma(x@data@group_vec))
        ,"and then"
        ,cli::col_br_magenta("counts")
        ,"each group member's contribution of the total and then finally calculates the"
        ,cli::col_br_magenta("count")
        ,"of each groups rolling cumulative porportion of the total"
      )

    )

  }else{

    cli::cat_bullet(
      paste(
        "The data set is summarized by"
        ,cli::col_br_magenta(stringr::str_flatten_comma(x@data@group_vec))
        ,"and then sums each group member's"
        ,cli::col_br_magenta(x@value@value_vec)
        ,"contribution of the total"
        ,cli::col_br_magenta(x@value@value_vec)
        ,"and then finally calculates each groups rolling cumulative porportion of the total"
      )
    )

  }

  cli::cat_bullet(
    paste(
      "Then cumulative distribution was then arranged from lowest to highest and finally classified into"
      ,n_values_len
      ,"break points"
      ,cli::col_yellow(stringr::str_flatten_comma(scales::percent(x@category_values)))
      ," and labelled into the following categories"
      ,cli::col_br_blue(stringr::str_flatten_comma(x@category_names))
    )
  )
  cli::cat_line("")


  cli::cat_line("")

  print_actions_steps(x)

  print_next_steps()

}

