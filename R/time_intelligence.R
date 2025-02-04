#' Aggregate and expand date table
#'
#' @param .data data either grouped or ungrouped
#' @param time_unit the time unit to aggregate the date column by: 'day', 'week', 'month', 'quarter' or 'year'
#' @param date  the date column
#' @param value  the value column to aggregate
#' @seealso [fpaR::make_aggregation_dbi()] which is the DBI equivalent of this function
#' @description
#' `make_aggregation_tbl()` summarizes a tibble to target time unit and completes the calendar to ensure
#' no missing days, month, quarter or years. If a grouped tibble is passed through it will complete the calendar
#' for each combination of the group
#' @details
#' This is in internal function to make it easier to ensure data has no missing dates to
#'  simplify the use of time intelligence functions downstream of the application.
#' If you want to summarize to a particular group, simply pass the tibble through to the `group_by()` argument
#' prior to function and the function will make summarize and make a complete calendar for each group item.
#' @return tibble
#'
#' @examples
#' make_aggregation_tbl(fpaR::sales,date=date,value=quantity,time_unit="day")
make_aggregation_tbl <- function(.data,date,value,time_unit) {

  assertthat::assert_that(base::is.data.frame(.data), msg = "Data must be a data frame.")
  assertthat::assert_that(base::is.character(time_unit), msg = "Time unit must be a character string.")
  assertthat::assert_that(time_unit %in% base::c("day", "week","quarter","semester","month", "year"), msg = "Time frame must be one of 'day', 'week','semester', 'month', or 'year'.")
  assertthat::assert_that(lubridate::is.Date(.data |> pull({{date}})), msg = "The date column is not in Date format.")
  #
  # # # Check if the column follows the yyyy-mm-dd format
  # formatted_dates <- format(date, "%Y-%m-%d")
  # assertthat::assert_that(base::all(date == base::as.Date(formatted_dates)), msg = "The date column does not follow the yyyy-mm-dd format.")
  #

  existing_groups <- dplyr::groups(.data) |>
    map(\(x) rlang::as_label(x)) |> purrr::simplify()

  # Floor the date to the specified time frame
  summary_tbl <- .data |>
    dplyr::mutate(
      date = lubridate::floor_date({{date}},time_unit)
      ,time_unit=time_unit
    ) |>
    dplyr::group_by(date,!!!existing_groups) |>
    dplyr::summarise(
      "{{value}}":= sum({{value}},na.rm=TRUE)
      ,.groups = "drop"
    )


  # Create a calendar table with all the dates in the specified time frame
  calendar_tbl <- tibble::tibble(
    date = base::seq.Date(from = base::min(summary_tbl$date,na.rm=TRUE), to = base::max(summary_tbl$date,na.rm = TRUE), by = time_unit)
  )

  # create crossing table of groups

  if(!existing_groups|> is_empty()){

    calendar_tbl <- dplyr::left_join(
      summary_tbl |> dplyr::distinct(pick(existing_groups)) |> dplyr::mutate(id="id")
      ,calendar_tbl |> dplyr::mutate(id="id")
      ,by=dplyr::join_by(id)
      ,relationship = "many-to-many"
    ) |>
      dplyr::select(-id)

  }

  # Perform a full join to ensure all time frames are represented
  full_tbl <- dplyr::full_join(
    calendar_tbl
    ,summary_tbl
    ,by = dplyr::join_by(date,!!!existing_groups)
  ) |>
    dplyr::mutate(
      # dplyr::across(dplyr::where(\(x) base::is.numeric(x)),\(x) tidyr::replace_na(x,0))
      "{{value}}":= dplyr::coalesce({{value}}, 0)
    )



  return(full_tbl)

}

# Formulas to be put inside the ti_tbl class----------------------

## year related functions

#' Year-to-date for tibble objects
#'
#' @param x ti_tbl
#'
#' @returns tibble
#'
ytd_tbl <- function(x){


  # create calendar table

  full_tbl <- create_calendar(x) |>
    dplyr::mutate(
      year=lubridate::year(date)
      ,.before = 1
    )
  # aggregate the data and create the cumulative sum

  out_tbl <- full_tbl |>
    dplyr::group_by(year,!!!x@calendar_tbl@group_quo) |>
    dplyr::arrange(date,.by_group = TRUE) |>
    dplyr::mutate(
      !!x@new_column_name:=base::cumsum(!!x@value_quo)
    ) |>
    dplyr::ungroup()

  return(out_tbl)

}


#' Previous year-to-date for tibble objects
#'
#' @param x ti_tbl
#'
#' @returns tibble
#'
pytd_tbl <- function(x){


  full_tbl <-  create_calendar(x) |>
    dplyr::mutate(
      year=lubridate::year(date)
      ,.before = 1
    )

  lag_tbl <- full_tbl|>
    dplyr::group_by(year,!!!x@calendar_tbl@group_quo) |>
    dplyr::arrange(date,.by_group = TRUE) |>
    dplyr::mutate(
      ,date_lag=date +years(1)
      ,!!x@new_column_name:=cumsum(!!x@value_quo)
    ) |>
    ungroup() |>
    dplyr::select(-c(date,year,!!x@value_quo))

  out_tbl <-   dplyr::left_join(
    full_tbl
    ,lag_tbl
    ,by=dplyr::join_by(date==date_lag,!!!x@calendar_tbl@group_quo)
  ) |>
    select(-c(!!x@value_quo))

  return(out_tbl)
}



#' Previous year-to-date for tibble objects
#'
#' @param x ti_tbl
#'
#' @returns tibble
#'
yoytd_tbl <- function(x){


 full_tbl <-  create_calendar(x) |>
    mutate(
      year=lubridate::year(date)
      ,.before = 1
    ) |>
   group_by(year,!!!x@calendar_tbl@group_quo) |>
   mutate(
     !!paste0("ytd_",x@value_quo):=cumsum(!!x@value_quo)
   )

  lag_tbl <- full_tbl|>
    group_by(year,!!!x@calendar_tbl@group_quo) |>
    arrange(date,.by_group = TRUE) |>
    dplyr::mutate(
      ,date_lag=date +years(1)
      ,!!x@new_column_name:=cumsum(!!x@value_quo)
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-c(date,year,!!x@value_quo,!!paste0("ytd_",x@value_quo)))

  out_tbl <-   dplyr::left_join(
    full_tbl
    ,lag_tbl
    ,by=dplyr::join_by(date==date_lag,!!!x@calendar_tbl@group_quo)
  ) |>
    select(-c(!!x@value_quo))

  return(out_tbl)

}

## quarter related functions -----------------

#' Quarter-to-date for tibble objects
#'
#' @param x ti_tbl
#'
#' @returns tibble
#'
qtd_tbl <- function(x){


  full_tbl <-  create_calendar(x) |>
    dplyr::mutate(
      year=lubridate::year(date)
      ,quarter=lubridate::quarter(date)
      ,.before = 1
    )



  out_tbl <- full_tbl |>
    dplyr::group_by(year,quarter,!!!x@calendar_tbl@group_quo) |>
    dplyr::arrange(date,.by_group = TRUE) |>
    dplyr::mutate(
      !!x@new_column_name:=base::cumsum(!!x@value_quo)
    ) |>
    dplyr::ungroup()

  return(out_tbl)

}

#' Previous quarter-to-date for tibble objects
#'
#' @param x ti_tbl
#'
#' @returns tibble
#'
pqtd_tbl <- function(x){


  full_tbl <-  create_calendar(x) |>
    dplyr::mutate(
      year=lubridate::year(date)
      ,quarter=lubridate::quarter(date)
      ,.before = 1
    )

  lag_tbl <- full_tbl|>
    dplyr::group_by(year,quarter,!!!x@calendar_tbl@group_quo) |>
    dplyr::arrange(date,.by_group = TRUE) |>
    dplyr::mutate(
      ,date_lag=date +quarters(1)
      ,!!x@new_column_name:=cumsum(!!x@value_quo)
    ) |>
    ungroup() |>
    dplyr::select(-c(date,quarter,year,!!x@value_quo))

  out_tbl <-   dplyr::left_join(
    full_tbl
    ,lag_tbl
    ,by=dplyr::join_by(date==date_lag,!!!x@calendar_tbl@group_quo)
  ) |>
    select(-c(!!x@value_quo))

  return(out_tbl)
}
#' Previous quarter-to-date for tibble objects
#'
#' @param x ti_tbl
#'
#' @returns tibble
#'
qoqtd_tbl <- function(x){


  full_tbl <-  create_calendar(x) |>
    mutate(
      year=lubridate::year(date)
      ,quarter=lubridate::quarter(date)
      ,.before = 1
    ) |>
    group_by(year,quarter,!!!x@calendar_tbl@group_quo) |>
    mutate(
      !!paste0("qtd_",x@value_quo):=cumsum(!!x@value_quo)
    )

  lag_tbl <- full_tbl|>
    dplyr::group_by(year,quarter,!!!x@calendar_tbl@group_quo) |>
    dplyr::arrange(date,.by_group = TRUE) |>
    dplyr::mutate(
      ,date_lag=date +quarters(1)
      ,!!x@new_column_name:=cumsum(!!x@value_quo)
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-c(date,quarter,year,!!x@value_quo,!!paste0("qtd_",x@value_quo)))

  out_tbl <-   dplyr::left_join(
    full_tbl
    ,lag_tbl
    ,by=dplyr::join_by(date==date_lag,!!!x@calendar_tbl@group_quo)
  ) |>
    dplyr::select(-c(!!x@value_quo))

  return(out_tbl)

}


## month related functions -------------------------

#' Month-to-date for tibble objects
#'
#' @param x ti_tbl
#'
#' @returns tibble
#'
mtd_tbl <- function(x){


  full_tbl <-  create_calendar(x) |>
    dplyr::mutate(
      year=lubridate::year(date)
      ,month=lubridate::month(date)
      ,.before = 1
    )



  out_tbl <- full_tbl |>
    dplyr::group_by(year,month,!!!x@calendar_tbl@group_quo) |>
    dplyr::arrange(date,.by_group = TRUE) |>
    dplyr::mutate(
      !!x@new_column_name:=base::cumsum(!!x@value_quo)
    ) |>
    dplyr::ungroup()

  return(out_tbl)





}

## week related functions-----------------

#' Week-to-date for tibble objects
#'
#' @param x ti_tbl
#'
#' @returns tibble
#'
wtd_tbl <- function(x){

  full_tbl <-  create_calendar(x) |>
    dplyr::mutate(
      year=lubridate::year(date)
      ,month=lubridate::month(date)
      ,week=lubridate::week(date)
      ,.before = 1
    )



  out_tbl <- full_tbl |>
    dplyr::group_by(year,month,week,!!!x@calendar_tbl@group_quo) |>
    dplyr::arrange(date,.by_group = TRUE) |>
    dplyr::mutate(
      !!x@new_column_name:=base::cumsum(!!x@value_quo)
    ) |>
    dplyr::ungroup()

  return(out_tbl)
}

## all to date related functions ----------------

#' All-to-date for tibble objects
#'
#' @param x ti_tbl
#'
#' @returns tibble
atd_tbl <- function(x){

  full_tbl <-  create_calendar(x)

  out_tbl <- full_tbl |>
    dplyr::group_by(!!!x@calendar_tbl@group_quo) |>
    dplyr::arrange(date,.by_group = TRUE) |>
    dplyr::mutate(
      !!x@new_column_name:=base::cumsum(!!x@value_quo)
    ) |>
    dplyr::ungroup()

  return(out_tbl)




}


## day related functions --------------------------

#' Day-over-day for tibble objects
#'
#' @param x ti_tbl
#'
#' @returns tibble
#'
dod_tbl <- function(x){

  full_tbl <-  create_calendar(x)


  lag_tbl <- full_tbl|>
    arrange(date,.by_group = TRUE) |>
    dplyr::mutate(
      date_lag=date %m+% lubridate::days(x@lag_n)
      ,!!x@new_column_name:=!!x@value_quo
    ) |>
    dplyr::select(-c(date,!!x@value_quo)) |>
    dplyr::ungroup()

  out_tbl <-   dplyr::left_join(
    full_tbl
    ,lag_tbl
    ,by=dplyr::join_by(date==date_lag,!!!x@calendar_tbl@group_quo)
  )
  # mutate(
  # !!x@new_column_name:= dplyr::coalesce(.data[[rlang::englue(x@new_column_name)]],0)
  # )

  return(out_tbl)
}

## comparing related functions ------------------


#' Week-over-week for tibble objects
#'
#' @param x ti_tbl
#'
#' @returns tibble
#'
wow_tbl <- function(x){


  full_tbl <-  create_calendar(x)


  lag_tbl <- full_tbl|>
    arrange(date,.by_group = TRUE) |>
    dplyr::mutate(
      date_lag=dplyr::lead(date,n = x@lag_n)
      ,!!x@new_column_name:=!!x@value_quo
    ) |>
    dplyr::select(-c(date,!!x@value_quo)) |>
    dplyr::ungroup()

  out_tbl <-   dplyr::left_join(
    full_tbl
    ,lag_tbl
    ,by=dplyr::join_by(date==date_lag,!!!x@calendar_tbl@group_quo)
  )


  return(out_tbl)

}



#' Month-over-month for tibble objectsj
#'
#' @param x ti_tbl
#'
#' @returns tibble
#'
mom_tbl <- function(x){



  lag_tbl <- full_tbl|>
    dplyr::arrange(date,.by_group = TRUE) |>
    dplyr::mutate(
      date_lag=dplyr::lead(date,n = x@lag_n)
      ,!!x@new_column_name:=!!x@value_quo
    ) |>
    dplyr::select(-c(date,!!x@value_quo)) |>
    dplyr::ungroup()

  out_tbl <-   dplyr::left_join(
    full_tbl
    ,lag_tbl
    ,by=dplyr::join_by(date==date_lag,!!!x@calendar_tbl@group_quo)
  )



}


#' Year-over-year
#'
#' @param x ti_tbl
#'
#' @returns tibble
#'
yoy_tbl <- function(x){


  full_tbl <-  create_calendar(x)


  lag_tbl <- full_tbl|>
    arrange(date,.by_group = TRUE) |>
    dplyr::mutate(
      date_lag=dplyr::lead(date,n = x@lag_n)
      ,!!x@new_column_name:=!!x@value_quo
    ) |>
    dplyr::select(-c(date,!!x@value_quo)) |>
    dplyr::ungroup()

  out_tbl <-   dplyr::left_join(
    full_tbl
    ,lag_tbl
    ,by=dplyr::join_by(date==date_lag,!!!x@calendar_tbl@group_quo)
  )

  return(out_tbl)
}



# functions that assign arguments to the ti_tbl class --------------------


## year related ti_tbl----------

#' Year-to-date
#' @param .data either a tibble or  DBI object
#' @param date the date column to aggregate
#' @param value the value column to summarize
#' @param calendar_type either 'standard' or '5-5-4' calendar
#' @description
#' This calculates the annual cumulative sum of targeted value using a standard or 5-5-4 calendar respecting
#' any groups that are passed through with `dplyr::group_by()`.
#'
#' Use `calculate()` to return the results
#'
#' @returns ytd_tbl or ytd_dbi
#' @export
#'
#' @examples
#' ytd(fpaR::sales,date=date,value=quantity,calendar_type="standard")
ytd <- function(.data,date,value,calendar_type){

  # Validate inputs
  assertthat::assert_that(base::is.data.frame(.data), msg = "data must be a data frame")


  # assigns inputs to ytd_tbl class

  out <- ?ti_tbl(
    calendar_tbl(
      data=.data
      ,calendar_type=calendar_type
      ,date_vec = rlang::as_label(rlang::enquo(date))
    )
    ,time_unit = time_unit("day")
    ,action=action("aggregate")
    ,value_vec = rlang::as_label(rlang::enquo(value))
    ,new_column_name_prefix = "ytd"
    ,sort_logic = TRUE
    ,fn=ytd_tbl
    ,new_date_column_name = "Year"
    ,second_column_name_prefix = NA_character_
    ,second_column_name=NA_character_
    ,lag_n = NA_integer_
  )
  return(out)

}


#' Prevoius year-to-date
#' @param .data either a tibble or  DBI object
#' @param date the date column to aggregate
#' @param value the value column to summarize
#' @param calendar_type either 'standard' or '5-5-4' calendar
#' @description
#' This calculates the annual cumulative sum of targeted value using a standard or 5-5-4 calendar respecting
#' any groups that are passed through with `dplyr::group_by()`.
#'
#' Use `calculate()` to return the results
#'
#' @returns ytd_tbl or ytd_dbi
#' @export
#'
#' @examples
#' ytd(fpaR::sales,date=date,value=quantity,calendar_type="standard")
pytd <- function(.data,date,value,calendar_type,lag_n){

  # Validate inputs
  assertthat::assert_that(base::is.data.frame(.data), msg = "data must be a data frame")


  # assigns inputs to ytd_tbl class

  out <- ti_tbl(
    calendar_tbl(
      data=.data
      ,calendar_type=calendar_type
      ,date_vec = rlang::as_label(rlang::enquo(date))
    )
    ,time_unit = time_unit("day")
    ,action=action("aggregate")
    ,value_vec = rlang::as_label(rlang::enquo(value))
    ,new_column_name_prefix = "pytd"
    ,sort_logic = TRUE
    ,fn=pytd_tbl
    ,lag_n = lag_n
    ,new_date_column_name = "Year"
    ,second_column_name_prefix = NA_character_
    ,second_column_name=NA_character_
    ,lag_n = NA_integer_
  )

  return(out)
}

#' Prevoius year-to-date
#' @param .data either a tibble or  DBI object
#' @param date the date column to aggregate
#' @param value the value column to summarize
#' @param calendar_type either 'standard' or '5-5-4' calendar
#' @description
#' This calculates the annual cumulative sum of targeted value using a standard or 5-5-4 calendar respecting
#' any groups that are passed through with `dplyr::group_by()`.
#'
#' Use `calculate()` to return the results
#'
#' @returns ytd_tbl or ytd_dbi
#' @export
#'
#' @examples
#' ytd(fpaR::sales,date=date,value=quantity,calendar_type="standard")
yoytd <- function(.data,date,value,calendar_type,lag_n){

  # Validate inputs
  assertthat::assert_that(base::is.data.frame(.data), msg = "data must be a data frame")


  # assigns inputs to ytd_tbl class

  out <- ti_tbl(
    calendar_tbl(
      data=.data
      ,calendar_type=calendar_type
      ,date_vec = rlang::as_label(rlang::enquo(date))
    )
    ,time_unit = time_unit("day")
    ,action=action("aggregate")
    ,value_vec = rlang::as_label(rlang::enquo(value))
    ,new_column_name_prefix = "pytd"
    ,sort_logic = TRUE
    ,fn=yoytd_tbl
    ,lag_n = lag_n
    ,new_date_column_name = "Year"
    ,second_column_name_prefix = NA_character_
    ,second_column_name=NA_character_
    ,lag_n = NA_integer_
  )

  return(out)
}

## quarter related ti_tbl-----------------------------

#' Quarter-to-date
#'
#' @param .data either a tibble or  DBI object
#' @param date the date column to aggregate
#' @param value the value column to summarize
#' @param calendar_type either 'standard' or '5-5-4' calendar
#' @description
#' This calculates the quarterly cumulative sum of targeted value using a standard or 5-5-4 calendar respecting
#' any groups that are passed through with `dplyr::group_by()`.
#'
#' Use `calculate()` to return the results
#'
#' @returns qtd_tbl or qtd_dbi object depending on what is passed through
#' @export
#'
#' @examples
#' qtd(fpaR::sales,date=date,value=quantity,calendar_type="standard")
qtd <- function(.data,date,value,calendar_type){

  # Validate inputs
  assertthat::assert_that(base::is.data.frame(.data), msg = "data must be a data frame")

  # Aggregate data based on provided time unit


  out <- ti_tbl(
    calendar_tbl(
      data                   = .data
      ,calendar_type         = calendar_type
      ,date_vec              =  rlang::as_label(rlang::enquo(date))
    )
    ,time_unit               = time_unit("day")
    ,action                  = action("aggregate")
    ,value_vec               = rlang::as_label(rlang::enquo(value))
    ,new_column_name_prefix  = "qtd"
    ,sort_logic              = TRUE
    ,fn                      = qtd_tbl
    ,new_date_column_name    = c("Year","Quarter")
    ,second_column_name_prefix = NA_character_
    ,second_column_name=NA_character_
    ,lag_n = NA_integer_
  )

  return(out)

}



#' Previous quarter quarter-to-date
#' @param .data either a tibble or  DBI object
#' @param date the date column to aggregate
#' @param value the value column to summarize
#' @param calendar_type either 'standard' or '5-5-4' calendar
#' @description
#' This calculates the annual cumulative sum of targeted value using a standard or 5-5-4 calendar respecting
#' any groups that are passed through with `dplyr::group_by()`.
#'
#' Use `calculate()` to return the results
#'
#' @returns ytd_tbl or ytd_dbi
#' @export
#'
#' @examples
#' ytd(fpaR::sales,date=date,value=quantity,calendar_type="standard")
pqtd <- function(.data,date,value,calendar_type,lag_n){

  # Validate inputs
  assertthat::assert_that(base::is.data.frame(.data), msg = "data must be a data frame")


  # assigns inputs to ytd_tbl class

  out <- ti_tbl(
    calendar_tbl(
      data=.data
      ,calendar_type=calendar_type
      ,date_vec = rlang::as_label(rlang::enquo(date))
    )
    ,time_unit = time_unit("day")
    ,action=action("aggregate")
    ,value_vec = rlang::as_label(rlang::enquo(value))
    ,new_column_name_prefix = "pqtd"
    ,sort_logic = TRUE
    ,fn=qytd_tbl
    ,lag_n = lag_n
    ,new_date_column_name    = c("Year","Quarter")
    ,second_column_name_prefix = NA_character_
    ,second_column_name=NA_character_
    ,lag_n = NA_integer_
  )

  return(out)
}

## month related ti_tbl-------------------

#' Month-to-date
#'
#' @param .data either a tibble or  DBI object
#' @param date the date column to aggregate
#' @param value the value column to summarize
#' @param calendar_type either 'standard' or '5-5-4' calendar
#' @description
#' This calculates the monthly cumulative sum of targeted value using a standard or 5-5-4 calendar respecting
#' any groups that are passed through with `dplyr::group_by()`.
#'
#' Use `calculate()` to return the results
#'
#' @returns mtd_tbl or mtd_dbi object depending on what is passed through
#' @export
#'
#' @examples
#' mtd(fpaR::sales,date=date,value=quantity,calendar_type="standard")
mtd <- function(.data,date,value,calendar_type){

    # Validate inputs
    assertthat::assert_that(base::is.data.frame(.data), msg = "data must be a data frame")



    out <- ti_tbl(
      calendar_tbl(
        data=.data
        ,calendar_type=calendar_type
        ,date_vec = rlang::as_label(rlang::enquo(date))
      )
      ,time_unit = time_unit("day")
      ,action=action("aggregate")
      ,value_vec = rlang::as_label(rlang::enquo(value))
      ,new_column_name_prefix = "mtd"
      ,sort_logic = TRUE
      ,fn=mtd_tbl
      ,new_date_column_name = c("year","month")
      ,second_column_name_prefix = NA_character_
      ,second_column_name=NA_character_
      ,lag_n = NA_integer_
    )


  return(out)
}

## week related ti_tbl-------------


#' Week-to-date
#'
#' @param .data either a tibble or  DBI object
#' @param date the date column to aggregate
#' @param value the value column to summarize
#' @param calendar_type either 'standard' or '5-5-4' calendar
#' @description
#' This calculates the weekly cumulative sum of targeted value using a standard or 5-5-4 calendar respecting
#' any groups that are passed through with `dplyr::group_by()`.
#'
#' Use `calculate()` to return the results
#'
#' @returns wtd_tbl or wtd_dbi
#' @export
#'
#' @examples
#' wtd(fpaR::sales,date=date,value=quantity,calendar_type="standard")
wtd <- function(.data,date,value,calendar_type){

  # Validate inputs
  assertthat::assert_that(base::is.data.frame(.data), msg = "data must be a data frame")



  out <- ti_tbl(
    calendar_tbl(
      data=.data
      ,calendar_type=calendar_type
      ,date_vec = rlang::as_label(rlang::enquo(date))
    )
    ,time_unit = time_unit("day")
    ,action=action("aggregate")
    ,value_vec = rlang::as_label(rlang::enquo(value))
    ,new_column_name_prefix = "wtd"
    ,sort_logic = TRUE
    ,fn=wtd_tbl
    ,new_date_column_name = c("year","month","week")
    ,second_column_name_prefix = NA_character_
    ,second_column_name=NA_character_
    ,lag_n = NA_integer_
  )


  return(out)
}

## all related ti_tbl-------------------------

#' All-to-date
#'
#' @param .data either a tibble or  DBI object
#' @param date the date column to aggregate
#' @param value the value column to summarize
#' @param calendar_type either 'standard' or '5-5-4' calendar
#' @description
#' This calculates the total cumulative sum of targeted value using a standard or 5-5-4 calendar respecting
#' any groups that are passed through with `dplyr::group_by()`.
#'
#' Use `calculate()` to return the results
#'
#' @returns atd_tbl or atd_dbi object depending on what is passed through
#' @export
#'
#' @examples
#' atd(fpaR::sales,date=date,value=quantity,calendar_type="standard")
atd <- function(.data,date,value,calendar_type){

  # Validate inputs
  assertthat::assert_that(base::is.data.frame(.data), msg = "data must be a data frame")



  out <- ti_tbl(
    calendar_tbl(
      data=.data
      ,calendar_type=calendar_type
      ,date_vec = rlang::as_label(rlang::enquo(date))
    )
    ,time_unit = time_unit("day")
    ,action=action("aggregate")
    ,value_vec = rlang::as_label(rlang::enquo(value))
    ,new_column_name_prefix = "atd"
    ,sort_logic = TRUE
    ,fn=atd_tbl
    ,new_date_column_name = c("date")
    ,second_column_name_prefix = NA_character_
    ,second_column_name=NA_character_
    ,lag_n = NA_integer_
  )

  return(out)

}

## comparison ti_tbl------------------


#' day over day calculation
#'
#' @param .data tibble or DBI object
#' @param date the date column to aggregate
#' @param value the value column to summarize
#' @param calendar_type either 'standard' or '5-5-4' calendar
#' @param lag_n the number of periods to lag, default is 1
#'
#' @returns dod_tbl or dbi_tbl
#' @export
#'
#' @examples
#' dod(fpaR::sales,date=order_date,value=quantity,calendar_type='standard',lag_n=1)
dod <- function(.data,date,value,calendar_type,lag_n=1){


  out <- ti_tbl(
    calendar_tbl(
      data=.data
      ,calendar_type=calendar_type
      ,date_vec = rlang::as_label(rlang::enquo(date))
    )
    ,time_unit = time_unit("day")
    ,action=action(c("aggregate","shift","compare"))
    ,value_vec = rlang::as_label(rlang::enquo(value))
    ,new_column_name_prefix = "dod"
    ,sort_logic = TRUE
    ,fn=dod_tbl
    ,new_date_column_name = NA_character_
    ,second_column_name_prefix = NA_character_
    ,second_column_name=NA_character_
    ,lag_n = NA_integer_

  )

  return(out)


}

#' Week-over-Week
#'
#' @param .data tibble or DBI object
#' @param date the date column to aggregate
#' @param value the value column to summarize
#' @param calendar_type either 'standard' or '5-5-4' calendar
#' @param lag_n the number of weeks to lag, default is 1
#'
#' @returns wow_tbl or wow_dbi
#' @export
#'
#' @examples
#' wow(fpaR::sales,date=order_date,value=quantity,calendar_type='standard',lag_n=1)
wow <- function(.data,date,value,calendar_type,lag_n=1){


  out <- ti_tbl(
    calendar_tbl(
      data=.data
      ,calendar_type=calendar_type
      ,date_vec = rlang::as_label(rlang::enquo(date))
    )
    ,time_unit = time_unit("week")
    ,action=action(c("aggregate","shift","compare"))
    ,value_vec = rlang::as_label(rlang::enquo(value))
    ,new_column_name_prefix = "wow"
    ,sort_logic = TRUE
    ,fn=wow_tbl
    ,new_date_column_name = NA_character_
    ,lag_n=lag_n
  )
  return(out)
}





#' Month-over-month
#'
#' @param .data tibble or DBI object
#' @param date the date column to aggregate
#' @param value the value column to summarize
#' @param calendar_type either 'standard' or '5-5-4' calendar
#' @param lag_n the number of months to lag, default is 1
#'
#' @returns mom_tbl or mom_dbi
#' @export
#'
#' @examples
#' mom(fpaR::sales,date=order_date,value=quantity,calendar_type='standard',lag_n=1)
mom <- function(.data,date,value,calendar_type,lag_n=1){


  out <- ti_tbl(
    calendar_tbl(
      data=.data
      ,calendar_type=calendar_type
      ,date_vec = rlang::as_label(rlang::enquo(date))
    )
    ,time_unit = time_unit("month")
    ,action=action(c("aggregate","shift","compare"))
    ,value_vec = rlang::as_label(rlang::enquo(value))
    ,new_column_name_prefix = "mom"
    ,sort_logic = TRUE
    ,fn=mom_tbl
    ,new_date_column_name = NA_character_
    ,second_column_name_prefix = NA_character_
    ,second_column_name=NA_character_
    ,lag_n=lag_n
  )
  return(out)
}


#' Year-over-year
#'
#' @param .data tibble or DBI object
#' @param date the date column to aggregate
#' @param value the value column to summarize
#' @param calendar_type either 'standard' or '5-5-4' calendar
#' @param lag_n the number of months to lag, default is 1
#'
#' @returns mom_tbl or mom_dbi
#' @export
#'
#' @examples
#' yoy(fpaR::sales,date=order_date,value=quantity,calendar_type='standard',lag_n=1)
yoy <- function(.data,date,value,calendar_type,lag_n=1){

  out <- ti_tbl(
    calendar_tbl(
      data=.data
      ,calendar_type=calendar_type
      ,date_vec = rlang::as_label(rlang::enquo(date))
    )
    ,time_unit = time_unit("year")
    ,action=action(c("aggregate","shift","compare"))
    ,value_vec = rlang::as_label(rlang::enquo(value))
    ,new_column_name_prefix = "yoy"
    ,sort_logic = TRUE
    ,fn=yoy_tbl
    ,new_date_column_name = NA_character_
    ,second_column_name_prefix = NA_character_
    ,second_column_name=NA_character_
    ,lag_n=lag_n
  )
  return(out)
}


