
## year related functions

#' Year-to-date for tibble objects
#'
#' @param x ti_tbl
#' @seealso [fpaR::pytd()fpaR::yoy()fpaR::yoytd()] are the year based ti functions
#' @description
#' `ytd_fn()` summarizes a tibble to target time unit and completes the calendar to ensure
#' no missing days, month, quarter or years. If a grouped tibble is passed through it will complete the calendar
#' for each combination of the group
#' @details
#' This is in internal function to make it easier to ensure data has no missing dates to
#'  simplify the use of time intelligence functions downstream of the application.
#' If you want to summarize to a particular group, simply pass the tibble through to the `group_by()` argument
#' prior to function and the function will make summarize and make a complete calendar for each group item.
#' @returns dbi
#'
ytd_fn <- function(x){

  # create calendar table

  full_dbi <- create_calendar(x) |>
    dplyr::mutate(
      year=lubridate::year(date)
    )


 # aggregate the data and create the cumulative sum

  out_dbi <- full_dbi |>
    dbplyr::window_order(date,!!!x@calendar@group_quo) |>
    dplyr::mutate(
      !!x@value@new_column_name[[1]]:=base::cumsum(!!x@value@value_quo)
      ,.by=c(year,!!!x@calendar@group_quo)
    ) |>
    dplyr::relocate(
      date,year
    )

  return(out_dbi)
}


#' Previous year-to-date for tibble objects
#'
#' @param x ti_tbl
#' @seealso [fpaR::pytd()fpaR::yoy()fpaR::yoytd()]
#' @description
#' `pytd_fn()` summarizes a tibble to target time unit and completes the calendar to ensure
#' no missing days, month, quarter or years. If a grouped tibble is passed through it will complete the calendar
#' for each combination of the group
#' @details
#' This is in internal function to make it easier to ensure data has no missing dates to
#'  simplify the use of time intelligence functions downstream of the application.
#' If you want to summarize to a particular group, simply pass the tibble through to the `group_by()` argument
#' prior to function and the function will make summarize and make a complete calendar for each group item.
#' @returns dbi
#'
#' @returns tibble
#'
pytd_fn <- function(x){


  # create calendar table
  full_dbi <-  create_calendar(x) |>
    dplyr::mutate(
      year=lubridate::year(date)
      ,.before = 1
    )

  # create lag table
  lag_dbi <- full_dbi|>
    dbplyr::window_order(date,!!!x@calendar@group_quo) |>
    dplyr::mutate(
      date_lag=as.Date(date+lubridate::years(!!x@fn@lag_n))
      ,!!x@value@new_column_name[[1]]:=cumsum(!!x@value@value_quo)
      # ,.by=c(year,!!!x@calendar@group_quo)
    ) |>
    dplyr::select(-c(date,year,!!x@value@value_quo))


  # join tables together
  out_tbl <-   dplyr::left_join(
    full_dbi
    ,lag_dbi
    ,by=dplyr::join_by(date==date_lag,!!!x@calendar@group_quo)
  ) |>
    dplyr::select(-c(!!x@value@value_quo))

  return(out_tbl)

}


#' Current year to date over previous year-to-date for tibble objects
#'
#' @param x ti_tbl
#'
#' @returns tibble
#'
yoytd_fn <- function(x){

  # ytd table
  x@value@new_column_name <- "ytd"
  ytd_dbi <- ytd_fn(x)

  #pytd table
  x@value@new_column_name <- "pytd"

  pytd_dbi <- pytd_fn(x)

  # join tables together

  out_dbi <-   dplyr::left_join(
    ytd_dbi
    ,pytd_dbi
    ,by=dplyr::join_by(date==date,year,!!!x@calendar@group_quo)
  )

  return(out_dbi)

}


#' Year-over-year
#'
#' @param x ti_tbl
#'
#' @returns tibble
#'
yoy_fn <- function(x){

  # create calendar
  full_dbi <-  create_calendar(x)

  # create lag
  lag_dbi <- full_dbi |>
    dbplyr::window_order(date,!!!x@calendar@group_quo) |>
    dplyr::mutate(
      date_lag=dplyr::lead(date,n = !!x@fn@lag_n)
      ,!!x@value@new_column_name[[1]]:=!!x@value@value_quo
      ,.by=c(!!!x@calendar@group_quo)
    ) |>
    dplyr::select(-c(date,!!x@value@value_quo))

  # bring tables together
  out_dbi <-   dplyr::left_join(
    full_dbi
    ,lag_dbi
    ,by=dplyr::join_by(date==date_lag,!!!x@calendar@group_quo)
  ) |>
    mutate(
      year=lubridate::year(date)
    )

  return(out_dbi)

}

#' Year-to-date vs. full prior year
#'
#' @param x ti_tbl
#'
#' @returns tibble
#'
ytdopy_fn <- function(x){

  # year-to-date table
   x@value@new_column_name <- "ytd"
  ytd_dbi <-  ytd_fn(x)

  #aggregate to prior year

  x@value@new_column_name <- "py"
  x@time_unit <- time_unit("year")

  py_dbi <-   yoy_fn(x)

  # join together

 out_dbi <-  ytd_dbi |>
   select(
     -c(!!x@value@value_quo)
   ) |>
    dplyr::left_join(
      py_dbi |> select(
        -c(!!x@value@value_quo)
      )
      ,by=dplyr::join_by(year,date,!!!x@calendar@group_quo)
    )

  return(out_tbl)
}


## quarter related functions -----------------

#' Quarter-to-date for tibble objects
#'
#' @param x ti_tbl
#'
#' @returns tibble
#'
qtd_fn <- function(x){


  full_dbi <-  create_calendar(x) |>
    dplyr::mutate(
      year=lubridate::year(date)
      ,quarter=lubridate::quarter(date)
      ,.before = 1
    )

  out_dbi <- full_dbi |>
    dbplyr::window_order(date,!!!x@calendar@group_quo) |>
    dplyr::mutate(
      !!x@value@new_column_name:=base::cumsum(!!x@value@value_quo)
      ,.by=c(year,quarter,!!!x@calendar@group_quo)
    ) |>
    dplyr::ungroup()

  return(out_dbi)

}

#' Previous quarter-to-date for tibble objects
#'
#' @param x ti_tbl
#'
#' @returns tibble
#'
pqtd_fn <- function(x){


  full_tbl <-  create_calendar(x) |>
    dplyr::mutate(
      year=lubridate::year(date)
      ,quarter=lubridate::quarter(date)
      ,.before = 1
    )

  lag_tbl <- full_tbl|>
    dbplyr::window_order(date) |>
    dplyr::mutate(
      ,date_lag=date +lubridate::quarters(x@fn@lag_n)
      ,!!x@nvalue@ew_column_name:=cumsum(!!x@value@value_quo)
      ,.by=c(year,quarter,!!!x@calendar@group_quo)
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-c(date,quarter,year,!!x@value@value_quo))


  out_tbl <-   dplyr::left_join(
    full_tbl
    ,lag_tbl
    ,by=dplyr::join_by(date==date_lag,!!!x@calendar@group_quo)
  ) |>
    dplyr::select(-c(!!x@value_quo))

  return(out_tbl)
}


#' Title
#'
#' @param x
#'
#' @returns
#' @export
#'
#' @examples
qoq_fn <- function(x){

  # create calendar
  full_dbi <-  create_calendar(x)

  # create lag
  lag_dbi <- full_dbi |>
    dbplyr::window_order(date,!!!x@calendar@group_quo) |>
    dplyr::mutate(
      date_lag=dplyr::lead(date,n = !!x@fn@lag_n)
      ,!!x@value@new_column_name[[1]]:=!!x@value@value_quo
      ,.by=c(!!!x@calendar@group_quo)
    ) |>
    dplyr::select(-c(date,!!x@value@value_quo))

  # bring tables together
  out_dbi <-   dplyr::left_join(
    full_dbi
    ,lag_dbi
    ,by=dplyr::join_by(date==date_lag,!!!x@calendar@group_quo)
  ) |>
    mutate(
      year=lubridate::year(date)
      ,quarter=lubridate::quarter(date)
    ) |>
    dplyr::relocate(date,year,quarter)

  return(out_dbi)
}


#' Previous quarter-to-date for tibble objects
#'
#' @param x ti_tbl
#'
#' @returns tibble
#'
qoqtd_fn <- function(x){



  ytd_tbl <- qtd_tbl(x)

  #pytd table

  pytd_tbl <- pqtd_tbl(x) |>
    dplyr::rename(
      !!x@value@second_column_name:=!!x@value@new_column_name
    )

  # join tables together

  out_tbl <-   dplyr::left_join(
    ytd_tbl
    ,pytd_tbl
    ,by=dplyr::join_by(date==date,year,quarter,!!!x@calendar@group_quo)
  )


  return(out_tbl)

}

#' quarter-to-date over previous quarter
#'
#' @param x ti_tbl
#'
#' @returns tibble
#'
qtdopq_fn <- function(x){

  # year-to-date table
  x@value@new_column_name <- "qtd"
  qtd_dbi <-  qtd_fn(x)

  x@time_unit@value <- "quarter"
  x@value@new_column_name <- "pq"

  pq_dbi <-  qoq(x)

  #aggregate to prior year
  pq_tbl <-   qtd_tbl |>
    dplyr::mutate(
      year=lubridate::year(date)
      ,quarter=lubridate::quarter(date)
    ) |>
    dplyr::group_by(year,quarter,!!!x@calendar@group_quo) |>
    dplyr::summarise(
      pq=base::sum(!!x@value@value_quo,na.rm=TRUE)
      ,.groups="drop"
    ) |>
    dplyr::mutate(
      !!x@value@second_column_name:=dplyr::lag(pq,x@fn@lag_n)
    ) |>
    dplyr::select(-c(pq))

  # join together

  out_tbl <-  qtd_tbl |>
    dplyr::left_join(
      pq_tbl
      ,by=dplyr::join_by(year,quater,!!!x@calendar@group_quo)
    )

  return(out_tbl)
}

## month related functions -------------------------

#' Month-to-date for tibble objects
#'
#' @param x ti_tbl
#'
#' @returns tibble
#'
mtd_fn <- function(x){


  full_tbl <-  create_calendar(x) |>
    dplyr::mutate(
      year=lubridate::year(date)
      ,month=lubridate::month(date)
      ,.before = 1
    )



  out_tbl <- full_tbl |>
    dplyr::group_by(year,month,!!!x@calendar@group_quo) |>
    dplyr::arrange(date,.by_group = TRUE) |>
    dplyr::mutate(
      !!x@new_column_name:=base::cumsum(!!x@value_quo)
    ) |>
    dplyr::ungroup()

  return(out_tbl)

}



#' Previous month-to-date for tibble objects
#'
#' @param x ti_tbl
#'
#' @returns tibble
#'
pmtd_fn <- function(x){


  # create calendar table

  full_tbl <-  create_calendar(x) |>
    dplyr::mutate(
      year=lubridate::year(date)
      ,month=lubirdate::month(date)
      ,.before = 1
    )

  # create lag table
  lag_tbl <- full_tbl|>
    dplyr::group_by(year,month,!!!x@calendar@group_quo) |>
    dplyr::arrange(date,.by_group = TRUE) |>
    dplyr::mutate(
      ,date_lag=date+lubridate::months(x@fn@lag_n)
      ,!!x@value@new_column_name:=cumsum(!!x@value@value_quo)
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-c(date,month,year,!!x@value@value_quo))


  # join tables together
  out_tbl <-   dplyr::left_join(
    full_tbl
    ,lag_tbl
    ,by=dplyr::join_by(date==date_lag,!!!x@calendar@group_quo)
  ) |>
    dplyr::select(-c(!!x@value@value_quo))

  return(out_tbl)

}


#' Current year to date over previous year-to-date for tibble objects
#'
#' @param x ti_tbl
#'
#' @returns tibble
#'
momtd_fn <- function(x){

  # ytd table

  mtd_tbl <- mtd_tbl(x)

  #pytd table

  pmtd_tbl <- pmtd_tbl(x) |>
    dplyr::rename(
      !!x@value@second_column_name:=!!x@value@new_column_name
    )

  # join tables together

  out_tbl <-   dplyr::left_join(
    ytd_tbl
    ,pytd_tbl
    ,by=dplyr::join_by(date==date,year,month,!!!x@calendar@group_quo)
  )

  return(out_tbl)

}


#' Year-over-year
#'
#' @param x ti_tbl
#'
#' @returns tibble
#'
mom_fn <- function(x){


  full_tbl <-  create_calendar(x)

  lag_tbl <- full_tbl|>
    dplyr::arrange(date,.by_group = TRUE) |>
    dplyr::mutate(
      date_lag=dplyr::lead(date,n = x@fn@lag_n)
      ,!!x@value@new_column_name:=!!x@value@value_quo
    ) |>
    dplyr::select(-c(date,!!x@value@value_quo)) |>
    dplyr::ungroup()


  out_tbl <-   dplyr::left_join(
    full_tbl
    ,lag_tbl
    ,by=dplyr::join_by(date==date_lag,!!!x@calendar@group_quo)
  )

  return(out_tbl)

}

#' Year-over-year
#'
#' @param x ti_tbl
#'
#' @returns tibble
#'
mtdopm_fn <- function(x){

  # year-to-date table
  ytd_tbl <-  ytd_tbl(x)


  #aggregate to prior year
  pm_tbl <-   ytd_tbl |>
    dplyr::mutate(
      year=lubridate::year(date)
      ,month=lubridate::month(date)
    ) |>
    dplyr::group_by(year,month,!!!x@calendar@group_quo) |>
    dplyr::summarise(
      fm=base::sum(!!x@value@value_quo,na.rm=TRUE)
      ,.groups="drop"
    ) |>
    dplyr::mutate(
      !!x@value@second_column_name:=dplyr::lag(fm,x@fn@lag_n)
    ) |>
    dplyr::select(-c(fm))

  # join together

  out_tbl <-  ytd_tbl |>
    dplyr::left_join(
      pm_tbl
      ,by=dplyr::join_by(year,month,!!!x@calendar@group_quo)
    )

  return(out_tbl)
}




## week related functions-----------------

#' Week-to-date for tibble objects
#'
#' @param x ti_tbl
#'
#' @returns tibble
#'
wtd_fn <- function(x){

  full_tbl <-  create_calendar(x) |>
    dplyr::mutate(
      year=lubridate::year(date)
      ,month=lubridate::month(date)
      ,week=lubridate::week(date)
      ,.before = 1
    )



  out_tbl <- full_tbl |>
    dplyr::group_by(year,month,week,!!!x@calendar@group_quo) |>
    dplyr::arrange(date,.by_group = TRUE) |>
    dplyr::mutate(
      !!x@new_column_name:=base::cumsum(!!x@value_quo)
    ) |>
    dplyr::ungroup()

  return(out_tbl)
}




#' Previous month-to-date for tibble objects
#'
#' @param x ti_tbl
#'
#' @returns tibble
#'
pwtd_fn <- function(x){


  # create calendar table

  full_tbl <-  create_calendar(x) |>
    dplyr::mutate(
      year=lubridate::year(date)
      ,month=lubirdate::month(date)
      ,week=lubirdate::week(date)
      ,.before = 1
    )

  # create lag table
  lag_tbl <- full_tbl|>
    dplyr::group_by(year,month,week,!!!x@calendar@group_quo) |>
    dplyr::arrange(date,.by_group = TRUE) |>
    dplyr::mutate(
      ,date_lag=date+lubridate::weeks(x@fn@lag_n)
      ,!!x@value@new_column_name:=cumsum(!!x@value@value_quo)
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-c(date,month,year,week,!!x@value@value_quo))


  # join tables together
  out_tbl <-   dplyr::left_join(
    full_tbl
    ,lag_tbl
    ,by=dplyr::join_by(date==date_lag,!!!x@calendar@group_quo)
  ) |>
    dplyr::select(-c(!!x@value@value_quo))

  return(out_tbl)

}


#' Current year to date over previous year-to-date for tibble objects
#'
#' @param x ti_tbl
#'
#' @returns tibble
#'
wowtd_fn <- function(x){

  # ytd table

  wtd_tbl <- wtd_tbl(x)

  #pytd table

  pwtd_tbl <- pwtd_tbl(x) |>
    dplyr::rename(
      !!x@value@second_column_name:=!!x@value@new_column_name
    )

  # join tables together

  out_tbl <-   dplyr::left_join(
    wtd_tbl
    ,pwtd_tbl
    ,by=dplyr::join_by(date==date,year,month,week,!!!x@calendar@group_quo)
  )

  return(out_tbl)

}


#' Year-over-year
#'
#' @param x ti_tbl
#'
#' @returns tibble
#'
wow_fn <- function(x){


  full_tbl <-  create_calendar(x)

  lag_tbl <- full_tbl|>
    dplyr::arrange(date,.by_group = TRUE) |>
    dplyr::mutate(
      date_lag=dplyr::lead(date,n = x@fn@lag_n)
      ,!!x@value@new_column_name:=!!x@value@value_quo
    ) |>
    dplyr::select(-c(date,!!x@value@value_quo)) |>
    dplyr::ungroup()


  out_tbl <-   dplyr::left_join(
    full_tbl
    ,lag_tbl
    ,by=dplyr::join_by(date==date_lag,!!!x@calendar@group_quo)
  )

  return(out_tbl)

}

#' Year-over-year
#'
#' @param x ti_tbl
#'
#' @returns tibble
#'
wtdopw_fn <- function(x){

  # year-to-date table
  wtd_tbl <-  wtd_tbl(x)


  #aggregate to prior year
  wm_tbl <-   wtd_tbl |>
    dplyr::mutate(
      year=lubridate::year(date)
      ,month=lubridate::month(date)
      ,week=lubridate::week(date)
    ) |>
    dplyr::group_by(year,month,week,!!!x@calendar@group_quo) |>
    dplyr::summarise(
      fw=base::sum(!!x@value@value_quo,na.rm=TRUE)
      ,.groups="drop"
    ) |>
    dplyr::mutate(
      !!x@value@second_column_name:=dplyr::lag(fw,x@fn@lag_n)
    ) |>
    dplyr::select(-c(fw))

  # join together

  out_tbl <-  wtd_tbl |>
    dplyr::left_join(
      pw_tbl
      ,by=dplyr::join_by(year,month,week,!!!x@calendar@group_quo)
    )

  return(out_tbl)
}


## all to date related functions ----------------

#' All-to-date for tibble objects
#'
#' @param x ti_tbl
#'
#' @returns tibble
atd_fn <- function(x){

  full_tbl <-  create_calendar(x)

  out_tbl <- full_tbl |>
    dplyr::group_by(!!!x@calendar@group_quo) |>
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
dod_fn <- function(x){

  full_tbl <-  create_calendar(x)



  lag_tbl <- full_tbl|>
    dplyr::arrange(date,.by_group = TRUE) |>
    dplyr::mutate(
      date_lag=lubridate::`%m+%`(date,lubridate::days(x@lag_n))
      ,!!x@new_column_name:=!!x@value_quo
    ) |>
    dplyr::select(-c(date,!!x@value_quo)) |>
    dplyr::ungroup()

  out_tbl <-   dplyr::left_join(
    full_tbl
    ,lag_tbl
    ,by=dplyr::join_by(date==date_lag,!!!x@calendar@group_quo)
  )
  # mutate(
  # !!x@new_column_name:= dplyr::coalesce(.data[[rlang::englue(x@new_column_name)]],0)
  # )

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
#' ytd(fpaR::sales,date=date,.value=quantity,calendar_type="standard")
ytd <- function(.data,.date,.value,calendar_type){

  # assigns inputs to ytd_tbl class

  x <- ti(
    calendar(
      data                 =.data
      ,calendar_type       = calendar_type
      ,date_vec            = rlang::as_label(rlang::enquo(.date))
    )
    ,time_unit             = time_unit("day")
    ,action                = action("aggregate")
    ,value = value(
      value_vec            =  rlang::as_label(rlang::enquo(.value))
      ,new_column_name     = "ytd"
      )
    ,fn=fn(
      new_date_column_name = "year"
      ,lag_n = NA_integer_
      ,fn_exec = ytd_fn
    )
  )

  return(x)

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
#' pytd(fpaR::sales,date=date,.value=quantity,calendar_type="standard")
pytd <- function(.data,.date,.value,calendar_type,lag_n){


  # assigns inputs to ytd_tbl class
  out <- ti(
    calendar(
      data=.data
      ,calendar_type=calendar_type
      ,date_vec = rlang::as_label(rlang::enquo(.date))
    )
    ,time_unit = time_unit("day")
    ,action=action("aggregate")
    ,value=value(
      value_vec = rlang::as_label(rlang::enquo(.value))
      ,new_column_name = "pytd"
    )
    ,fn=fn(
      lag_n = lag_n
      ,new_date_column_name = "year"
      ,fn_exec =pytd_fn
    )
  )

  return(out)
}

#' Current year-to-.date compared to previous year-to-.date
#' @param .data either a tibble or  DBI object
#' @param .date the .date column to aggregate
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
#' ytd(fpaR::sales,.date=.date,.value=quantity,calendar_type="standard")
yoytd <- function(.data,.date,.value,calendar_type,lag_n){


  # Vali.date inputs

  # assigns inputs to yoytd class

  x <- ti(
    calendar(
      data=.data
      ,calendar_type=calendar_type
      ,date_vec = rlang::as_label(rlang::enquo(.date))
    )
    ,time_unit = time_unit("day")
    ,action=action("aggregate")
    ,value=value(
      value_vec = rlang::as_label(rlang::enquo(.value))
      ,new_column_name = c("ytd","pytd")
    )
    ,fn=fn(
      lag_n = lag_n
      ,new_date_column_name = "year"
      ,fn_exec = yoytd_fn
    )
  )

  return(x)
}


#' Year-over-year
#'
#' @param .data tibble or DBI object
#' @param .date the .date column to aggregate
#' @param value the value column to summarize
#' @param calendar_type either 'standard' or '5-5-4' calendar
#' @param lag_n the number of months to lag, default is 1
#'
#' @returns mom_tbl or mom_dbi
#' @export
#'
#' @examples
#' yoy(fpaR::sales,.date=order_.date,.value=quantity,calendar_type='standard',lag_n=1)
yoy <- function(.data,.date,.value,calendar_type,lag_n=1){

  out <- ti(
    calendar(
      data=.data
      ,calendar_type=calendar_type
      ,date_vec = rlang::as_label(rlang::enquo(.date))
    )
    ,time_unit = time_unit("year")
    ,action=action(c("aggregate","shift","compare"))

    ,value=value(
      value_vec = rlang::as_label(rlang::enquo(.value))
      ,new_column_name = "yoy"
    )
    ,fn=fn(
      new_date_column_name = "date"
      ,lag_n=lag_n
      ,fn_exec = yoy_fn
    )
  )

  return(out)

}

#' Year-to-.date over full previous year
#'
#' @param .data tibble or DBI object
#' @param .date the .date column to aggregate
#' @param value the value column to summarize
#' @param calendar_type either 'standard' or '5-5-4' calendar
#' @param lag_n the number of years to lag, default is 1
#'
#' @returns mom_tbl or mom_dbi
#' @export
#'
#' @examples
#' ytdoy(fpaR::sales,.date=order_.date,.value=quantity,calendar_type='standard',lag_n=1)
ytdopy <- function(.data,.date,.value,calendar_type,lag_n=1){

  out <- ti(
    calendar(
      data=.data
      ,calendar_type=calendar_type
      ,date_vec = rlang::as_label(rlang::enquo(.date))
    )
    ,time_unit = time_unit("day")

    ,action=action(c("aggregate","shift","compare"))

    ,value=value(
      value_vec = rlang::as_label(rlang::enquo(.value))
      ,new_column_name = c("ytd","py")
    )

    ,fn=fn(
      new_date_column_name = c("date","year")
      ,lag_n=lag_n
      ,fn_exec = ytdopy_fn
    )
  )

  return(out)

}




## quarter related ti_tbl-----------------------------

#' quarter-to-.date
#'
#' @param .data either a tibble or  dbi object
#' @param .date the .date column to aggregate
#' @param value the value column to summarize
#' @param calendar_type either 'standard' or '5-5-4' calendar
#' @description
#' this calculates the quarterly cumulative sum of targeted value using a standard or 5-5-4 calendar respecting
#' any groups that are passed through with `dplyr::group_by()`.
#'
#' use `calculate()` to return the results
#'
#' @returns qtd_tbl or qtd_dbi object depending on what is passed through
#' @export
#'
#' @examples
#' qtd(fpar::sales,.date=.date,.value=quantity,calendar_type="standard")
qtd <- function(.data,.date,.value,calendar_type){


  # Aggregate data based on provided time unit

  out <- ti(
    calendar(
      data                       = .data
      ,calendar_type             = calendar_type
      ,date_vec                  =  rlang::as_label(rlang::enquo(.date))
    )
    ,time_unit                   = time_unit("day")
    ,action                      = action("aggregate")
    ,value = value(
      value_vec                 = rlang::as_label(rlang::enquo(.value))
      ,new_column_name           = "qtd"
    )
    ,fn=fn(
      new_date_column_name       = c("year","quarter")
      ,lag_n                     = NA_integer_
      ,fn_exec                   = qtd_fn
    )
  )


  return(out)
}



#' Previous quarter quarter-to-.date
#' @param .data either a tibble or  DBI object
#' @param .date the .date column to aggregate
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
#' ytd(fpaR::sales,.date=.date,.value=quantity,calendar_type="standard")
pqtd <- function(.data,.date,.value,calendar_type,lag_n){

  # Vali.date inputs
  assertthat::assert_that(base::is.data.frame(.data), msg = "data must be a data frame")

  # assigns inputs to pqtd_tbl class
  out <- ti(
    calendar(
      data=.data
      ,calendar_type=calendar_type
      ,date_vec = rlang::as_label(rlang::enquo(.date))
    )
    ,time_unit = time_unit("day")
    ,action=action("aggregate")
    ,value=value(
      value_vec = rlang::as_label(rlang::enquo(.value))
      ,new_column_name = "pqtd"
    )
    ,fn=fn(
      lag_n = lag_n
      ,new_date_column_name    = c("year","quarter")
      ,fn_exec=pytd_fn
    )
  )

  return(out)
}


#' Current year-to-.date compared to previous year-to-.date
#' @param .data either a tibble or  DBI object
#' @param .date the .date column to aggregate
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
#' ytd(fpaR::sales,.date=.date,.value=quantity,calendar_type="standard")
qoqtd <- function(.data,.date,.value,calendar_type,lag_n){


  # Vali.date inputs
  assertthat::assert_that(base::is.data.frame(.data), msg = "data must be a data frame")

  # assigns inputs to yoytd class

  out <- ti(
    calendar(
      data=.data
      ,calendar_type=calendar_type
      ,date_vec = rlang::as_label(rlang::enquo(.date))
    )
    ,time_unit = time_unit("day")
    ,action=action("aggregate")
    ,value=value(
      value_vec = rlang::as_label(rlang::enquo(.value))
      ,new_column_name = c("qtd","pqtd")
    )
    ,fn=fn(
      lag_n = lag_n
      ,new_date_column_name = c("year","quarter")
      ,fn_exec=qoqtd_fn
    )
  )

  return(out)
}


#' Quarter-to-date over full previous quarter
#'
#' @param .data tibble or DBI object
#' @param .date the .date column to aggregate
#' @param value the value column to summarize
#' @param calendar_type either 'standard' or '5-5-4' calendar
#' @param lag_n the number of years to lag, default is 1
#'
#' @returns mom_tbl or mom_dbi
#' @export
#'
#' @examples
#' ytdoy(fpaR::sales,.date=order_.date,.value=quantity,calendar_type='standard',lag_n=1)
qtdopq <- function(.data,.date,.value,calendar_type,lag_n=1){

  out <- ti(
    calendar(
      data=.data
      ,calendar_type=calendar_type
      ,date_vec = rlang::as_label(rlang::enquo(.date))
    )
    ,time_unit = time_unit("day")
    ,action=action(c("aggregate","shift","compare"))

    ,value=value(
      value_vec = rlang::as_label(rlang::enquo(.value))
      ,new_column_name  = "qtd"
    )
    ,fn=fn(
      new_date_column_name = c(".date","year","quarter")
      ,lag_n=lag_n
      ,fn_exec=qtdopq_fn
    )
  )

  return(out)

}
## month related functions


#' Month-over-month
#'
#' @param .data tibble or DBI object
#' @param .date the .date column to aggregate
#' @param value the value column to summarize
#' @param calendar_type either 'standard' or '5-5-4' calendar
#' @param lag_n the number of months to lag, default is 1
#'
#' @returns mom_tbl or mom_dbi
#' @export
#'
#' @examples
#' mom(fpaR::sales,.date=order_.date,.value=quantity,calendar_type='standard',lag_n=1)
qoq <- function(.data,.date,.value,calendar_type,lag_n=1){

  out <- ti(
    calendar(
      data=.data
      ,calendar_type=calendar_type
      ,date_vec = rlang::as_label(rlang::enquo(.date))
    )
    ,time_unit = time_unit("quarter")
    ,action=action("aggregate")
    ,value=value(
      value_vec = rlang::as_label(rlang::enquo(.value))
      ,new_column_name  = "qoq"
    )
    ,fn=fn(
      lag_n = lag_n
      ,new_date_column_name = c("year","quarter")
      ,fn_exec=qoq_fn
    )
  )



  return(out)
}



## month related ti_tbl-------------------

#' Month-to-.date
#'
#' @param .data either a tibble or  DBI object
#' @param .date the .date column to aggregate
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
#' mtd(fpaR::sales,.date=.date,.value=quantity,calendar_type="standard")
mtd <- function(.data,.date,.value,calendar_type){

    # Vali.date inputs
    assertthat::assert_that(base::is.data.frame(.data), msg = "data must be a data frame")

    out <- ti(
      calendar(
        data=.data
        ,calendar_type=calendar_type
        ,date_vec = rlang::as_label(rlang::enquo(.date))
      )
      ,time_unit = time_unit("day")
      ,action=action("aggregate")
      ,value=value(
        value_vec = rlang::as_label(rlang::enquo(.value))
        ,new_column_name_prefix = "mtd"
      )
      ,fn=fn_tbl(
        new_date_column_name = c("year","month")
        ,lag_n = NA_integer_
        ,fn_exec=mtd_fn
      )
    )

  return(out)

}

#' Prior month-to-date
#'
#' @param .data either a tibble or  DBI object
#' @param .date the .date column to aggregate
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
#' mtd(fpaR::sales,.date=.date,.value=quantity,calendar_type="standard")
pmtd <- function(.data,.date,.value,calendar_type,lag_n){

  # Vali.date inputs
  assertthat::assert_that(base::is.data.frame(.data), msg = "data must be a data frame")

  out <- ti(
    calendar(
      data=.data
      ,calendar_type=calendar_type
      ,date_vec = rlang::as_label(rlang::enquo(.date))
    )
    ,time_unit = time_unit("day")
    ,action=action("aggregate")
    ,value=value(
      value_vec = rlang::as_label(rlang::enquo(.value))
      ,new_column_name_prefix = "pmtd"
    )
    ,fn=fn(
      new_date_column_name = c("year","month")
      ,lag_n = lag_n
      ,fn_exec=pmtd_fn
    )
  )

  return(out)
}

#' Month-to-.date
#'
#' @param .data either a tibble or  DBI object
#' @param .date the .date column to aggregate
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
#' mtd(fpaR::sales,.date=.date,.value=quantity,calendar_type="standard")
momtd <- function(.data,.date,.value,calendar_type,lag_n){

  # Vali.date inputs
  assertthat::assert_that(base::is.data.frame(.data), msg = "data must be a data frame")

  out <- ti(
    calendar(
      data=.data
      ,calendar_type=calendar_type
      ,date_vec = rlang::as_label(rlang::enquo(.date))
    )
    ,time_unit = time_unit("day")
    ,action=action("aggregate")
    ,value=value(
      value_vec = rlang::as_label(rlang::enquo(.value))
      ,new_column_name_prefix = "momtd"
    )
    ,fn=fn(
      new_date_column_name = c("year","month")
      ,lag_n = lag_n
      ,fn_exec = momtd_fn
    )
  )

  return(out)
}



#' Month-to-.date
#'
#' @param .data either a tibble or  DBI object
#' @param .date the .date column to aggregate
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
#' mtd(fpaR::sales,.date=.date,.value=quantity,calendar_type="standard")
mtdopm <- function(.data,.date,.value,calendar_type,lag_n){

  # Vali.date inputs
  assertthat::assert_that(base::is.data.frame(.data), msg = "data must be a data frame")

  out <- ti(
    calendar(
      data=.data
      ,calendar_type=calendar_type
      ,date_vec = rlang::as_label(rlang::enquo(.date))
    )
    ,time_unit = time_unit("day")
    ,action=action("aggregate")
    ,value=value(
      value_vec = rlang::as_label(rlang::enquo(.value))
      ,new_column_name_prefix = "mtdopm"
    )
    ,fn=fn(
      new_date_column_name = c("year","month")
      ,lag_n = lag_n
      ,fn_exec = mtdopm_fn
    )
  )

  return(out)
}

#' Month-over-month
#'
#' @param .data tibble or DBI object
#' @param .date the .date column to aggregate
#' @param value the value column to summarize
#' @param calendar_type either 'standard' or '5-5-4' calendar
#' @param lag_n the number of months to lag, default is 1
#'
#' @returns mom_tbl or mom_dbi
#' @export
#'
#' @examples
#' mom(fpaR::sales,.date=order_.date,.value=quantity,calendar_type='standard',lag_n=1)
mom <- function(.data,.date,.value,calendar_type,lag_n=1){

  out <- ti(
    calendar(
      data=.data
      ,calendar_type=calendar_type
      ,date_vec = rlang::as_label(rlang::enquo(.date))
    )
    ,time_unit = time_unit("day")
    ,action=action("aggregate","shift","compare")
    ,value=value(
      value_vec = rlang::as_label(rlang::enquo(.value))
      ,new_column_name_prefix = "mom"
    )
    ,fn=fn(
      new_date_column_name = c("date","year","month")
      ,lag_n = NA_integer_
      ,fn_exec=mom_fn
    )
  )
    return(out)

}


## week related ti_tbl-------------


#' Week-to-.date
#'
#' @param .data either a tibble or  DBI object
#' @param .date the .date column to aggregate
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
#' wtd(fpaR::sales,.date=.date,.value=quantity,calendar_type="standard")
wtd <- function(.data,.date,.value,calendar_type){

  # Vali.date inputs

  out <- ti(
    calendar(
      data=.data
      ,calendar_type=calendar_type
      ,date_vec = rlang::as_label(rlang::enquo(.date))
    )
    ,time_unit = time_unit("day")
    ,action=action("aggregate")
    ,value=value(
      value_vec = rlang::as_label(rlang::enquo(.value))
      ,new_column_name_prefix = "wtd"
    )
    ,fn=fn(
      new_date_column_name = c("year","month","week")
      ,lag_n = NA_integer_
      ,fn_exec=wtd_fn
    )
  )

  return(out)

}
#' Week-to-.date
#'
#' @param .data either a tibble or  DBI object
#' @param .date the .date column to aggregate
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
#' wtd(fpaR::sales,.date=.date,.value=quantity,calendar_type="standard")
pwtd <- function(.data,.date,.value,calendar_type,lag_n){

  # Vali.date inputs
  assertthat::assert_that(base::is.data.frame(.data), msg = "data must be a data frame")

  out <- ti(
    calendar(
      data=.data
      ,calendar_type=calendar_type
      ,date_vec = rlang::as_label(rlang::enquo(.date))
    )
    ,time_unit = time_unit("day")
    ,action=action("aggregate")
    ,value=value(
      value = rlang::as_label(rlang::enquo(.value))
      ,new_column_name_prefix = "pwtd"
    )
    ,fn=fn(
      new_date_column_name = c("year","month","week")
      ,lag_n = lag_n
      ,fn_exec=pwtd_fn
    )
  )

  return(out)

}

#' Week-to-.date
#'
#' @param .data either a tibble or  DBI object
#' @param .date the .date column to aggregate
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
#' wtd(fpaR::sales,.date=.date,.value=quantity,calendar_type="standard")
wowtd <- function(.data,.date,.value,calendar_type,lag_n){

  # Vali.date inputs
  assertthat::assert_that(base::is.data.frame(.data), msg = "data must be a data frame")

  out <- ti(
    calendar(
      data=.data
      ,calendar_type=calendar_type
      ,date_vec = rlang::as_label(rlang::enquo(.date))
    )
    ,time_unit = time_unit("day")
    ,action=action("aggregate")
    ,value=value(
      value_vec = rlang::as_label(rlang::enquo(.value))
      ,new_column_name_prefix = "wowtd"
    )
    ,fn=fn(
      new_date_column_name = c("year","month","week")
      ,lag_n = lag_n
      ,fn_exec=wowtd_fn
    )
  )
  return(out)
}


#' Week-to-.date
#'
#' @param .data either a tibble or  DBI object
#' @param .date the .date column to aggregate
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
#' wtd(fpaR::sales,.date=.date,.value=quantity,calendar_type="standard")
wtwopw <- function(.data,.date,.value,calendar_type,lag_n){

  # Vali.date inputs
  assertthat::assert_that(base::is.data.frame(.data), msg = "data must be a data frame")

  out <- ti(
    calendar(
      data=.data
      ,calendar_type=calendar_type
      ,date_vec = rlang::as_label(rlang::enquo(.date))
    )
    ,time_unit = time_unit("day")
    ,action=action("aggregate")
    ,value=value(
      value_vec = rlang::as_label(rlang::enquo(.value))
      ,new_column_name_prefix = "wtwopw"
    )
    ,fn=fn(
      new_date_column_name = c("year","month","week")
      ,lag_n = lag_n
      ,fn_exec=wtdopw_fn
    )
  )


  return(out)
}


#' Week-over-Week
#'
#' @param .data tibble or DBI object
#' @param .date the .date column to aggregate
#' @param value the value column to summarize
#' @param calendar_type either 'standard' or '5-5-4' calendar
#' @param lag_n the number of weeks to lag, default is 1
#'
#' @returns wow_tbl or wow_dbi
#' @export
#'
#' @examples
#' wow(fpaR::sales,.date=order_.date,.value=quantity,calendar_type='standard',lag_n=1)
wow <- function(.data,.date,.value,calendar_type,lag_n=1){


  out <- ti(
    calendar(
      data=.data
      ,calendar_type=calendar_type
      ,date_vec = rlang::as_label(rlang::enquo(.date))
    )
    ,time_unit = time_unit("day")
    ,action=action("aggregate","shift","compare")
    ,value=value(
      value_vec = rlang::as_label(rlang::enquo(.value))
      ,new_column_name_prefix = "wow"
    )
    ,fn=fn(
      new_date_column_name = c("date")
      ,lag_n = lag_n
      ,fn_exec=wow_fn
    )
  )

    return(out)

}


## all related ti_tbl-------------------------

#' All-to-.date
#'
#' @param .data either a tibble or  DBI object
#' @param .date the .date column to aggregate
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
#' atd(fpaR::sales,.date=.date,.value=quantity,calendar_type="standard")
atd <- function(.data,.date,.value,calendar_type){

  # Vali.date inputs


  out <- ti(
    calendar(
      data=.data
      ,calendar_type=calendar_type
      ,date_vec = rlang::as_label(rlang::enquo(.date))
    )
    ,time_unit = time_unit("day")
    ,action=action("aggregate")
    ,value=value(
      value_vec = rlang::as_label(rlang::enquo(.value))
      ,new_column_name_prefix = "atd"
    )
    ,fn=fn(
      new_date_column_name = c("date")
      ,lag_n = NA_integer_
      ,fn_exec=atd_fn
    )
  )

  return(out)

}


## comparison ti_tbl------------------


#' day over day calculation
#'
#' @param .data tibble or DBI object
#' @param .date the .date column to aggregate
#' @param value the value column to summarize
#' @param calendar_type either 'standard' or '5-5-4' calendar
#' @param lag_n the number of periods to lag, default is 1
#'
#' @returns dod_tbl or dbi_tbl
#' @export
#'
#' @examples
#' dod(fpaR::sales,.date=order_.date,.value=quantity,calendar_type='standard',lag_n=1)
dod <- function(.data,.date,.value,calendar_type,lag_n=1){

  out <- ti(
    calendar(
      data=.data
      ,calendar_type=calendar_type
      ,date_vec = rlang::as_label(rlang::enquo(.date))
    )
    ,time_unit = time_unit("day")
    ,action=action("aggregate","shift","compare")
    ,value=value(
      value_vec = rlang::as_label(rlang::enquo(.value))
      ,new_column_name_prefix = "dod"
      ,second_column_name_prefix = NA_character_
    )
    ,fn=fn(
      new_date_column_name = c("date")
      ,lag_n = NA_integer_
      ,fn_exec=dod_fn
    )
  )
  return(out)
}
