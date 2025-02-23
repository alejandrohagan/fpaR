
## year related functions

#' Year-to-date execution function
#'
#' @param x ti_tbl
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
#' @param x ti object
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
    dbplyr::window_order(date) |>
    dplyr::mutate(
      date_lag=as.Date(date+lubridate::years(!!x@fn@lag_n))
      ,!!x@value@new_column_name[[1]]:=cumsum(!!x@value@value_quo)
      ,.by=c(year,!!!x@calendar@group_quo)
    ) |>
    dplyr::select(-c(date,year,!!x@value@value_quo))


  # join tables together
  out_tbl <-   dplyr::full_join(
    full_dbi
    ,lag_dbi
    ,by=dplyr::join_by(date==date_lag,!!!x@calendar@group_quo)
  ) |>
    dplyr::select(-c(!!x@value@value_quo)) |>
    dbplyr::window_order(date,year,quarter,!!!x@calendar@group_quo) |>
    dplyr::group_by(date,year,!!!x@calendar@group_quo) |>
    tidyr::fill(date,.direction = "down") |>
    dplyr::ungroup() |>
    dplyr::summarise(
      dplyr::across(dplyr::contains(x@value@value_vec),\(x) sum(x,na.rm=TRUE))
      ,.by=c(date,year,!!!x@calendar@group_quo)
    ) |>
    filter(
      !is.na(year)
    )

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

  ytd_dbi <- ytd(.data=x@calendar@data,.date=!!x@calendar@date_quo,.value = !!x@value@value_quo,calendar_type = x@calendar@calendar_type) |>
    ytd_fn()

  #pytd table

  pytd_dbi <- pytd(.data=x@calendar@data,.date=!!x@calendar@date_quo,.value = !!x@value@value_quo,calendar_type = x@calendar@calendar_type,lag_n = x@fn@lag_n) |>
    pytd_fn()

  # join tables together

  out_dbi <-   dplyr::full_join(
    ytd_dbi
    ,pytd_dbi
    ,by=dplyr::join_by(date==date,year,!!!x@calendar@group_quo)
  ) |>
    dplyr::group_by(date,!!!x@calendar@group_quo) |>
    tidyr::fill(date,.direction = "down") |>
    dplyr::ungroup() |>
    dplyr::summarise(
      dplyr::across(dplyr::contains(x@value@value_vec),\(x) sum(x,na.rm=TRUE))
      ,.by=c(date,!!!x@calendar@group_quo)
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
      ,.after=date
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
  ytd_dbi <-  ytd(.data = x@calendar@data,.date = !!x@calendar@date_quo,.value = !!x@value@value_quo,calendar_type = x@calendar@calendar_type) |>
    calculate()

  #aggregate to prior year

  py_dbi <-   yoy(.data = x@calendar@data,.date = !!x@calendar@date_quo,.value = !!x@value@value_quo,calendar_type = x@calendar@calendar_type,lag_n = x@fn@lag_n) |>
    calculate()

  # join together

 out_dbi <-  ytd_dbi |>
   select(
     -c(!!x@value@value_quo)
   ) |>
    dplyr::left_join(
      py_dbi |> dplyr::select(
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

# x <- fpaR::qtd(.data,.date = order_date,.value = margin,calendar_type = "standard")

  full_dbi <-  create_calendar(x) |>
    dplyr::mutate(
      year=lubridate::year(date)
      ,quarter=lubridate::quarter(date)
      ,.before = 1
    )

    out_dbi <- full_dbi |>
    dbplyr::window_order(date) |>
    dplyr::mutate(
      !!x@value@new_column_name:=base::cumsum(!!x@value@value_quo)
      ,.by=c(year,quarter,!!!x@calendar@group_quo)
    ) |>
    dplyr::ungroup()

  return(out_dbi)

}


#' Previous quarter-to-date for tibble objects
#' @param x ti object
#' @description
#' `pqtd_fn()` summarizes a tibble to target time unit and completes the calendar to ensure
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
pqtd_fn <- function(x){


  # create calendar table
  full_dbi <-  create_calendar(x) |>
    dplyr::mutate(
      year=lubridate::year(date)
      ,quarter=lubridate::quarter(date)
      ,.before = 1
    )

  lag_dbi <- full_dbi |>
    dbplyr::window_order(date,!!!x@calendar@group_quo) |>
    mutate(
      date_lag=as.Date(sql("date - INTERVAL '3 months' * 2"))
      ,!!x@value@new_column_name[[1]]:=cumsum(!!x@value@value_quo)
      ,.by=c(year,!!!x@calendar@group_quo)
    ) |>
    dplyr::select(-c(date,year,quarter,!!x@value@value_quo))



  # join tables together
  out_tbl <-   dplyr::full_join(
    full_dbi
    ,lag_dbi
    ,by=dplyr::join_by(date==date_lag,!!!x@calendar@group_quo)
  ) |>
    dplyr::select(-c(!!x@value@value_quo)) |>
    dbplyr::window_order(date,year,quarter,!!!x@calendar@group_quo) |>
    dplyr::group_by(date,year,quarter,!!!x@calendar@group_quo) |>
    tidyr::fill(date,.direction = "down") |>
    dplyr::ungroup() |>
    dplyr::summarise(
      dplyr::across(dplyr::contains(x@value@value_vec),\(x) sum(x,na.rm=TRUE))
      ,.by=c(date,year,quarter,!!!x@calendar@group_quo)
    ) |>
    filter(
      !is.na(year)
    )

  return(out_tbl)

}


#' Current quarter to date over previous quarter-to-date for tibble objects
#'
#' @param x ti_tbl
#'
#' @returns tibble
#'
qoqtd_fn <- function(x){

  # ytd table

qtd_dbi <- qtd(.data=x@calendar@data,.date=!!x@calendar@date_quo,.value = !!x@value@value_quo,calendar_type = x@calendar@calendar_type) |>
  qtd_fn()

  # pytd table

  pqtd_dbi <- pqtd(.data=x@calendar@data,.date=!!x@calendar@date_quo,.value = !!x@value@value_quo,calendar_type = x@calendar@calendar_type,lag_n = x@fn@lag_n) |>
    pqtd_fn()

  # join tables together

  out_dbi <-   dplyr::full_join(
    qtd_dbi
    ,pqtd_dbi
    ,by=dplyr::join_by(date==date,year,quarter,!!!x@calendar@group_quo)
  ) |>
    dplyr::group_by(date,year,quarter,!!!x@calendar@group_quo) |>
    tidyr::fill(date,.direction = "down") |>
    dplyr::ungroup() |>
    dplyr::summarise(
      dplyr::across(dplyr::contains(x@value@value_vec),\(x) sum(x,na.rm=TRUE))
      ,.by=c(date,year,quarter,!!!x@calendar@group_quo)
    ) |>
    dplyr::filter(
      !is.na(year)
    )

  return(out_dbi)

}


#' Quarter-over-quarter execution function
#'
#' @param x ti object
#'
#' @returns dbi object
#'
qoq_fn <- function(x){

  # create calendar
  full_dbi <-  fpaR::create_calendar(x)

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


#
#' quarter-to-date over previous quarter
#'
#' @param x ti_tbl
#'
#' @returns tibble
#'
qtdopq_fn <- function(x){

  # year-to-date table

  qtd_dbi <-  qtd(.data = x@calendar@data,.date = !!x@calendar@date_quo,.value = !!x@value@value_quo,calendar_type = x@calendar@calendar_type) |>
    calculate()

  qoq_dbi <-  qoq(.data = x@calendar@data,.date = !!x@calendar@date_quo,.value = !!x@value@value_quo,calendar_type = x@calendar@calendar_type) |>
    calculate()

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


  full_dbi <-  create_calendar(x) |>
    dplyr::mutate(
      year=lubridate::year(date)
      ,month=lubridate::month(date)
      ,.before = 1
    )

  out_dbi <- full_dbi |>
    dplyr::group_by(year,month,!!!x@calendar@group_quo) |>
    dplyr::arrange(date,.by_group = TRUE) |>
    dplyr::mutate(
      !!x@value@new_column_name:=base::cumsum(!!x@value@value_quo)
    ) |>
    dplyr::ungroup()

  return(out_dbi)

}



#' Previous month-to-date for tibble objects
#'
#' @param x ti_tbl
#'
#' @returns tibble
#'
pmtd_fn <- function(x){


  # create calendar table

  full_dbi <-  create_calendar(x) |>
    dplyr::mutate(
      year=lubridate::year(date)
      ,month=lubridate::month(date)
      ,.before = 1
    )

  # create lag table
  lag_dbi <- full_dbi |>
    dbplyr::window_order(date) |>
    dplyr::mutate(
      date_lag=as.Date(sql("date - INTERVAL '1 months' * 1"))
      ,!!x@value@new_column_name:=cumsum(!!x@value@value_quo)
      ,.by=c(year,month,!!!x@calendar@group_quo)
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-c(date,month,year,!!x@value@value_quo))


  # join tables together
  out_dbi <-   dplyr::left_join(
    full_dbi
    ,lag_dbi
    ,by=dplyr::join_by(date==date_lag,!!!x@calendar@group_quo)
  ) |>
    dplyr::select(-c(!!x@value@value_quo)) |>
    dbplyr::window_order(date) |>
    dplyr::group_by(date,year,month,!!!x@calendar@group_quo) |>
    tidyr::fill(date,.direction = "down") |>
    dplyr::ungroup() |>
    dplyr::summarise(
      dplyr::across(dplyr::contains(x@value@value_vec),\(x) sum(x,na.rm=TRUE))
      ,.by=c(date,year,month,!!!x@calendar@group_quo)
    ) |>
    dplyr::filter(
      !is.na(year)
    )

  return(out_dbi)

}


#' Current year to date over previous year-to-date for tibble objects
#'
#' @param x ti_tbl
#'
#' @returns tibble
#'
momtd_fn <- function(x){

  # mtd table

  mtd_dbi <- mtd(.data = x@calendar@data,.date = !!x@calendar@date_quo,.value = !!x@value@value_quo,calendar_type = x@calendar@calendar_type) |>
    calculate()

  # pmtd table

  pmtd_dbi <- pmtd(.data = x@calendar@data,.date = !!x@calendar@date_quo,.value = !!x@value@value_quo,calendar_type = x@calendar@calendar_type,lag_n = x@fn@lag_n) |>
    calculate()

  # join tables together

  out_dbi <-   dplyr::left_join(
    mtd_dbi
    ,pmtd_dbi
    ,by=dplyr::join_by(date==date,year,month,!!!x@calendar@group_quo)
  ) |>
    dplyr::group_by(date,year,month,!!!x@calendar@group_quo) |>
    tidyr::fill(date,.direction = "down") |>
    dplyr::ungroup() |>
    dplyr::summarise(
      dplyr::across(dplyr::contains(x@value@value_vec),\(x) sum(x,na.rm=TRUE))
      ,.by=c(date,year,month,!!!x@calendar@group_quo)
    ) |>
    dplyr::filter(
      !is.na(year)
    )

  return(out_tbl)

}


#' month-over-month
#'
#' @param x ti object
#'
#' @returns dbi
#'
mom_fn <- function(x){

  full_dbi <-  create_calendar(x)

  # create lag
  lag_dbi <- full_dbi |>
    dbplyr::window_order(date,!!!x@calendar@group_quo) |>
    dplyr::mutate(
      date_lag=dplyr::lead(date,n = !!x@fn@lag_n)
      ,!!x@value@new_column_name[[1]]:=!!x@value@value_quo
      ,.by=c(!!!x@calendar@group_quo)
    ) |>
    dplyr::select(-c(date,!!x@value@value_quo)) |>
    dplyr::filter(!is.na(date_lag))



  # bring tables together
  out_dbi <-   dplyr::left_join(
    full_dbi
    ,lag_dbi
    ,by=dplyr::join_by(date==date_lag,!!!x@calendar@group_quo)
  ) |>
    mutate(
      year=lubridate::year(date)
      ,month=lubridate::month(date)
      ,.after=date
    )

  return(out_dbi)

}

#' Year-over-year
#'
#' @param x ti_tbl
#'
#' @returns tibble
#'
mtdopm_fn <- function(x){


  # year-to-date table
  mtd_dbi <-  mtd(.data = x@calendar@data,.date = !!x@calendar@date_quo,.value = !!x@value@value_quo,calendar_type = x@calendar@calendar_type) |>
    calculate()

  #aggregate to prior year

  pm_dbi <-   mom(.data = x@calendar@data,.date = !!x@calendar@date_quo,.value = !!x@value@value_quo,calendar_type = x@calendar@calendar_type,lag_n = x@fn@lag_n) |>
    calculate()

  # join together

 out_dbi <-  mtd_dbi |>
   dplyr::select(
     -c(!!x@value@value_quo)
   ) |>
    dplyr::left_join(
      pm_dbi |> dplyr::select(
        -c(!!x@value@value_quo)
      )
      ,by=dplyr::join_by(year,month,date,!!!x@calendar@group_quo)
    )

  return(out_dbi)


}




## week related functions-----------------

#' Week-to-date for tibble objects
#'
#' @param x ti_tbl
#'
#' @returns tibble
#'
wtd_fn <- function(x){

  full_dbi <-  create_calendar(x) |>
    dplyr::mutate(
      year=lubridate::year(date)
      ,month=lubridate::month(date)
      ,week=sql("DATE_PART('week',date)")
      ,.before = 1
    )



  out_dbi <- full_dbi |>
    dbplyr::window_order(date) |>
    dplyr::mutate(
      !!x@value@new_column_name:=cumsum(!!x@value@value_quo)
      ,.by=c(year,month,week,!!!x@calendar@group_quo)
    )

  return(out_dbi)

}




#' Previous month-to-date for tibble objects
#'
#' @param x ti_tbl
#'
#' @returns tibble
#'
pwtd_fn <- function(x){

  # create calendar table

  full_dbi <-  create_calendar(x) |>
    dplyr::mutate(
      year=lubridate::year(date)
      ,month=lubridate::month(date)
      ,week=sql("DATE_PART('week',date)")
      ,.before = 1
    )


  # create lag table
  lag_dbi <- full_dbi|>
    dbplyr::window_order(date) |>
    dplyr::mutate(
      date_lag=as.Date(sql("date + INTERVAL '1 weeks' * 1"))
      ,!!x@value@new_column_name:=cumsum(!!x@value@value_quo)
      ,week_lag=sql("DATE_PART('week',date_lag)")
      ,.by=c(year,month,week,!!!x@calendar@group_quo)
    ) |>
    dplyr::select(-c(date,month,year,week,!!x@value@value_quo))


  # join tables together
  out_dbi <-   dplyr::full_join(
    full_dbi
    ,lag_dbi
    ,by=dplyr::join_by(date==date_lag,!!!x@calendar@group_quo)
  ) |>
    dplyr::select(-c(!!x@value@value_quo)) |>
    dbplyr::window_order(date) |>
    dplyr::group_by(date,year,month,!!!x@calendar@group_quo) |>
    tidyr::fill(date,.direction = "down") |>
    dplyr::ungroup() |>
    dplyr::summarise(
      dplyr::across(dplyr::contains(x@value@value_vec),\(x) sum(x,na.rm=TRUE))
      ,.by=c(date,year,month,!!!x@calendar@group_quo)
    ) |>
    dplyr::filter(
      !is.na(year)
    )

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


#' Week-over-week
#'
#' @param x ti_tbl
#'
#' @returns tibble
#'
wow_fn <- function(x){


  full_dbi <-  create_calendar(x)


  lag_dbi <- full_dbi|>
    dbplyr::window_order(date) |>
    dplyr::mutate(
      date_lag=dplyr::lead(date,n = !!x@fn@lag_n)
      ,!!x@value@new_column_name:=!!x@value@value_quo
      ,.by=c(!!!x@calendar@group_quo)
    ) |>
    dplyr::select(-c(date,!!x@value@value_quo))


  out_dbi <-   dplyr::left_join(
    full_dbi
    ,lag_dbi
    ,by=dplyr::join_by(date==date_lag,!!!x@calendar@group_quo)
  )

  return(out_dbi)

}

#' Year-over-year
#'
#' @param x ti_tbl
#'
#' @returns tibble
#'
wtdopw_fn <- function(x){

  # year-to-date table
  wtd_dbi <-  wtd(.data = x@calendar@data,.date = !!x@calendar@date_quo,.value = !!x@value@value_quo,calendar_type = x@calendar@calendar_type) |>
    calculate()

  #aggregate to prior year

  pw_dbi <-   wow(.data = x@calendar@data,.date = !!x@calendar@date_quo,.value = !!x@value@value_quo,calendar_type = x@calendar@calendar_type,lag_n = x@fn@lag_n) |>
    calculate()

  # join together

  out_dbi <-  wtd_dbi |>
    select(
      -c(!!x@value@value_quo)
    ) |>
    dplyr::left_join(
      pw_dbi |> dplyr::select(
        -c(!!x@value@value_quo)
      )
      ,by=dplyr::join_by(date,!!!x@calendar@group_quo)
    )

  return(out_dbi)

}


## all to date related functions ----------------

#' All-to-date for tibble objects
#'
#' @param x ti_tbl
#'
#' @returns tibble
atd_fn <- function(x){

  full_dbi <-  create_calendar(x)

  out_dbi<- full_dbi |>
    dbplyr::window_order(date) |>
    dplyr::mutate(
      !!x@value@new_column_name:=base::cumsum(!!x@value@value_quo)
      ,.by=c(!!!x@calendar@group_quo)
    )

  return(out_tbl)

}


## day related functions --------------------------

#' Day-over-day
#'
#' @param x ti
#'
#' @returns dbi
#'
dod_fn <- function(x){


  full_dbi <-  create_calendar(x)


  lag_dbi <- full_dbi |>
    dbplyr::window_order(date) |>
    dplyr::mutate(
      date_lag=lead(date,n=!!x@fn@lag_n)
      ,!!x@value@new_column_name:=!!x@value@value_quo
      ,.by=c(!!!x@calendar@group_quo)
    ) |>
    dplyr::select(-c(date,!!x@value@value_quo))

  out_dbi <-   dplyr::left_join(
    full_dbi
    ,lag_dbi
    ,by=dplyr::join_by(date==date_lag,!!!x@calendar@group_quo)
  )

  return(out_dbi)

}


# functions that assign arguments to the ti_tbl class --------------------

## year related ti_tbl----------



#' Year-to-date
#' @name ytd
#' @param .data tibble or dbi object
#' @param .date the date column to group by
#' @param .value the value column to summarize
#' @param calendar_type either 'standard' or '5-5-4' calendar
#' @seealso [fpaR::pytd()], [fpaR::yoy()], [fpaR::yoytd()] are the year-based ti functions
#' @description
#' For each group, `ytd()`  will the running sum of a value based on the calendar date.
#' This function will print out the summary of steps and actions. Use `calculate()` to return the results
#' @details
#' This creates a calendar object with complete dates to ensure no missing days, month, quarter or years.
#' If a grouped object is passed through it will complete the calendar  for each combination of the group.
#' This is part of the time intelligence family of functions.
#' Pass the arguments to  `ytd()` and it will print out its actions and assumptions
#' Use `calculate()` to return the results
#'
#' @returns ti object
#' @export
#'
#' @examples
#' ytd(fpaR::sales,.date=order_date,.value=quantity,calendar_type="standard")
ytd <- function(.data,.date,.value,calendar_type){

  # assigns inputs to ytd_tbl class

  x <- ti(
    calendar(
      data                 = .data
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
      ,lag_n               = NA_integer_
      ,fn_exec             = ytd_fn
    )
  )

  return(x)

}


#' Prevoius year-to-date
#' @name pytd
#' @param .data either a tibble or  DBI object
#' @param .date the date column to aggregate
#' @param .value the value column to summarize
#' @param calendar_type either 'standard' or '5-5-4' calendar
#' @description
#' This calculates the annual cumulative sum of targeted value using a standard or 5-5-4 calendar respecting
#' any groups that are passed through with `dplyr::group_by()`.
#' @details
#' This is part of the time intelligence family of functions.
#' Pass the arguments to  `pytd()` and it will print out its actions and assumptions
#' Use `calculate()` to return the results
#'
#' @returns ti object
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
pqtd <- function(.data,.date,.value,calendar_type,lag_n){


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
      ,new_column_name           = "pqtd"
    )
    ,fn=fn(
      new_date_column_name       = c("year","quarter")
      ,lag_n                     = lag_n
      ,fn_exec                   = qtd_fn
    )
  )


  return(out)
}




#' Previous quarter-to-date
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
      ,new_column_name = c("pqtd")
    )
    ,fn=fn(
      lag_n = lag_n
      ,new_date_column_name = c("year","quarter")
      ,fn_exec=pqtd_fn
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
        ,new_column_name = "mtd"
      )
      ,fn=fn(
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
      ,new_column_name  = "pmtd"
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
    ,time_unit = time_unit("month")
    ,action=action("aggregate")
    ,value=value(
      value_vec = rlang::as_label(rlang::enquo(.value))
      ,new_column_name  = "mom"
    )
    ,fn=fn(
      new_date_column_name = c("date","year","month")
      ,lag_n = lag_n
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
      ,new_column_name = "wtd"
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
      value_vec = rlang::as_label(rlang::enquo(.value))
      ,new_column_name  = "pwtd"
    )
    ,fn=fn(
      new_date_column_name = c("year","month","week")
      ,lag_n = lag_n
      ,fn_exec=pwtd_fn
    )
  )

  return(out)

}

#' Week-to-date over previous week-to-date
#' @name wowtd
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
#' @returns dbi object
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
      ,new_column_name  = "wowtd"
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
      ,new_column_name  = "wtwopw"
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
    ,time_unit = time_unit("week")
    ,action=action(c("aggregate","shift","compare"))
    ,value=value(
      value_vec = rlang::as_label(rlang::enquo(.value))
      ,new_column_name  = "wow"
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
      ,new_column_name  = "atd"
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
    ,action=action(c("aggregate","shift","compare"))
    ,value=value(
      value_vec = rlang::as_label(rlang::enquo(.value))
      ,new_column_name = "dod"
    )
    ,fn=fn(
      new_date_column_name = c("date")
      ,lag_n = lag_n
      ,fn_exec=dod_fn
    )
  )
  return(out)
}
