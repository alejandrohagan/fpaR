
## year related functions---------------

#' Year-to-date execution function
#' @name ytd_fn
#' @param x ti object
#' @description
#' [ytd_fn()] is the function that is called by [ytd()] when passed through to [calculate]
#' @seealso [ytd()] for the function's class
#' @details
#' This is internal non exported function that is nested in ti class and is called upon when the underlying function is called
#' by [calculate]
#' This will return a dbi object that can converted to a tibble object with [dplyr::collect()]
#' @returns dbi object
#'
ytd_fn <- function(x){


  # create calendar table

  full_dbi <- create_calendar(x) |>
    dplyr::mutate(
      year=lubridate::year(date)
    )

 # aggregate the data and create the cumulative sum

  out_dbi <- full_dbi |>
    dbplyr::window_order(date) |>
    dplyr::mutate(
      !!x@value@new_column_name_vec:=base::cumsum(!!x@value@value_quo)
      ,.by=c(year,!!!x@data@group_quo)
    ) |>
    dplyr::relocate(
      date,year
    )

  return(out_dbi)
}


#' @title Previous year-to-date execution function
#' @name pytd_fn
#' @param x ti object
#' @description
#' [pytd_fn()] is the function that is called by [pytd()] when passed through to [calculate]
#' @seealso [pytd()] for the function's intent
#' @details
#' This is internal non exported function that is nested in ti class and is called upon when the underlying function is called
#' by [calculate]
#' This will return a dbi object that can converted to a tibble object with [dplyr::collect()]
#' @returns dbi object
#'
pytd_fn <- function(x){


  # x <- fpaR::pytd(.data =sales ,.date = order_date,.value = margin,calendar_type = "standard",lag_n = 2)


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
      ,!!x@value@new_column_name_vec:=cumsum(!!x@value@value_quo)
      ,.by=c(year,!!!x@data@group_quo)
    ) |>
    dplyr::select(-c(date,year,!!x@value@value_quo))


  # join tables together
  out_tbl <-   dplyr::full_join(
    full_dbi
    ,lag_dbi
    ,by=dplyr::join_by(date==date_lag,!!!x@data@group_quo)
  ) |>
    dplyr::select(-c(!!x@value@value_quo)) |>
    dbplyr::window_order(date) |>
    dplyr::group_by(date,year,!!!x@data@group_quo) |>
    tidyr::fill(date,.direction = "down") |>
    dplyr::ungroup() |>
    dplyr::summarise(
      dplyr::across(dplyr::contains(x@value@value_vec),\(x) sum(x,na.rm=TRUE))
      ,.by=c(date,year,!!!x@data@group_quo)
    )

  return(out_tbl)

}


#' @title Current year to date over previous year-to-date exection function
#' @name yoytd_fn
#' @param x ti object
#' @description
#' [yoytd_fn()] is the function that is called by [yoytd()] when passed through to [calculate]
#' @seealso [yoytd()] for the function's intent
#' @details
#' This is internal non exported function that is nested in ti class and is called upon when the underlying function is called
#' by [calculate]
#' This will return a dbi object that can converted to a tibble object with [dplyr::collect()]
#' @returns dbi object
#'
yoytd_fn <- function(x){



  # ytd table

  ytd_dbi <- ytd(.data=x@data@data,.date=!!x@data@date_quo,.value = !!x@value@value_quo,calendar_type = x@data@data_type) |>
    ytd_fn()

  #pytd table

  pytd_dbi <- pytd(.data=x@data@data,.date=!!x@data@date_quo,.value = !!x@value@value_quo,calendar_type = x@data@data_type,lag_n = x@fn@lag_n) |>
    pytd_fn()

  # join tables together

  out_dbi <-   dplyr::full_join(
    ytd_dbi
    ,pytd_dbi
    ,by=dplyr::join_by(date==date,year,!!!x@data@group_quo)
  ) |>
    dplyr::group_by(date,!!!x@data@group_quo) |>
    tidyr::fill(date,.direction = "down") |>
    dplyr::ungroup() |>
    dplyr::summarise(
      dplyr::across(dplyr::contains(x@value@value_vec),\(x) sum(x,na.rm=TRUE))
      ,.by=c(date,!!!x@data@group_quo)
    )

  return(out_dbi)

}


#' @title Current year-to-date over previous period year-to-date eeecution function
#' @name yoy_fn
#' @param x ti object
#' @description
#' [yoy_fn()] is the function that is called by [yoy()] when passed through to [calculate]
#' @seealso [yoy()] for the function's intent
#' @details
#' This is internal non exported function that is nested in ti class and is called upon when the underlying function is called
#' by [calculate]
#' This will return a dbi object that can converted to a tibble object with [dplyr::collect()]
#' @returns dbi object
#'
yoy_fn <- function(x){

  # create calendar
  full_dbi <-  create_calendar(x)

  # create lag
  lag_dbi <- full_dbi |>
    dbplyr::window_order(date,!!!x@data@group_quo) |>
    dplyr::mutate(
      date_lag=dplyr::lead(date,n = !!x@fn@lag_n)
      ,!!x@value@new_column_name_vec:=!!x@value@value_quo
      ,days_in_current_period=sql("day(last_day(date))")
      , days_in_previous_period=sql("day(last_day(date_lag))")
      ,.by=c(!!!x@data@group_quo)
    ) |>
    dplyr::select(-c(date,!!x@value@value_quo))

  # bring tables together
  out_dbi <-   dplyr::left_join(
    full_dbi
    ,lag_dbi
    ,by=dplyr::join_by(date==date_lag,!!!x@data@group_quo,missing_date_indicator)
  ) |>
    dplyr::mutate(
      year=lubridate::year(date)
      ,.after=date
    ) |>
    dplyr::mutate(
      month=lubridate::month(date)
    )

  return(out_dbi)

}

#' Year-to-date over full prior period year
#' @name ytdopy_fn
#' @param x ti object
#' @description
#' [ytdopy_fn()] is the function that is called by [ytdopy()] when passed through to [calculate]
#' @seealso [ytdopy()] for the function's intent
#' @details
#' This is internal non exported function that is nested in ti class and is called upon when the underlying function is called
#' by [calculate]
#' This will return a dbi object that can converted to a tibble object with[dplyr::collect()]
#' @export
#' @returns dbi object
#'
ytdopy_fn <- function(x){

  # year-to-date table
  ytd_dbi <-  ytd(.data = x@data@data,.date = !!x@data@date_quo,.value = !!x@value@value_quo,calendar_type = x@data@data_type) |>
    calculate()

  #aggregate to prior year

  py_dbi <-   yoy(.data = x@data@data,.date = !!x@data@date_quo,.value = !!x@value@value_quo,calendar_type = x@data@data_type,lag_n = x@fn@lag_n) |>
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
      ,by=dplyr::join_by(year,date,!!!x@data@group_quo)
    )

  return(out_dbi)
}


## quarter related functions -----------------

#' Quarter-to-date execution function
#' @name qtd_fn
#' @param x ti object
#' @description
#' [qtd_fn()] is the function that is called by [qtd()] when passed through to [calculate]
#' @seealso [qtd()] for the function's intent
#' @details
#' This is internal non exported function that is nested in ti class and is called upon when the underlying function is called
#' by [calculate]
#' This will return a dbi object that can converted to a tibble object with [dplyr::collect()]
#' @returns dbi object
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
      !!x@value@new_column_name_vec:=base::cumsum(!!x@value@value_quo)
      ,.by=c(year,quarter,!!!x@data@group_quo)
    ) |>
    dplyr::ungroup()

  return(out_dbi)

}

#' Previous quarter-to-date for tibble objects
#' @name pqtd_fn
#' @param x ti object
#' @description
#' [pqtd_fn()] is the function that is called by [pqtd()] when passed through to [calculate]
#' @seealso [pqtd()] for the function's intent
#' @details
#' This is internal non exported function that is nested in ti class and is called upon when the underlying function is called
#' by [calculate]
#' This will return a dbi object that can converted to a tibble object with [dplyr::collect()]
#' @returns dbi object
#'
pqtd_fn <- function(x){


 lag_n_vec <-  x@fn@lag_n |> rlang::as_label()

  # create calendar table

  full_dbi <-  create_calendar(x) |>
    dplyr::mutate(
      year=lubridate::year(date)
      ,quarter=lubridate::quarter(date)
      ,.before = 1
    )

  lag_dbi <- full_dbi |>
    dbplyr::window_order(date,quarter,year) |>
    dplyr::mutate(
      date_lag=as.Date(dplyr::sql(glue::glue("date + INTERVAL '3 months' * {lag_n_vec}")))
    ) |>
    dplyr::mutate(
      !!x@value@new_column_name_vec:=cumsum(!!x@value@value_quo)
      ,.by=c(year,quarter,!!!x@data@group_quo)
    ) |>
    dplyr::select(-c(date,year,quarter,!!x@value@value_quo))


  # join tables together
  out_dbi <-   dplyr::full_join(
    full_dbi
    ,lag_dbi
    ,by=dplyr::join_by(date==date_lag,!!!x@data@group_quo)
  ) |>
    dplyr::select(-c(!!x@value@value_quo)) |>
    dbplyr::window_order(date,year,quarter,!!!x@data@group_quo) |>
    dplyr::group_by(date,year,quarter,!!!x@data@group_quo) |>
    tidyr::fill(date,.direction = "down") |>
    dplyr::ungroup() |>
    dplyr::summarise(
      dplyr::across(dplyr::contains(x@value@value_vec),\(x) sum(x,na.rm=TRUE))
      ,.by=c(date,year,quarter,!!!x@data@group_quo)
    ) |>
    dplyr::filter(
      !is.na(year)
    )

  return(out_dbi)

}


#' Current quarter to date over previous quarter-to-date for tibble objects
#' @name qoqtd_fn
#' @param x ti object
#' @description
#' [qoqtd_fn()] is the function that is called by [qoqtd()] when passed through to [calculate]
#' @seealso [qoqtd()] for the function's intent
#' @details
#' This is internal non exported function that is nested in ti class and is called upon when the underlying function is called
#' by [calculate]
#' This will return a dbi object that can converted to a tibble object with [dplyr::collect()]
#' @returns dbi object
#'
qoqtd_fn <- function(x){

  # ytd table

qtd_dbi <- qtd(.data=x@data@data,.date=!!x@data@date_quo,.value = !!x@value@value_quo,calendar_type = x@data@data_type) |>
  qtd_fn()

  # pytd table

  pqtd_dbi <- pqtd(.data=x@data@data,.date=!!x@data@date_quo,.value = !!x@value@value_quo,calendar_type = x@data@data_type,lag_n = x@fn@lag_n) |>
    pqtd_fn()

  # join tables together

  out_dbi <-   dplyr::full_join(
    qtd_dbi
    ,pqtd_dbi
    ,by=dplyr::join_by(date==date,year,quarter,!!!x@data@group_quo)
  ) |>
    dplyr::group_by(date,year,quarter,!!!x@data@group_quo) |>
    tidyr::fill(date,.direction = "down") |>
    dplyr::ungroup() |>
    dplyr::summarise(
      dplyr::across(dplyr::contains(x@value@value_vec),\(x) sum(x,na.rm=TRUE))
      ,.by=c(date,year,quarter,!!!x@data@group_quo)
    ) |>
    dplyr::filter(
      !is.na(year)
    )

  return(out_dbi)

}


#' Quarter-over-quarter execution function
#' @name qoq_fn
#' @param x ti object
#' @description
#' [qoq_fn()] is the function that is called by [qoq()] when passed through to [calculate]
#' @seealso [qoq()] for the function's intent
#' @details
#' This is internal non exported function that is nested in ti class and is called upon when the underlying function is called
#' by [calculate]
#' This will return a dbi object that can converted to a tibble object with [dplyr::collect()]
#' @returns dbi object
#'
qoq_fn <- function(x){

  # create calendar
  full_dbi <-  fpaR::create_calendar(x)

  # create lag
  lag_dbi <- full_dbi |>
    dbplyr::window_order(date,!!!x@data@group_quo) |>
    dplyr::mutate(
      date_lag=dplyr::lead(date,n = !!x@fn@lag_n)
      ,!!x@value@new_column_name_vec:=!!x@value@value_quo
      ,.by=c(!!!x@data@group_quo)
    ) |>
    dplyr::select(-c(date,!!x@value@value_quo))

  # bring tables together
  out_dbi <-   dplyr::left_join(
    full_dbi
    ,lag_dbi
    ,by=dplyr::join_by(date==date_lag,!!!x@data@group_quo)
  ) |>
    dplyr::mutate(
      year=lubridate::year(date)
      ,quarter=lubridate::quarter(date)
    ) |>
    dplyr::relocate(date,year,quarter)

  return(out_dbi)
}



#' quarter-to-date over previous quarter execution function
#' @name qtdopq_fn
#' @param x ti object
#' @description
#' [qtdopq_fn()] is the function that is called by [qtdopq()] when passed through to [calculate]
#' @seealso [qtdopq()] for the function's intent
#' @details
#' This is internal non exported function that is nested in ti class and is called upon when the underlying function is called
#' by [calculate]
#' This will return a dbi object that can converted to a tibble object with [dplyr::collect()]
#' @returns dbi object
#'
qtdopq_fn <- function(x){

  # year-to-date table

  qtd_dbi <-  qtd(.data = x@data@data,.date = !!x@data@date_quo,.value = !!x@value@value_quo,calendar_type = x@data@data_type) |>
    calculate()

  qoq_dbi <-  qoq(.data = x@data@data,.date = !!x@data@date_quo,.value = !!x@value@value_quo,calendar_type = x@data@data_type,lag_n = x@fn@lag_n) |>
    calculate()

  # join together

  out_dbi <-  qtd_dbi |>
    dplyr::left_join(
      qoq_dbi
      ,by=dplyr::join_by(year,quater,!!!x@data@group_quo)
    )

  return(out_dbi)
}

## month related functions -------------------------

#' Month-to-date execution function
#' @name mtd_fn
#' @param x ti object
#' @description
#' [mtd_fn()] is the function that is called by [mtd()] when passed through to [calculate]
#' @seealso [mtd()] for the function's intent
#' @details
#' This is internal non exported function that is nested in ti class and is called upon when the underlying function is called
#' by [calculate]
#' This will return a dbi object that can converted to a tibble object with [dplyr::collect()]
#' @returns dbi object
#'
mtd_fn <- function(x){


  full_dbi <-  create_calendar(x) |>
    dplyr::mutate(
      year=lubridate::year(date)
      ,month=lubridate::month(date)
      ,.before = 1
    )

  out_dbi <- full_dbi |>
    dbplyr::window_order(date) |>
    dplyr::mutate(
      !!x@value@new_column_name_vec:=base::cumsum(!!x@value@value_quo)
      ,.by=c(year,month,!!!x@data@group_quo)
    ) |>
    mutate(
      days_in_current_period=lubridate::day(date)
    )

  return(out_dbi)

}



#' Previous month-to-date execution function
#' @name pmtd_fn
#' @param x ti object
#' @description
#' [pmtd_fn()] is the function that is called by [pmtd()] when passed through to [calculate]
#' @seealso [pmtd()] for the function's intent
#' @details
#' This is internal non exported function that is nested in ti class and is called upon when the underlying function is called
#' by [calculate]
#' This will return a dbi object that can converted to a tibble object with [dplyr::collect()]
#' @returns dbi object
#'
pmtd_fn <- function(x){


 lag_n_vec <-  x@fn@lag_n |> rlang::as_label()
  # create calendar table

  full_dbi <-  create_calendar(x) |>
    dplyr::mutate(
      year=lubridate::year(date)
      ,month=lubridate::month(date)
      ,.before = 1
    ) |>
    select(-c(missing_date_indicator))

  # create lag table
  lag_dbi <- full_dbi |>
    dbplyr::window_order(date) |>
    dplyr::mutate(
      date_lag=as.Date(dplyr::sql(glue::glue("date + INTERVAL '1 months' * {lag_n_vec}")))
      ,!!x@value@new_column_name_vec:=cumsum(!!x@value@value_quo)
      ,.by=c(year,month,!!!x@data@group_quo)
    ) |>
    dplyr::select(-c(month,year,!!x@value@value_quo)) |>
    dplyr::mutate(
      month=lubridate::month(date_lag)
      ,year=lubridate::year(date_lag)
    ) |>
    mutate(
      days_in_comparison_period=lubridate::day(date)
    ) |>
    dplyr::select(-c(year,month,date))

  # join tables together
  out_dbi <-  dplyr::full_join(
    full_dbi
    ,lag_dbi
    ,by=dplyr::join_by(date==date_lag,!!!x@data@group_quo)
  ) |>
    dplyr::select(-c(!!x@value@value_quo)) |>
    dbplyr::window_order(date,!!!x@data@group_quo) |>
    tidyr::fill(c(days_in_comparison_period,!!x@value@new_column_name_quo),.direction = "down") |>
    dplyr::summarise(
      !!x@value@new_column_name_vec:=max(!!x@value@new_column_name_quo,na.rm=TRUE)
      ,days_in_comparison_period=max(days_in_comparison_period,na.rm=TRUE)
      ,.by=c(date,year,month,!!!x@data@group_quo)
    )


  return(out_dbi)


}


#' Current year to date over previous year-to-date for tibble objects
#' @name momtd_fn
#' @param x ti object
#' @description
#' [momtd_fn()] is the function that is called by [momtd()] when passed through to [calculate]
#' @seealso [momtd()] for the function's intent
#' @details
#' This is internal non exported function that is nested in ti class and is called upon when the underlying function is called
#' by [calculate]
#' This will return a dbi object that can converted to a tibble object with [dplyr::collect()]
#' @returns dbi object
#'
momtd_fn <- function(x){

  # mtd table

  mtd_dbi <- mtd(.data = x@data@data,.date = !!x@data@date_quo,.value = !!x@value@value_quo,calendar_type = x@data@data_type) |>
    calculate()

  # pmtd table

  pmtd_dbi <- pmtd(.data = x@data@data,.date = !!x@data@date_quo,.value = !!x@value@value_quo,calendar_type = x@data@data_type,lag_n = x@fn@lag_n) |>
    calculate()

  # join tables together

  out_dbi <-   dplyr::full_join(
    mtd_dbi
    ,pmtd_dbi
    ,by=dplyr::join_by(date==date,year,month,!!!x@data@group_quo,missing_date_indicator)
  ) |>
    dplyr::summarise(
      dplyr::across(dplyr::contains(x@value@value_vec),\(x) sum(x,na.rm=TRUE))
      ,.by=c(missing_date_indicator,date,year,month,!!!x@data@group_quo,pp_missing_dates_cnt,pp_extra_dates_cnt)
    )
  return(out_dbi)

}


#' month-over-month execution function
#' @name mom_fn
#' @param x ti object
#' @description
#' [mom_fn()] is the function that is called by [mom()] when passed through to [calculate]
#' @seealso [mom()] for the function's intent
#' @details
#' This is internal non exported function that is nested in ti class and is called upon when the underlying function is called
#' by [calculate]
#' This will return a dbi object that can converted to a tibble object with [dplyr::collect()]
#' @returns dbi object
#'
mom_fn <- function(x){

  full_dbi <-  create_calendar(x)

  # create lag
  lag_dbi <- full_dbi |>
    dbplyr::window_order(date,!!!x@data@group_quo) |>
    dplyr::mutate(
      date_lag=dplyr::lead(date,n = !!x@fn@lag_n)
      ,!!x@value@new_column_name_vec:=!!x@value@value_quo
      ,days_in_current_period=sql("day(last_day(date))")
      ,days_in_previous_period=sql("day(last_day(date_lag))")
      ,.by=c(!!!x@data@group_quo)
    ) |>
    dplyr::select(-c(date,!!x@value@value_quo)) |>
    dplyr::filter(!is.na(date_lag))



  # bring tables together
  out_dbi <-   dplyr::left_join(
    full_dbi
    ,lag_dbi
    ,by=dplyr::join_by(date==date_lag,!!!x@data@group_quo,missing_date_indicator)
  ) |>
    dplyr::mutate(
      year=lubridate::year(date)
      ,month=lubridate::month(date)
      ,.after=date
    )

  return(out_dbi)

}

#' Month-over-month vs. prior full momth execution function
#' @name mtdopm_fn
#' @param x ti object
#' @description
#' [mtdopm_fn()] is the function that is called by [mtdopm()] when passed through to [calculate]
#' @seealso [mtdopm()] for the function's intent
#' @details
#' This is internal non exported function that is nested in ti class and is called upon when the underlying function is called
#' by [calculate]
#' This will return a dbi object that can converted to a tibble object with [dplyr::collect()]
#' @returns dbi object
#'
mtdopm_fn <- function(x){


  # year-to-date table
  mtd_dbi <-  mtd(.data = x@data@data,.date = !!x@data@date_quo,.value = !!x@value@value_quo,calendar_type = x@data@data_type) |>
    calculate()

  #aggregate to prior year

  pm_dbi <-   mom(.data = x@data@data,.date = !!x@data@date_quo,.value = !!x@value@value_quo,calendar_type = x@data@data_type,lag_n = x@fn@lag_n) |>
    calculate()

  # join together

 out_dbi <-  mtd_dbi |>
   dplyr::select(
     -c(!!x@value@value_quo)
   ) |>
    dplyr::left_join(
      pm_dbi |>
        dplyr::select(
        -c(!!x@value@value_quo)
      )
      ,by=dplyr::join_by(year,month,date,!!!x@data@group_quo,missing_date_indicator)
    )

  return(out_dbi)


}




## week related functions-----------------

#' Week-to-date execution fucntion
#' @name wtd_fn
#' @param x ti object
#' @description
#' [wtd_fn()] is the function that is called by [wtd()] when passed through to [calculate]
#' @seealso [wtd()] for the function's intent
#' @details
#' This is internal non exported function that is nested in ti class and is called upon when the underlying function is called
#' by [calculate]
#' This will return a dbi object that can converted to a tibble object with [dplyr::collect()]
#' @returns dbi object
#'
wtd_fn <- function(x){

  full_dbi <-  create_calendar(x) |>
    dplyr::mutate(
      year=lubridate::year(date)
      ,month=lubridate::month(date)
      ,week=dplyr::sql("DATE_PART('week',date)")
      ,.before = 1
    )



  out_dbi <- full_dbi |>
    dbplyr::window_order(date) |>
    dplyr::mutate(
      !!x@value@new_column_name_vec:=cumsum(!!x@value@value_quo)
      ,.by=c(year,month,week,!!!x@data@group_quo)
    )

  return(out_dbi)

}





#' Previous month-to-date for tibble objects
#' @name pwtd_fn
#' @param x ti object
#' @description
#' [pwtd_fn()] is the function that is called by [pwtd()] when passed through to [calculate]
#' @seealso [pwtd()] for the function's intent
#' @details
#' This is internal non exported function that is nested in ti class and is called upon when the underlying function is called
#' by [calculate]
#' This will return a dbi object that can converted to a tibble object with [dplyr::collect()]
#' @returns dbi object
#'
pwtd_fn <- function(x){

 lag_n_vec <-  x@fn@lag_n |> rlang::as_label()

  # create calendar table

  full_dbi <-  create_calendar(x) |>
    dplyr::mutate(
      year=lubridate::year(date)
      ,month=lubridate::month(date)
      ,week=dplyr::sql("DATE_PART('week',date)")
      ,.before = 1
    )


  # create lag table
  lag_dbi <- full_dbi|>
    dbplyr::window_order(date) |>
    dplyr::mutate(
      date_lag=as.Date(dplyr::sql(glue::glue("date + INTERVAL '1 weeks' * {lag_n_vec}")))
      ,!!x@value@new_column_name_vec:=cumsum(!!x@value@value_quo)
      ,week_lag=dplyr::sql("DATE_PART('week',date_lag)")
      ,.by=c(year,month,week,!!!x@data@group_quo)
    ) |>
    dplyr::select(-c(date,month,year,week,!!x@value@value_quo))


  # join tables together
  out_dbi <-   dplyr::full_join(
    full_dbi
    ,lag_dbi
    ,by=dplyr::join_by(date==date_lag,!!!x@data@group_quo)
  ) |>
    dplyr::select(-c(!!x@value@value_quo)) |>
    dbplyr::window_order(date) |>
    dplyr::group_by(date,year,month,!!!x@data@group_quo) |>
    tidyr::fill(date,.direction = "down") |>
    dplyr::ungroup() |>
    dplyr::summarise(
      dplyr::across(dplyr::contains(x@value@value_vec),\(x) sum(x,na.rm=TRUE))
      ,.by=c(date,year,month,!!!x@data@group_quo)
    ) |>
    dplyr::filter(
      !is.na(year)
    )

  return(out_tbl)

}


#' Current year to date over previous year-to-date for tibble objects
#' @name wowtd_fn
#' @param x ti object
#' @description
#' [wowtd_fn()] is the function that is called by [wowtd()] when passed through to [calculate]
#' @seealso [wowtd()] for the function's intent
#' @details
#' This is internal non exported function that is nested in ti class and is called upon when the underlying function is called
#' by [calculate]
#' This will return a dbi object that can converted to a tibble object with [dplyr::collect()]
#' @returns dbi object
#'
wowtd_fn <- function(x){

  # ytd table

  wtd_tbl <- wtd_tbl(x)

  #pytd table

  pwtd_tbl <- pwtd_tbl(x) |>
    dplyr::rename(
      !!x@value@second_column_name:=!!x@value@new_column_name_quo
    )

  # join tables together

  out_tbl <-   dplyr::left_join(
    wtd_tbl
    ,pwtd_tbl
    ,by=dplyr::join_by(date,year,month,week,!!!x@data@group_quo)
  )

  return(out_tbl)

}


#' Week-over-week execution function
#' @name wow_fn
#' @param x ti object
#' @description
#' [wow_fn()] is the function that is called by [wow()] when passed through to [calculate]
#' @seealso [wow()] for the function's intent
#' @details
#' This is internal non exported function that is nested in ti class and is called upon when the underlying function is called
#' by [calculate]
#' This will return a dbi object that can converted to a tibble object with [dplyr::collect()]
#' @returns dbi object
#'
wow_fn <- function(x){


  full_dbi <-  create_calendar(x)


  lag_dbi <- full_dbi|>
    dbplyr::window_order(date) |>
    dplyr::mutate(
      date_lag=dplyr::lead(date,n = !!x@fn@lag_n)
      ,!!x@value@new_column_name_vec:=!!x@value@value_quo
      ,.by=c(!!!x@data@group_quo)
    ) |>
    dplyr::select(-c(date,!!x@value@value_quo))


  out_dbi <-   dplyr::left_join(
    full_dbi
    ,lag_dbi
    ,by=dplyr::join_by(date==date_lag,!!!x@data@group_quo)
  )

  return(out_dbi)

}

#' Year-over-year
#' @name wtdopw_fn
#' @param x ti object
#' @description
#' [wtdopw_fn()] is the function that is called by [wtdopw()] when passed through to [calculate]
#' @seealso [wtdopw()] for the function's intent
#' @details
#' This is internal non exported function that is nested in ti class and is called upon when the underlying function is called
#' by [calculate]
#' This will return a dbi object that can converted to a tibble object with [dplyr::collect()]
#' @returns dbi object
#'
wtdopw_fn <- function(x){

  # year-to-date table
  wtd_dbi <-  wtd(.data = x@data@data,.date = !!x@data@date_quo,.value = !!x@value@value_quo,calendar_type = x@data@data_type) |>
    calculate()

  #aggregate to prior year

  pw_dbi <-   wow(.data = x@data@data,.date = !!x@data@date_quo,.value = !!x@value@value_quo,calendar_type = x@data@data_type,lag_n = x@fn@lag_n) |>
    calculate()

  # join together

  out_dbi <-  wtd_dbi |>
    dplyr::select(
      -c(!!x@value@value_quo)
    ) |>
    dplyr::left_join(
      pw_dbi |>
        dplyr::select(
        -c(!!x@value@value_quo)
      )
      ,by=dplyr::join_by(date,!!!x@data@group_quo)
    )

  return(out_dbi)

}


## all to date related functions ----------------

#' All-to-date execution function
#' @name atd_fn
#' @param x ti object
#' @description
#' [atd_fn()] is the function that is called by [atd()] when passed through to [calculate]
#' @seealso [atd()] for the function's intent
#' @details
#' This is internal non exported function that is nested in ti class and is called upon when the underlying function is called
#' by [calculate]
#' This will return a dbi object that can converted to a tibble object with [dplyr::collect()]
#' @returns dbi object
#'
atd_fn <- function(x){

  full_dbi <-  create_calendar(x)

  out_dbi<- full_dbi |>
    dbplyr::window_order(date) |>
    dplyr::mutate(
      !!x@value@new_column_name_vec:=base::cumsum(!!x@value@value_quo)
      ,.by=c(!!!x@data@group_quo)
    )
  return(out_tbl)
}


## day related functions --------------------------

#' Day-over-day execution function
#' @name dod_fn
#' @param x ti object
#' @description
#' [dod_fn()] is the function that is called by [dod()] when passed through to [calculate]
#' @seealso [dod()] for the function's intent
#' @details
#' This is internal non exported function that is nested in ti class and is called upon when the underlying function is called
#'by [calculate]
#' This will return a dbi object that can converted to a tibble object with [dplyr::collect()]
#' @returns dbi object
#'
dod_fn <- function(x){


  full_dbi <-  create_calendar(x)


  lag_dbi <- full_dbi |>
    dbplyr::window_order(date) |>
    dplyr::mutate(
      date_lag=dplyr::lead(date,n=!!x@fn@lag_n)
      ,!!x@value@new_column_name_vec:=!!x@value@value_quo
      ,.by=c(!!!x@data@group_quo)
    ) |>
    dplyr::select(-c(date,!!x@value@value_quo))

  out_dbi <-   dplyr::left_join(
    full_dbi
    ,lag_dbi
    ,by=dplyr::join_by(date==date_lag,!!!x@data@group_quo)
  )

  return(out_dbi)

}
