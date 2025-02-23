
## year related functions

#' Year-to-date execution function
#' @name ytd_fn
#' @param x ti object
#' @description
#' `ytd_fn()` is the function that is called by `ytd()` when passed through to `calculate()`
#' @seealso [ytd()] for the function's class
#' @details
#' This is internal non exported function that is nested in ti class and is called upon when the underlying function is called
#' by `calculated()`.
#' This will return a dbi object that can converted to a tibble object with`dplyr::collect()`
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


#' Previous year-to-date
#' @name pytd_fn
#' @param x ti object
#' @description
#' `pytd_fn()` is the function that is called by `pytd()` when passed through to `calculate()`
#' @seealso [pytd()] for the function's intent
#' @details
#' This is internal non exported function that is nested in ti class and is called upon when the underlying function is called
#' by `calculated()`.
#' This will return a dbi object that can converted to a tibble object with`dplyr::collect()`
#' @returns dbi object
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
#' @name yoytd_fn
#' @param x ti object
#' @description
#' `yoytd_fn()` is the function that is called by `yoytd()` when passed through to `calculate()`
#' @seealso [yoytd()] for the function's intent
#' @details
#' This is internal non exported function that is nested in ti class and is called upon when the underlying function is called
#' by `calculated()`
#' This will return a dbi object that can converted to a tibble object with`dplyr::collect()`
#' @returns dbi object
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
#' @name yoy_fn
#' @param x ti object
#' @description
#' `yoy_fn()` is the function that is called by `yoy()` when passed through to `calculate()`
#' @seealso [yoy()] for the function's intent
#' @details
#' This is internal non exported function that is nested in ti class and is called upon when the underlying function is called
#' by `calculated()`
#' This will return a dbi object that can converted to a tibble object with`dplyr::collect()`
#' @returns dbi object
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

#' Year-to-date over full prior year
#' @name ytdopy_fn
#' @param x ti object
#' @description
#' `ytdopy_fn()` is the function that is called by `ytdopy()` when passed through to `calculate()`
#' @seealso [ytdopy()] for the function's intent
#' @details
#' This is internal non exported function that is nested in ti class and is called upon when the underlying function is called
#' by `calculated()`
#' This will return a dbi object that can converted to a tibble object with`dplyr::collect()`
#' @returns dbi object
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

#' Quarter-to-date execution function
#' @name qtd_fn
#' @param x ti object
#' @description
#' `qtd_fn()` is the function that is called by `qtd()` when passed through to `calculate()`
#' @seealso [qtd()] for the function's intent
#' @details
#' This is internal non exported function that is nested in ti class and is called upon when the underlying function is called
#' by `calculated()`
#' This will return a dbi object that can converted to a tibble object with`dplyr::collect()`
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
      !!x@value@new_column_name:=base::cumsum(!!x@value@value_quo)
      ,.by=c(year,quarter,!!!x@calendar@group_quo)
    ) |>
    dplyr::ungroup()

  return(out_dbi)

}

#' Previous quarter-to-date for tibble objects
#' @name pqtd_fn
#' @param x ti object
#' @description
#' `pqtd_fn()` is the function that is called by `pqtd()` when passed through to `calculate()`
#' @seealso [pqtd()] for the function's intent
#' @details
#' This is internal non exported function that is nested in ti class and is called upon when the underlying function is called
#' by `calculated()`
#' This will return a dbi object that can converted to a tibble object with `dplyr::collect()`
#' @returns dbi object
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
#' @name qoqtd_fn
#' @param x ti object
#' @description
#' `qoqtd_fn()` is the function that is called by `qoqtd()` when passed through to `calculate()`
#' @seealso [qoqtd()] for the function's intent
#' @details
#' This is internal non exported function that is nested in ti class and is called upon when the underlying function is called
#' by `calculated()`
#' This will return a dbi object that can converted to a tibble object with `dplyr::collect()`
#' @returns dbi object
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
#' @name qoq_fn
#' @param x ti object
#' @description
#' `qoq_fn()` is the function that is called by `qoq()` when passed through to `calculate()`
#' @seealso [qoq()] for the function's intent
#' @details
#' This is internal non exported function that is nested in ti class and is called upon when the underlying function is called
#' by `calculated()`
#' This will return a dbi object that can converted to a tibble object with `dplyr::collect()`
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



#' quarter-to-date over previous quarter
#' @name qtdopq_fn
#' @param x ti object
#' @description
#' `qtdopq_fn()` is the function that is called by `qtdopq()` when passed through to `calculate()`
#' @seealso [qtdopq()] for the function's intent
#' @details
#' This is internal non exported function that is nested in ti class and is called upon when the underlying function is called
#' by `calculated()`
#' This will return a dbi object that can converted to a tibble object with `dplyr::collect()`
#' @returns dbi object
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

#' Month-to-date execution function
#' @name mtd_fn
#' @param x ti object
#' @description
#' `mtd_fn()` is the function that is called by `mtd()` when passed through to `calculate()`
#' @seealso [mtd()] for the function's intent
#' @details
#' This is internal non exported function that is nested in ti class and is called upon when the underlying function is called
#' by `calculated()`
#' This will return a dbi object that can converted to a tibble object with `dplyr::collect()`
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
    dplyr::group_by(year,month,!!!x@calendar@group_quo) |>
    dplyr::arrange(date,.by_group = TRUE) |>
    dplyr::mutate(
      !!x@value@new_column_name:=base::cumsum(!!x@value@value_quo)
    ) |>
    dplyr::ungroup()

  return(out_dbi)

}



#' Previous month-to-date for tibble objects
#' @name pmtd_fn
#' @param x ti object
#' @description
#' `pmtd_fn()` is the function that is called by `pmtd()` when passed through to `calculate()`
#' @seealso [pmtd()] for the function's intent
#' @details
#' This is internal non exported function that is nested in ti class and is called upon when the underlying function is called
#' by `calculated()`
#' This will return a dbi object that can converted to a tibble object with `dplyr::collect()`
#' @returns dbi object
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
#' @name momtd_fn
#' @param x ti object
#' @description
#' `momtd_fn()` is the function that is called by `momtd()` when passed through to `calculate()`
#' @seealso [momtd()] for the function's intent
#' @details
#' This is internal non exported function that is nested in ti class and is called upon when the underlying function is called
#' by `calculated()`
#' This will return a dbi object that can converted to a tibble object with `dplyr::collect()`
#' @returns dbi object
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
#' @name mom_fn
#' @param x ti object
#' @description
#' `mom_fn()` is the function that is called by `mom()` when passed through to `calculate()`
#' @seealso [mom()] for the function's intent
#' @details
#' This is internal non exported function that is nested in ti class and is called upon when the underlying function is called
#' by `calculated()`
#' This will return a dbi object that can converted to a tibble object with `dplyr::collect()`
#' @returns dbi object
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

#' Month-over-month vs. prior full momth execution function
#' @name mtdopm_fn
#' @param x ti object
#' @description
#' `mtdopm_fn()` is the function that is called by `mtdopm()` when passed through to `calculate()`
#' @seealso [mtdopm()] for the function's intent
#' @details
#' This is internal non exported function that is nested in ti class and is called upon when the underlying function is called
#' by `calculated()`
#' This will return a dbi object that can converted to a tibble object with `dplyr::collect()`
#' @returns dbi object
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

#' Week-to-date execution fucntion
#' @name wtd_fn
#' @param x ti object
#' @description
#' `wtd_fn()` is the function that is called by `wtd()` when passed through to `calculate()`
#' @seealso [wtd()] for the function's intent
#' @details
#' This is internal non exported function that is nested in ti class and is called upon when the underlying function is called
#' by `calculated()`
#' This will return a dbi object that can converted to a tibble object with `dplyr::collect()`
#' @returns dbi object
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
#' @name pwtd_fn
#' @param x ti object
#' @description
#' `pwtd_fn()` is the function that is called by `pwtd()` when passed through to `calculate()`
#' @seealso [pwtd()] for the function's intent
#' @details
#' This is internal non exported function that is nested in ti class and is called upon when the underlying function is called
#' by `calculated()`
#' This will return a dbi object that can converted to a tibble object with `dplyr::collect()`
#' @returns dbi object
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
#' @name wowtd_fn
#' @param x ti object
#' @description
#' `wowtd_fn()` is the function that is called by `wowtd()` when passed through to `calculate()`
#' @seealso [wowtd()] for the function's intent
#' @details
#' This is internal non exported function that is nested in ti class and is called upon when the underlying function is called
#' by `calculated()`
#' This will return a dbi object that can converted to a tibble object with `dplyr::collect()`
#' @returns dbi object
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
#' @name wow_fn
#' @param x ti object
#' @description
#' `wow_fn()` is the function that is called by `wow()` when passed through to `calculate()`
#' @seealso [wow()] for the function's intent
#' @details
#' This is internal non exported function that is nested in ti class and is called upon when the underlying function is called
#' by `calculated()`
#' This will return a dbi object that can converted to a tibble object with `dplyr::collect()`
#' @returns dbi object
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
#' @name wtdopw_fn
#' @param x ti object
#' @description
#' `wtdopw_fn()` is the function that is called by `wtdopw()` when passed through to `calculate()`
#' @seealso [wtdopw()] for the function's intent
#' @details
#' This is internal non exported function that is nested in ti class and is called upon when the underlying function is called
#' by `calculated()`
#' This will return a dbi object that can converted to a tibble object with `dplyr::collect()`
#' @returns dbi object
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

#' All-to-date execution function
#' @name atd_fn
#' @param x ti object
#' @description
#' `atd_fn()` is the function that is called by `atd()` when passed through to `calculate()`
#' @seealso [atd()] for the function's intent
#' @details
#' This is internal non exported function that is nested in ti class and is called upon when the underlying function is called
#' by `calculated()`
#' This will return a dbi object that can converted to a tibble object with `dplyr::collect()`
#' @returns dbi object
#'
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
#' @name dod_fn
#' @param x ti object
#' @description
#' `dod_fn()` is the function that is called by `dod()` when passed through to `calculate()`
#' @seealso [dod()] for the function's intent
#' @details
#' This is internal non exported function that is nested in ti class and is called upon when the underlying function is called
#' by `calculated()`
#' This will return a dbi object that can converted to a tibble object with `dplyr::collect()`
#' @returns dbi object
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

#' @title Current period year-to-date
#' @name ytd
#' @param .data tibble or dbi object (either grouped or ungrouped)
#' @param .date the date column to group by
#' @param .value the value column to summarize
#' @param calendar_type select either 'standard' or '5-5-4' calendar, see 'Details' for additional information
#'
#' @description
#' -  For each group, [ytd()]  will create the running annual sum of a value based on the calendar type specified
#' -  The function returns a ti object which prints out the summary of steps and actions that will take to create the calendar table and calculations
#' -  Use [calculate()] to return the results
#' @details
#' -  This function creates a complete calendar object that fills in any missing days, weeks, months, quarters, or years
#' -  If you provide a grouped object with [dplyr::group_by()], it will generate a complete calendar for each group
#' -  The function creates a `ti` object, which pre-processes the data and arguments for further downstream functions
#'
#' **standard calendar**
#' -  The standard calendar splits the year into 12 months (with 28–31 days each) and uses a 7-day week
#' -  It automatically accounts for leap years every four years to match the Gregorian calendar
#'
#' **5-5-4 calendar**
#' -  The 5-5-4 calendar divides the fiscal year into 52 weeks (occasionally 53), organizing each quarter into two 5-week periods and one 4-week period.
#' -  This system is commonly used in retail and financial reporting
#' @family time_intelligence
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


#' @title Previous period year-to-date
#' @name pytd
#' @inheritParams ytd
#' @param lag_n the number of periods to lag
#' @description
#' -  For each group, [pytd()]  will create the running annual sum of a value based on the calendar type for the previous year compared to the current year calendar date
#' -  If no period exists, it will return `NA`
#' -  The function returns a ti object which prints out the summary of steps and actions that will take to create the calendar table and calculations
#' -  Use [calculate()] to return the results
#' @inherit ytd details
#' @family time_intelligence
#' @returns ti object
#' @export
#'
#' @examples
#' pytd(fpaR::sales,date=order_date,.value=quantity,calendar_type="standard",lag_n=1)
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


#' @title Current period year-to-date compared to previous period year-to-date
#' @name yoytd
#' @inheritParams pytd
#' @description
#' -  This calculates the annual cumulative sum of targeted value and compares it with the previous period's annual cumulative to date sum using a standard or 5-5-4 calendar respecting
#' any groups that are passed through with [dplyr::group_by()]
#' -  Use [calculate()] to return the results
#' @inherit ytd details
#' @family time_intelligence
#' @returns ti object
#' @export
#'
#' @examples
#' yoytd(fpaR::sales,.date=order_date,.value=quantity,calendar_type="standard")
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


#' @title Current full period year over previous full period year
#' @name yoy
#' @inheritParams pytd
#' @description
#' -  This calculates the full year value compared to the previous year value respecting
#' any groups that are passed through with [dplyr::group_by()]
#'
#' -  Use [calculate()] to return the results
#' @inherit ytd details
#' @family time_intelligence
#'
#' @returns ti object
#' @export
#'
#' @examples
#' yoy(fpaR::sales,.date=order_date,.value=quantity,calendar_type='standard',lag_n=1)
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




#' @title Current period year-to-date compared to full previous period
#' @name ytdopy
#' @inheritParams pytd
#' @description
#' -  This calculates the full year value compared to the previous year value using a standard or 5-5-4 calendar respecting
#' any groups that are passed through with [dplyr::group_by()]
#'
#' -  Use [calculate()] to return the results
#' @inherit ytd details
#' @family time_intelligence
#' @returns ti object
#' @examples
#' ytdopy(fpaR::sales,.date=order_date,.value=quantity,calendar_type='standard',lag_n=1)
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

#' @title  Current period quarter-to-date
#' @name qtd
#' @inheritParams ytd
#' @description
#' This calculates the full year value compared to the previous year value using a standard or 5-5-4 calendar respecting
#' any groups that are passed through with [dplyr::group_by()]
#'
#' Use [calculate()] to return the results
#' @inherit ytd details
#' @family time_intelligence
#' @returns ti object
#' @export
#'
#' @examples
#' qtd(fpar::sales,.date=order_date,.value=quantity,calendar_type="standard")
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


#' @title Prior period quarter-to-date
#' @name pqtd
#' @inheritParams pytd
#' @description
#' -  This calculates the quarterly cumulative sum of targeted value using a standard or 5-5-4 calendar respecting
#' any groups that are passed through with [dplyr::group_by()]
#'
#' use [calculate()] to return the results
#' @inherit ytd details
#' @family time_intelligence
#' @returns ti object
#' @export
#'
#' @examples
#' qtd(fpar::sales,.date=order_date,.value=quantity,calendar_type="standard")
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


#' @title Current period quarter-to-date compared to previous period quarter-to-date
#' @name qoqtd
#' @inheritParams pytd
#' @description
#' -  This calculates the annual cumulative sum of targeted value using a standard or 5-5-4 calendar respecting
#' any groups that are passed through with [dplyr::group_by()]
#'
#' -  Use [calculate()] to return the results
#' @inherit ytd details
#' @family time_intelligence
#' @returns ti object
#' @export
#'
#' @examples
#' ytd(fpaR::sales,.date=order_date,.value=quantity,calendar_type="standard")
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


#' @title Current period quarter-to-date over previous period quarter
#' @name qtdopq
#' @inheritParams pytd
#' @description
#' A short description...
#' @inherit ytd details
#' @family time_intelligence
#' @returns ti object
#' @export
#'
#' @examples
#' qtdopq(fpaR::sales,.date=order_date,.value=quantity,calendar_type='standard',lag_n=1)
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
      new_date_column_name = c("date","year","quarter")
      ,lag_n=lag_n
      ,fn_exec=qtdopq_fn
    )
  )
  return(out)
}


#' @title Current period quarter over previous period quarter
#' @name qoq
#' @description
#' A short description...
#'
#' @inherit ytd details
#' @family time_intelligence
#' @returns ti object
#' @export
#'
#' @examples
#' qoq(fpaR::sales,.date=order_date,.value=quantity,calendar_type='standard',lag_n=1)
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

#' @title Current period month-to-date
#' @name mtd
#' @inheritParams ytd
#' @description
#' This calculates the monthly cumulative sum of targeted value using a standard or 5-5-4 calendar respecting
#' any groups that are passed through with [dplyr::group_by()]
#'
#' Use [calculate()] to return the results
#' @inherit ytd details
#' @family time_intelligence
#' @returns ti object
#' @export
#' @examples
#' mtd(fpaR::sales,.date=order_date,.value=quantity,calendar_type="standard")
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



#' @title Previous period month-to-date
#' @name pmtd
#' @inheritParams pytd
#' @description
#' This calculates the monthly cumulative sum of targeted value using a standard or 5-5-4 calendar respecting
#' any groups that are passed through with [dplyr::group_by()]
#'
#' Use [calculate()] to return the results
#' @inherit ytd details
#' @family time_intelligence
#' @returns ti object
#' @export
#' @examples
#' pmtd(fpaR::sales,.date=order_date,.value=quantity,calendar_type="standard")
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

#' @title Current period month to date compared to previous period month-to-date
#' @name momtd
#' @inheritParams pytd
#' @description
#' This calculates the monthly cumulative sum of targeted value using a standard or 5-5-4 calendar respecting
#' any groups that are passed through with [dplyr::group_by()]
#'
#' Use [calculate()] to return the results
#' @inherit ytd details
#' @family time_intelligence
#' @returns ti object
#' @export
#' @examples
#' momtd(fpaR::sales,.date=order_date,.value=quantity,calendar_type="standard")
momtd <- function(.data,.date,.value,calendar_type,lag_n){


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



#' Current month-to-date over full previous period month
#' @name mtdopm
#' @inheritParams pytd
#' @description
#' This calculates the monthly cumulative sum of targeted value using a standard or 5-5-4 calendar respecting
#' any groups that are passed through with [dplyr::group_by()]
#'
#' Use [calculate()] to return the results
#' @inherit ytd details
#' @family time_intelligence
#' @returns ti object
#' @export
#'
#' @examples
#' mtdopm(fpaR::sales,.date=order_date,.value=quantity,calendar_type="standard")
mtdopm <- function(.data,.date,.value,calendar_type,lag_n){
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

#' @title Current full period month over previous full period month
#' @name mom
#' @inheritParams pytd
#' @description
#' This calculates the monthly cumulative sum of targeted value using a standard or 5-5-4 calendar respecting
#' any groups that are passed through with [dplyr::group_by()]
#'
#' Use [calculate()] to return the results
#' @inherit ytd details
#' @family time_intelligence
#' @returns ti object
#' @export
#' @examples
#' mom(fpaR::sales,.date=order_date,.value=quantity,calendar_type='standard',lag_n=1)
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


#' @title Current period week-to-date
#' @name wtd
#' @inheritParams ytd
#' @description
#' This calculates the monthly cumulative sum of targeted value using a standard or 5-5-4 calendar respecting
#' any groups that are passed through with [dplyr::group_by()]
#'
#' Use [calculate()] to return the results
#' @inherit ytd details
#' @family time_intelligence
#' @returns ti object
#' @export
#' @examples
#' wtd(fpaR::sales,.date=date,.value=quantity,calendar_type="standard")
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



#' @title Previous period week-to-date
#' @name pwtd
#' @inheritParams pytd
#' @description
#' This calculates the monthly cumulative sum of targeted value using a standard or 5-5-4 calendar respecting
#' any groups that are passed through with [dplyr::group_by()]
#'
#' Use [calculate()] to return the results
#' @inherit ytd details
#' @family time_intelligence
#' @returns ti object
#' @export
#' @examples
#' pwtd(fpaR::sales,.date=order_date,.value=quantity,calendar_type="standard")
pwtd <- function(.data,.date,.value,calendar_type,lag_n){


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




#' @title Current period Wwek-to-date over previous period week-to-date
#' @name wowtd
#' @inheritParams pytd
#' @description
#' This calculates the monthly cumulative sum of targeted value using a standard or 5-5-4 calendar respecting
#' any groups that are passed through with [dplyr::group_by()]
#'
#' Use [calculate()] to return the results
#' @inherit ytd details
#' @family time_intelligence
#' @returns ti object
#' @export
#' @examples
#' wowtd(fpaR::sales,.date=order_date,.value=quantity,calendar_type="standard")
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


#' @title Current period week-to-date over full previous period week
#' @name wtdopw
#' @inheritParams pytd
#' @description
#' This calculates the monthly cumulative sum of targeted value using a standard or 5-5-4 calendar respecting
#' any groups that are passed through with [dplyr::group_by()]
#'
#' Use [calculate()] to return the results
#' @inherit ytd details
#' @family time_intelligence
#' @returns ti object
#' @export
#' @examples
#' wtdopw(fpaR::sales,.date=order_date,.value=quantity,calendar_type="standard")
wtdopw <- function(.data,.date,.value,calendar_type,lag_n){

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


#' @title Current full period week over full previous period week
#' @name wow
#' @inheritParams pytd
#' @description
#' This calculates the monthly cumulative sum of targeted value using a standard or 5-5-4 calendar respecting
#' any groups that are passed through with [dplyr::group_by()]
#'
#' Use [calculate()] to return the results
#' @inherit ytd details
#' @family time_intelligence
#' @returns ti object
#' @export
#' @examples
#' wow(fpaR::sales,.date=order_date,.value=quantity,calendar_type='standard',lag_n=1)
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

#' @title All period-to-date
#' @name atd
#' @inheritParams ytd
#' @description
#' This calculates the monthly cumulative sum of targeted value using a standard or 5-5-4 calendar respecting
#' any groups that are passed through with [dplyr::group_by()]
#'
#' Use [calculate()] to return the results
#' @inherit ytd details
#' @family time_intelligence
#' @returns ti object
#' @export
#' @examples
#' atd(fpaR::sales,.date=order_date,.value=quantity,calendar_type="standard")
atd <- function(.data,.date,.value,calendar_type){

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


## Day related functions------------------

#' @title Current period day over previous period day
#' @name dod
#' @inheritParams pytd
#' @description
#' This calculates the monthly cumulative sum of targeted value using a standard or 5-5-4 calendar respecting
#' any groups that are passed through with [dplyr::group_by()]
#'
#' Use [calculate()] to return the results
#' @inherit ytd details
#' @family time_intelligence
#' @returns ti object
#' @export
#' @examples
#' dod(fpaR::sales,.date=order_date,.value=quantity,calendar_type='standard',lag_n=1)
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
