create_calendar <- new_generic("create_calendar","x")

calculate <- new_generic("calculate","x")


#' Title
#'
#' @param x
#'
#' @returns
#' @export
#'
#' @examples
method(create_calendar,ti_tbl) <- function(x){

  summary_tbl <- x@calendar_tbl@data |>
    dplyr::mutate(
      date = lubridate::floor_date(!!x@calendar_tbl@date_quo,unit = x@time_unit@value)
      ,time_unit=x@time_unit@value
    ) |>
    dplyr::group_by(date,.add=TRUE) |>
    dplyr::summarise(
      !!x@value_vec:= sum(!!x@value_quo,na.rm=TRUE)
      ,.groups = "drop"
    )

  calendar_tbl <- tibble::tibble(
    date = base::seq.Date(from = min(summary_tbl$date), to = max(summary_tbl$date), by = x@time_unit@value)
  )

  # Create a calendar table with all the dates in the specified time frame
  if(x@calendar_tbl@group_indicator){

    calendar_tbl <- dplyr::left_join(

      summary_tbl |> dplyr::distinct(!!!x@calendar_tbl@group_quo) |> dplyr::mutate(id="id")

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
    ,by = dplyr::join_by(date,!!!x@calendar_tbl@group_quo)
  ) |>
    dplyr::mutate(
      # dplyr::across(dplyr::where(\(x) base::is.numeric(x)),\(x) tidyr::replace_na(x,0))
      !!x@value_vec:= dplyr::coalesce(!!x@value_quo, 0)
    ) |>
    dplyr::arrange(!!!x@calendar_tbl@group_quo,date)

  return(full_tbl)
}



#' Title
#'
#' @param x ytd_tbl object
#'
#' @returns
#' @export
#' @examples
method(calculate,ytd_tbl) <- function(x){

  # Aggregate data based on provided time unit

  full_tbl <- create_calendar(x) |>
    dplyr::mutate(
      year=lubridate::year(date)
      ,.before = 1
    )



  out_tbl <- full_tbl |>
    dplyr::group_by(year,!!!x@calendar_tbl@group_quo) |>
    dplyr::arrange(date,.by_group = TRUE) |>
    dplyr::mutate(
      !!x@new_column_name:=base::cumsum(!!x@value_quo)
    ) |>
    dplyr::ungroup()

  return(out_tbl)

}


method(print,ytd_tbl) <- function(x){

  x@fn

  group_count <- x@calendar_tbl@group_count
  value_chr <- x@value_vec
  show <- cli::cli_div(theme = cli::simple_theme())



  cli::cli_h1("Year To Date:")
  cli::cli_code("ytd()")
  cli::cli_h2("Description:")
  cli::cli_par()
  cli::cli_text("This will create a cumulative sum of {.field {value_chr}}, from ",cli::col_blue("January 1st")," till ",cli::col_blue("December 31st"))

  cli::builtin_theme()

  cli::cli_h2("Calendar:")
  cli::cat_bullet(paste("The calendar was aggregated to the",cli::col_yellow(x@time_unit@value),"time unit"))

  cli::cli_text("A ",cli::col_br_red(x@calendar_tbl@calendar_type)," calendar is created with ",cli::col_green("{group_count} group{?s}"))

  cli::cat_bullet(paste("Calendar ranges from",cli::col_br_green(x@calendar_tbl@min_date),"to",cli::col_br_green(x@calendar_tbl@max_date)))
  cli::cat_bullet(paste(cli::col_blue(x@calendar_tbl@date_missing),"days were missing and replaced with 0"))
  cli::cli_text("New date column ",cli::col_br_red(x@new_date_column_name)," was created")
  cli::cli_h2("Actions:")

  cli::cli_text(x@action@value[1])
  cli::cli_text(x@action@value[2])
  cli::cli_text(x@action@value[3])

  if(x@calendar_tbl@group_indicator){

  cli::cli_text("{stringr::str_flatten_comma(x@calendar_tbl@group_vec,last = ' and ')} groups are in the table")

  }

  cli::cli_rule()

  cli::cli_li("Use {.code calculate()} to return the results")

  cli::cli_end(show)
}

method(print,dod_tbl) <- function(x){

  make_print_message(x)


}










#' Title
#'
#' @param x
#'
#' @returns
#' @export
#'
#' @examples
method(calculate,qtd_tbl) <- function(x){


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



#' Title
#'
#' @param x
#'
#' @returns
#' @export
#'
#' @examples
method(calculate,mtd_tbl) <- function(x){


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


#' Title
#'
#' @param x
#'
#' @returns
#' @export
#'
#' @examples
method(calculate,wtd_tbl) <- function(x){


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



#' Title
#'
#' @param x
#'
#' @returns
#' @export
#'
#' @examples
method(calculate,atd_tbl) <- function(x){


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


method(calculate,dod_tbl) <- function(x){


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



method(calculate,wow_tbl) <- function(x){


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


method(calculate,mom_tbl) <- function(x){


  full_tbl <-  create_calendar(x)


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


  return(out_tbl)

}


method(calculate,yoy_tbl) <- function(x){


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

