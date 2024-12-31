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
    date = base::seq.Date(from = x@calendar_tbl@min_date, to = x@calendar_tbl@max_date, by = x@time_unit@value)
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
#' @param x totalytd_tbl object
#'
#' @returns
#' @export
#' @name totalytd_tbl
#' @examples
method(calculate,totalytd_tbl) <- function(x){

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


method(print,totalytd_tbl) <- function(x){

  group_count <- x@calendar_tbl@group_count
  value_chr <- x@value_vec
  show <- cli::cli_div(theme = cli::simple_theme())

  cli::cli_h1("Total Year To Date:")
  cli::cli_code("totalytd()")
  cli::cli_h2("Description:")
  cli::cli_par()
  cli::cli_text("This will create a rolling sum of {.field {value_chr}}, from January 1st till December 31st")

  cli::builtin_theme()

  cli::cli_h2("Calendar:")
  cli::cat_bullet(paste("The calendar was aggregated to the",cli::col_yellow(x@time_unit@value),"time unit"))
  cli::cat_bullet(cli::cli_text("A ",cli::bg_br_white(cli::col_br_red(x@calendar_tbl@type))," calendar is created with {group_count} group{?s}"))
  cli::cat_bullet(paste("Calendar ranges from",cli::col_br_green(x@calendar_tbl@min_date),"to",cli::col_br_green(x@calendar_tbl@max_date)))
  cli::cat_bullet(paste(cli::col_blue(x@calendar_tbl@date_missing),"days were missing and replaced with 0"))
  cli::cat_bullet(cli::cli_text("New date column ",cli::col_br_red(x@new_date_column_name)," was created"))
  cli::cli_h2("Actions:")

  cli::cli_text(x@action@value[1])
  cli::cli_text(x@action@value[2])
  cli::cli_text(x@action@value[3])

  if(x@calendar_tbl@group_indicator){
  cli::cli_text("{stringr::str_flatten_comma(x@calendar_tbl@group_vec,last = ' and ')} groups are in the table")
  }

  # cli::cli_par()
  cli::cli_rule()

  cli::cli_li("Use {.code calculate()} to return the results")

  cli::cli_end(show)
}


#' Title
#'
#' @param x
#'
#' @returns
#' @export
#'
#' @examples
method(calculate,totalqtd_tbl) <- function(x){


full_tbl <-  create_calendar(x) |>
  dplyr::mutate(
    year=lubridate::year(date)
    ,quarter=lubridate::quarter(date)
    ,.before = 1
  )



out_tbl <- full_tbl |>
  dplyr::group_by(year,quarter,!!!x@calendar_tbl@group_quo) |>
  dplyr::mutate(
    !!x@new_column_name:=base::cumsum(!!x@value_quo)
  ) |>
  dplyr::ungroup()

return(out_tbl)

}
