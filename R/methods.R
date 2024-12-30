
calculate <- new_generic("calculate","x")

method(calculate,totalytd_tbl) <- function(x){

  # Aggregate data based on provided time unit

  full_tbl <-  x@data |>
    fpaR::make_aggregation_tbl(!!x@group_quo,date_var=!!x@calendar_tbl@date_quo,value_var=!!x@value_quo,time_unit=x@time_unit@value) |>
    dplyr::mutate(
      year=lubridate::year(date)
      ,.before = 1
    )



  out_tbl <- full_tbl |>
    dplyr::group_by(year) |>
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

  cli::cli_h2("Actions:")

  cli::cli_text(x@action@value[1])
  cli::cli_text(x@action@value[2])
  cli::cli_text(x@action@value[3])


  # cli::cli_par()
  cli::cli_rule()

  cli::cli_li("Use {.code calculate()} to return the results")

  cli::cli_end(show)
}


method(calculate,totalqtd_tbl) <- function(x){

full_tbl <-  x@data |>
  make_aggregation_tbl(!!x@calendar_tbl@group_quo,date_var=!!x@calendar_tbl@date_quo,value_var=!!x@value_quo,time_unit=x@time_unit@value) |>
  dplyr::mutate(
    year=lubridate::year(date)
    ,quarter=lubridate::quarter(date)
    ,.before = 1
  )



out_tbl <- full_tbl |>
  dplyr::group_by(year,quarter,!!x@calendar_tbl@group_quo) |>
  dplyr::mutate(
    !!x@new_column_name:=base::cumsum(!!x@value_quo)
  ) |>
  dplyr::ungroup()

return(out_tbl)

}
