
#' Make Cohort Table
#' @description
#' A database remake of 'https://github.com/PeerChristensen/cohorts' cohort package combining
#' chort_table_month, cohort_table_year, cohort_table_day into a single package.
#' Re-written to be database friendly tested against snowflake and duckdb databases
#'
#' @param .data Either a database or tibble object
#' @param id_var The variable that you want to track over time
#' @param time_unit The time unit as string eg('day','week','month',etc) to round the transaction date passed to lubridate::floor_date()
#' @param date_var  The date column representing the transaction date
#' @param period_label Logical value if you want the represent the dates as periods or Dates
#'
#' @return tibble or DBI object
#' @export
#'
#' @examples
#' fpaR::sales |>  make_cohort_tbl(id_var=product_key,date_var=order_date,time_unit = 'week',period_label =TRUE)
#'
#'
make_cohort_tbl <- function(.data,id_var,date_var,time_unit="month",period_label=FALSE){


## validation tests

  assertthat::assert_that(
    .data |> pull({{date_var}}) |> assertthat::is.date()
    ,msg = "Please ensure date_var column is in Date format"
  )

  assertthat::assert_that(
    assertthat::is.flag(period_label)
    ,msg = "Please ensure period_label is TRUE or FALSE"
  )

  assertthat::assert_that(
    base::any(time_unit %in% c("day","week","month","year","halfyear","quarter","second","minute","hour","bimonth","season"))
    ,msg = "Please time_unit matches 'day','week','month','year','halfyear','quarter','second','minute','hour','bimonth','season'"
  )

  # create flags

  db_flag <- base::any(

      stringr::str_detect(
        stringr::str_to_lower(
          base::class(.data)
          )
        ,"dbi"
        )
      )

## sql base

if(db_flag){

  summary_tbl <- .data |>

    dplyr::group_by({{id_var}}) |>
    dplyr::mutate(date = lubridate::floor_date({{date_var}},unit="week")) |>
    dplyr::mutate(cohort = base::min(date,na.rm=TRUE)) |>
    dbplyr::window_order(date) |>
    dplyr::group_by(cohort, date) |>
    dplyr::summarise(
      users = dplyr::n_distinct({{id_var}})
      ,.groups = "drop"
    ) |>
    dplyr::mutate(period_id=dplyr::sql("DENSE_RANK() OVER (ORDER BY date)")) |>
    dplyr::arrange(date)

} else{

## tibble base


  summary_tbl <- .data |>

    dplyr::group_by({{id_var}}) |>

    dplyr::mutate(
       date   = lubridate::floor_date({{date_var}},unit=time_unit)
      ,cohort = base::min(date,na.rm=TRUE)
      ) |>
    dplyr::group_by(cohort, date) |>
    dplyr::summarise(
      users = dplyr::n_distinct({{id_var}})
      ) |>
    dplyr::group_by(date) |>

    dplyr::mutate(
     period_id= dplyr::cur_group_id()
   ) |>
    dplyr::ungroup()

}

  if(period_label==FALSE){

  out <- summary_tbl |>
    dplyr::select(-period_id) |>
    tidyr::pivot_wider(names_from=date,values_from=users) |>
    dplyr::ungroup() |>
    dplyr::arrange(cohort) |>
    dplyr::mutate(
      cohort_id = dplyr::row_number()
    ) |>
    dplyr::relocate(cohort_id)

  }else{

    out <- summary_tbl |>
      select(-date) |>
      tidyr::pivot_wider(names_from=period_id,values_from=users,names_prefix = "p_") |>
      dplyr::ungroup() |>
      dplyr::arrange(cohort) |>
      dplyr::mutate(cohort_id = dplyr::row_number()) |>
      relocate(cohort_id)


  }

    obj_class <-  stringr::str_flatten_comma(base::class(out),last = " or ")

    cli::cli_alert_info("Returning obj of '{obj_class}' class")

    return(out)

}
