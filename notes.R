
## fix arrange/ window_roder
## need to update the action class for distinct
## come up with print method -- segment vs. category values


x <- cohorts::online_cohorts |> make_db_tbl()

## clean up date cohort

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
    assertthat::is.flag(period_label)
    ,msg = "Please ensure period_label is TRUE or FALSE"
  )


  # create flags



  ## sql base
 # summary_tbl <-  x@data@data |>

  summary_tbl <-

      dplyr::group_by(!!!x@data@group_quo) |>
      dplyr::mutate(date = lubridate::floor_date(!!x@data@date_quo,unit="month")) |>
      dplyr::mutate(cohort = min(date,na.rm=TRUE)) |>
      dbplyr::window_order(date) |>
      dplyr::group_by(cohort, date) |>
      dplyr::summarise(
        !!x@data@group_vec:= dplyr::n_distinct(!!!x@data@group_quo)
        ,.groups = "drop"
      ) |>
      dplyr::mutate(period_id=dplyr::sql("DENSE_RANK() OVER (ORDER BY date)")) |>
      dplyr::arrange(date)


    ## tibble base



  if(period_label==FALSE){

    out <- summary_tbl |>
      dplyr::select(-period_id) |>
      tidyr::pivot_wider(names_from=date,values_from=customer_key) |>
      dplyr::ungroup() |>
      dplyr::arrange(cohort) |>
      dplyr::mutate(
        cohort_id = dplyr::row_number()
      ) |>
      dplyr::relocate(cohort_id)

  }else{

    out <- summary_tbl |>
      select(-date) |>
      tidyr::pivot_wider(names_from=period_id,values_from=customer_key,names_prefix = "p_") |>
      dplyr::ungroup() |>
      dplyr::arrange(cohort) |>
      dplyr::mutate(cohort_id = dplyr::row_number()) |>
      dplyr::relocate(cohort_id)


  }

  obj_class <-  stringr::str_flatten_comma(base::class(out),last = " or ")

  cli::cli_alert_info("Returning obj of '{obj_class}' class")

  return(out)

}

## make non-standard calendar () 8 hours

## convert website to bookdown 4 hours

## factor analysis?? (24 hours)

## lm sql



summary_tbl <-
  x |>
  dplyr::group_by(CustomerID) |>
  dplyr::mutate(date = lubridate::floor_date(InvoiceDate,unit="week")) |>
  dplyr::mutate(cohort = min(date,na.rm=TRUE)) |>
  dbplyr::window_order(date) |>
  dplyr::group_by(cohort, date) |>
  dplyr::summarise(
    user= dplyr::n_distinct(CustomerID)
    ,.groups = "drop"
  ) |>
  dplyr::mutate(period_id=dplyr::sql("DENSE_RANK() OVER (ORDER BY date)")) |>
  dplyr::arrange(date)


## tibble base



if(period_label==FALSE){

  out <- summary_tbl |>
    dplyr::select(-period_id) |>
    tidyr::pivot_wider(names_from=date,values_from=user,names_repair = janitor::make_clean_names) |>
    dplyr::ungroup() |>
    dplyr::arrange(cohort) |>
    dplyr::mutate(
      cohort_id = dplyr::row_number()
    ) |>
    dplyr::relocate(cohort_id)

}else{

  out <- summary_tbl |>
    select(-date) |>
    tidyr::pivot_wider(names_from=period_id,values_from=customer_key,names_prefix = "p_") |>
    dplyr::ungroup() |>
    dplyr::arrange(cohort) |>
    dplyr::mutate(cohort_id = dplyr::row_number()) |>
    dplyr::relocate(cohort_id)


}

return(out)

}



library(devtools)
document()

cohort <- function(.data,.date,.value,calendar_type,time_unit="month"){

  # .data <- sales
  # .date <- "order_date"
  # .value <- "customer_key"



 x <-  segment(
    data=data(
      .data
      ,calendar_type = "standard"
      ,date_vec = rlang::as_label(rlang::enquo(.date))
      )
    ,category = category(category_values = 0)
    ,fn = fn(
      fn_exec = cohort_fn
      ,fn_name = "Cohort"
      ,fn_long_name = "Time based Cohort"
      ,new_date_column_name = "cohort_date")
    ,time_unit = time_unit(value=time_unit)
    ,value = value(
      value_vec = rlang::as_label(rlang::enquo(.value))
      ,new_column_name_vec = "cohort"
      )
    ,action = action(value="distinct",method="test")
    )

return(x)
}



cohort_fn <- function(x,period_label=FALSE){

#
# .data <- sales
# .date <- quo(order_date)
# .value <- "customer_key"
# time_unit <- "month"
# calendar_type <- "standard"

# x <- cohort(
#   .data =.data
#   ,time_unit=time_unit
#   ,.value=.value
#   ,.date=rlang::as_label(rlang::enquo(.date))
#   ,calendar_type = calendar_type
#   )


  ## validation tests

#
#   assertthat::assert_that(
#     assertthat::is.flag(period_label)
#     ,msg = "Please ensure period_label is TRUE or FALSE"
#   )

  # complete calendar

  data <- create_calendar(x)

  # x@data@data |> collect() |>  nrow()
  ## sql base
  # summary_tbl <-

  summary_dbi <-   x@data@data  |>
    dplyr::mutate(date = lubridate::floor_date(!!x@data@date_quo,unit=!!x@time_unit@value)) |>
    dplyr::group_by(!!!x@value@value_quo) |>
    dplyr::mutate(cohort_date = min(date,na.rm=TRUE)) |>
    # dbplyr::window_order(date) |>
    dplyr::group_by(cohort_date, date) |>
    dplyr::summarise(
      !!x@value@new_column_name_vec:= dplyr::n_distinct(!!!x@value@value_quo)
      ,.groups = "drop"
    ) |>
    dplyr::mutate(period_id=dplyr::sql("DENSE_RANK() OVER (ORDER BY date)")) |>
    # dbplyr::window_order(date)
    dplyr::arrange(date)


complete_summary_dbi <- fpaR::seq_date_sql(
  start_date = x@data@min_date
  ,end_date = x@data@max_date
  ,time_unit = x@time_unit@value
  ,con=dbplyr::remote_con(x@data@data)
  ) |>
  dplyr::left_join(
    summary_dbi
    ,by = join_by(date)
  )


  if(!period_label){

    out <- complete_summary_dbi |>
      dplyr::select(-period_id) |>
      dplyr::arrange(date) |>
        tidyr::pivot_wider(
          names_from=date
          ,values_from=!!x@value@new_column_name_quo
          ,names_repair = janitor::make_clean_names
          ,values_fill=0
          ) |>
      dplyr::arrange(cohort_date) |>
      # dbplyr::window_order(cohort_date) |>
      dplyr::mutate(
        cohort_id = dplyr::row_number()
      ) |>
      dplyr::relocate(cohort_id)

  }else{

    out <- complete_summary_dbi |>
      dplyr::arrange(date) |>
      dplyr::select(-date) |>
      tidyr::pivot_wider(
        names_from=period_id
        ,values_from=x@value@new_column_name_quo
        ,names_prefix = "p_"
        ,values_fill = 0
        ) |>
      dplyr::ungroup() |>
      # dplyr::compute() |>
      # dbplyr::window_order(cohort_date) |>
      dplyr::arrange(cohort_date) |>
      dplyr::mutate(cohort_id = dplyr::row_number()) |>
      dplyr::relocate(cohort_id)

  }
  return(out)

}

.data <- sales |> filter(year(order_date)==2023)
x <- cohort(.data,.date=order_date,calendar_type = "standard",.value = customer_key,time_unit = "month")

cohort_fn(x,period_label = TRUE)

