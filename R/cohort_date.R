
#' Make Cohort Table
#'
#' @param .data
#' @param id_var
#' @param time_unit
#' @param date_var
#' @param period_label
#'
#' @return
#' @export
#'
#' @examples
make_cohort_tbl <- function(.data,id_var,date_var,time_unit="month",period_label=FALSE){


  db_flag <- any(str_detect(class(.data),"dbi"))


## sql base

if(db_flag){
  summary_tbl <- .data |>

    dplyr::group_by({{id_var}}) |>

    dplyr::mutate(
        date    = lubridate::floor_date({{date_var}},unit='month')
       ,cohort  = min(date,na.rm=TRUE)
    ) |>

    dplyr::group_by(cohort, date) |>

    dplyr::summarise(
      users = dplyr::n_distinct(StoreKey)
    ) |>

    mutate(period_id=dplyr::sql("DENSE_RANK() OVER (ORDER BY cohort)")) |>
    dbplyr::window_order(date)

} else{

## tibble base


  summary_tbl <- .data |>

    dplyr::group_by({{id_var}}) |>

    dplyr::mutate(
       date   = lubridate::floor_date({{date_var}},unit=time_unit)
      ,cohort = min(date,na.rm=TRUE)
      ) |>

    dplyr::group_by(cohort, date) |>

    dplyr::summarise(
      users = dplyr::n_distinct({{id_var}})
      ,.groups = "keep"
      ) |>

   mutate(
     period_id= dplyr::cur_group_id()
   ) |>
    dbplyr::window_order(date)

}



  #
  #
  #
  if(period_label==FALSE & db_flag==TRUE){


  out <- summary_tbl |>
    select(-period_id) |>
    tidyr::pivot_wider(names_from=date,values_from=users,values_fill = 0) |>
    dplyr::ungroup() |>
    dplyr::mutate(cohort_id = row_number()) |>
    relocate(cohort_id)

  return(out)

  }else if(period_label==TRUE & db_flag==TRUE){

    out <- summary_tbl |>
      select(-date) |>
      tidyr::pivot_wider(names_from=period_id,values_from=users,values_fill = 0,names_prefix = "period_") |>
      dplyr::ungroup() |>
      dplyr::mutate(cohort_id = row_number()) |>
      relocate(cohort_id,num_range(prefix="period_",range=1:100))

    return(out)


  } else {

    return(summary_tbl)
  }

  #  out <-  summary_tbl |>
  #     select(-date) |>
  #     tidyr::pivot_wider(names_from=period_label,values_from=users) |>
  #     dplyr::ungroup() |>
  #     dplyr::mutate(cohort = 1:dplyr::n_distinct(cohort))
  #
  #     obj_class <-  stringr::str_flatten_comma(base::class(out),last = " or ")
  #
  #     cli::cli_alert_info("Returning obj of '{obj_class}' class")
  #
  #   return(out)
  #
  #
  # } else{
  #   out <-  summary_db |>
  #   select(-period_label)
  #   tidyr::pivot_wider(names_from=date_var,values_from=users) |>
  #     dplyr::ungroup() |>
  #     dplyr::mutate(cohort = 1:dplyr::n_distinct(cohort))
  #
  #   obj_class <-  stringr::str_flatten_comma(base::class(out),last = " or ")
  #
  #   cli::cli_alert_info("Returning obj of '{obj_class}' class")
  #
  #   return(out)
  #
  # }

  # test_db |>
  #   select(-group_id) |>
  #   tidyr::pivot_wider(names_from=date,values_from=users) |>
  #     dplyr::ungroup() |>
  #     dplyr::mutate(cohort_id = row_number()) |>
  #   relocate(cohort_id)


}
