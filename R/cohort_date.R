
#' Make Cohort Table
#'
#' @param .data
#' @param id_var
#' @param date
#' @param time_unit
#'
#' @return
#' @export
#'
#' @examples
make_cohort_tbl <- function(.data,id_var,date,time_unit="month",period_label=FALSE){



  ## object Test

  ### SQL
  id_var_vec <- substitute(id_var)
  time_unit_vec <- as.character(time_unit)
  date_vec <- substitute(date)
  .data <- "contoso_fact_sales"

  id_var_vec <- "StoreKey"
  time_unit_vec <- "month"
  date_vec <- "DateKey"


  print(id_var_vec)
  print(date_vec)
  print(time_unit_vec)


  cohort_sql <- glue::glue_sql(

    "WITH date_var_calculated AS (
  SELECT
        {`id_var_vec`}
        ,DATE_TRUNC({time_unit_vec}, {`date_vec`}) AS date_var
        ,MIN(DATE_TRUNC({time_unit_vec}, {`date_vec`})) OVER (PARTITION BY {`id_var_vec`}) AS cohort

    FROM  {`.data`}),

summary_calculated AS (
    SELECT
        cohort
        ,date_var
        ,COUNT(DISTINCT {`id_var_vec`}) AS users

    FROM
        date_var_calculated

    GROUP BY
        cohort
        ,date_var)

SELECT
    *,
    DENSE_RANK() OVER (ORDER BY cohort, date_var) AS group_id
FROM
    summary_calculated;"
,.con=con
  )

  print(cohort_sql)
#
  out_db <- DBI::dbGetQuery(con,cohort_sql)


  return(out_db)


  ###


  #
  # contoso_fact_sales_db |>
  #   dplyr::group_by(StoreKey) |>
  #   dplyr::mutate(date_var = lubridate::floor_date(DateKey,unit="month")) |>
  #   dplyr::mutate(cohort = min(date_var)) |>
  #   dplyr::group_by(cohort, date_var) |>
  #   dplyr::summarise(
  #     users = dplyr::n_distinct(StoreKey)
  #   ) |>
  #   dplyr::cur_group_id()
  #
  #
  #
  #
  # summary_tbl <- .data %>%
  #   dplyr::group_by({{id_var}}) |>
  #   dplyr::mutate(date_var = lubridate::floor_date({{date}},unit=time_unit)) |>
  #   dplyr::mutate(cohort = min(date_var)) |>
  #   dplyr::group_by(cohort, date_var) |>
  #   dplyr::summarise(
  #     users = dplyr::n_distinct({{id_var}})
  #     ) |>
  #   dplyr::cur_group_id()
  #
  # if(period_label){
  #  out <-  summary_tbl |>
  #     select(-date_var)
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
  #   out <-  summary_tbl |>
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




}
