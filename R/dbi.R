#'
#' #' Aggregate and expand date table
#' #'
#' #' @param .data dbi object
#' #' @param time_unit which time unit to aggregate to
#' #' @param date
#' #' @param value
#' #'
#' #' @return list of dbi and sql object
#' #' @export
#' #'
#' make_aggregation_dbi <- function(.data,date,value,time_unit){
#'
#'   ## create inputs----------
#'   original_query <- fpaR::capture_original_query(.data)
#'
#'   min_date <- .data |>
#'     dplyr::summarize(
#'       min = min({{ date_var }}, na.rm = TRUE)
#'     ) |>
#'     dplyr::pull(min)
#'
#'   max_date <- .data |>
#'     dplyr::summarize(
#'       max = max({{ date_var }}, na.rm = TRUE)
#'     ) |>
#'     dplyr::pull(max)
#'
#'   date_var     <- fpaR::convert_input_to_string({{ date_var }})
#'   value_var    <- fpaR::convert_input_to_string({{ value_var }})
#'   time_unit    <- fpaR::convert_input_to_string({{ time_unit }})
#'   interval_key <- base::paste("1", time_unit)
#'   con          <- dbplyr::remote_con(.data)
#'
#'   # Create conditional variables-----------------
#'
#'   if(!missing(...)){
#'
#'
#'
#'     # declare vars
#'     group_var              <- fpaR::convert_dots_to_string(...)
#'
#'
#'     summary_group_var      <- paste0("SUMMARY_TBL.",group_var)
#'     unique_group_var       <- paste0("UNIQUE_GROUPS.",group_var)
#'     cross_joined_group_var <- paste0("CROSS_JOINED.",group_var)
#'
#'     if(length(group_var)>1){
#'
#'       summary_group_id       <- purrr::map(group_var,\(x) DBI::Id("SUMMARY_TBL"  ,x))
#'       unique_group_id        <- purrr::map(group_var,\(x) DBI::Id("UNIQUE_GROUPS",x))
#'       cross_joined_group_id  <- purrr::map(group_var,\(x) DBI::Id("CROSS_JOINED" ,x))
#'
#'     }else{
#'
#'       summary_group_id       <- DBI::Id("SUMMARY_TBL"  ,group_var)
#'       unique_group_id        <- DBI::Id("UNIQUE_GROUPS",group_var)
#'       cross_joined_group_id  <- DBI::Id("CROSS_JOINED" ,group_var)
#'
#'
#'     }
#'     #create join sql queries
#'
#'
#'     initial_join_sql <- glue::glue_sql(
#'       "LEFT JOIN
#'     SUMMARY_TBL
#'     ON
#'     CROSS_JOINED.date = SUMMARY_TBL.date"
#'       ,.con=con)
#'
#'
#'     if(length(group_var)>1){
#'
#'
#'       cross_joined_group_id <- purrr::map(group_var,\(x) DBI::Id("CROSS_JOINED" ,x))
#'       summary_group_id      <- purrr::map(group_var,\(x) DBI::Id("SUMMARY_TBL"  ,x))
#'       join_group_var        <- glue::glue_sql("AND {`cross_joined_group_id`}={`summary_group_id`}",.con=con)
#'       join_group_sql        <- dplyr::sql(base::paste0(join_group_var) |> stringr::str_flatten(collapse = " "))
#'
#'     }else{
#'
#'       cross_joined_group_id <- DBI::Id("CROSS_JOINED" ,group_var)
#'       summary_group_id      <- DBI::Id("SUMMARY_TBL"  ,group_var)
#'       join_group_sql        <- glue::glue_sql("AND {`cross_joined_group_id`}={`summary_group_id`}",.con=con)
#'     }
#'   }
#'
#'   ## create calendar---------------
#'
#'   calendar_sql <- fpaR::seq_date_sql(start_date = {{min_date}},end_date = {{max_date}},time_unit = {{time_unit}},con = con,cte=TRUE)$sql
#'
#'   ## no group summary table-------
#'
#'   no_group_summary_sql <-
#'     fpaR::with(
#'       query=fpaR::sql_query_select(
#'         select = glue::glue_sql("
#'                        DATE_TRUNC({time_unit}, {`date_var`})::DATE AS date
#'                        ,SUM({`value_var`}) AS {`value_var`}",.con=con)
#'         ,group_by=dplyr::sql("ALL")
#'         ,previous_query = TRUE
#'         ,.data = .data
#'       )$sql
#'       ,query_name=summary_tbl
#'       ,order="last"
#'
#'     )
#'
#'
#'   no_group_collect_sql <-
#'     glue::glue_sql("
#'   SELECT
#'   CALENDAR_TBL.date
#'   ,COALESCE(SUMMARY_TBL.{`value_var`}, 0) AS {`value_var`}
#'
#'   FROM
#'   CALENDAR_TBL
#'
#'   LEFT JOIN
#'   SUMMARY_TBL ON
#'
#'   CALENDAR_TBL.date = SUMMARY_TBL.date
#'
#'   ORDER BY
#'   SUMMARY_TBL.date",.con = con)
#'
#'
#'   ## group summary table--------
#'
#'   if(!missing(...)){
#'
#'
#'     group_summary_sql <- fpaR::with(
#'
#'       query=
#'         fpaR::sql_query_select(
#'
#'           select = glue::glue_sql("
#'                        DATE_TRUNC({time_unit}, {`date_var`})::DATE AS date
#'                        ,SUM({`value_var`}) AS {`value_var`}
#'                       ,{`group_var`*}",.con=con)
#'
#'           ,group_by=dplyr::sql("ALL")
#'           ,previous_query = TRUE
#'           ,.data = .data
#'         )$sql
#'       ,query_name=SUMMARY_TBL
#'       ,order="middle"
#'     )
#'
#'
#'
#'     unique_groups_sql <- fpaR::with(
#'       query=glue_sql("
#'         SELECT DISTINCT {`summary_group_id`*}
#'         FROM SUMMARY_TBL",.con=con)
#'       ,query_name=UNIQUE_GROUPS
#'       ,order = "middle"
#'     )
#'
#'
#'     cross_join_sql <- fpaR::with(
#'       query=
#'         glue_sql("
#'     SELECT
#'     CALENDAR_TBL.date,
#'     {`unique_group_id`*}
#'
#'     FROM
#'     CALENDAR_TBL
#'     CROSS JOIN
#'     UNIQUE_GROUPS",.con=con)
#'       ,query_name=CROSS_JOINED
#'       ,order = "last"
#'     )
#'
#'     group_collect_sql <-
#'       glue::glue_sql("
#'     SELECT
#'     CROSS_JOINED.date,
#'     {`cross_joined_group_id`*},
#'     COALESCE(SUMMARY_TBL.{`value_var`}, 0) AS {`value_var`}
#'
#'     FROM
#'     CROSS_JOINED",.con = con)
#'   }
#'
#'   ## return logic---------------
#'
#'   if (missing(...)) {
#'
#'     no_group_cte <- fpaR::cte(
#'       con = con
#'       ,calendar_sql
#'       ,no_group_summary_sql
#'       ,no_group_collect_sql
#'     )
#'
#'     return(no_group_cte)
#'
#'   } else {
#'
#'
#'     group_cte <- cte(
#'       con=con
#'       ,calendar_sql
#'       ,group_summary_sql
#'       ,unique_groups_sql
#'       ,cross_join_sql
#'       ,group_collect_sql
#'       ,initial_join_sql
#'       ,join_group_sql
#'     )
#'     return(group_cte)
#'   }
#' }
#'
#'
#' #' Total year to date values
#' #'
#' #' @param .data tibble of values
#' #' @param ... optional columns to group by
#' #' @param date_var column with date var to aggregate by
#' #' @param value_var column with value to aggregate
#' #'
#' #' @return tibble
#' #' @export
#' #'
#' #' @examples
#' #' totalytd(fpaR:sales,date_var = order_date,value_var = quantity)
#' # totalytd <- function(.data,...,date_var,value_var){
#' #
#' #   # Validate inputs
#' #   assertthat::assert_that(base::is.data.frame(.data), msg = "data must be a data frame")
#' #
#' #   # Aggregate data based on provided time unit
#' #
#' #   full_tbl <-  .data |>
#' #     make_aggregation_tbl(...,date_var={{date_var}},value_var={{value_var}},time_unit="day") |>
#' #     dplyr::mutate(
#' #       year=lubridate::year(date)
#' #       ,.before = 1
#' #     )
#' #
#' #
#' #
#' #   out_tbl <- full_tbl |>
#' #     dplyr::group_by(year,...) |>
#' #     dplyr::arrange(date,.by_group = TRUE) |>
#' #     dplyr::mutate(
#' #     ytd=base::cumsum({{value_var}})
#' #   ) |>
#' #     dplyr::ungroup()
#' #
#' #   return(out_tbl)
#' #
#' # }
#'
#' #' Total year to date values
#' #'
#' #' @param .data tibble of values
#' #' @param ... optional columns to group by
#' #' @param date_var column with date var to aggregate by
#' #' @param value_var column with value to aggregate
#' #'
#' #' @return dbi object
#' #' @export
#' #'
#' #' @examples
#' totalytd_dbi <- function(.data,...,date_var,value_var){
#'
#'   # Validate inputs
#'   assertthat::assert_that(
#'     any(class(.data) %in% c("tbl_dbi"))
#'     , msg = "data must be a DBI"
#'   )
#'
#'   # Aggregate data based on provided time unit
#'   full_dbi <- .data |>
#'     make_aggregation_dbi(...,date_var={{date_var}},value_var={{value_var}},time_unit="day") |>
#'     pluck("dbi") |>
#'     dplyr::mutate(
#'       year=lubridate::year(date)
#'     )
#'
#'
#'   out_dbi <- full_dbi |>
#'     dplyr::group_by(...,year) |>
#'     dbplyr::window_order(date) |>
#'     dplyr::mutate(
#'       ytd=base::cumsum({{value_var}})
#'     ) |>
#'     dplyr::ungroup()
#'
#'   return(out_dbi)
#'
#' }
#'
#'
#' #' Total quarter to date values
#' #'
#' #' @param .data tibble of values
#' #' @param ... optional columns to group by
#' #' @param date_var column with date var to aggregate by
#' #' @param value_var column with value to aggregate
#' #'
#' #' @return tibble
#' #' @export
#' #'
#' #' @examples
#' #' totalqtd(fpaR:sales,date_var = order_date,value_var = quantity)
#' # totalqtd <- function(.data,...,date_var,value_var){
#' #
#' #   # Validate inputs
#' #   assertthat::assert_that(base::is.data.frame(.data), msg = "data must be a data frame")
#' #
#' #   # Aggregate data based on provided time unit
#' #
#' #   full_tbl <-  .data |>
#' #     make_aggregation_tbl(...,date_var={{date_var}},value_var={{value_var}},time_unit="day") |>
#' #     dplyr::mutate(
#' #       year=lubridate::year(date)
#' #       ,quarter=lubridate::quarter(date)
#' #       ,.before = 1
#' #     )
#' #
#' #
#' #
#' #   out_tbl <- full_tbl |>
#' #     dplyr::group_by(year,quarter,...) |>
#' #     dplyr::mutate(
#' #       qtd=base::cumsum({{value_var}})
#' #     ) |>
#' #     dplyr::ungroup()
#' #
#' #   return(out_tbl)
#' #
#' # }
#'
#' #' Total quarter to date values
#' #'
#' #' @param .data tibble of values
#' #' @param ... optional columns to group by
#' #' @param date_var column with date var to aggregate by
#' #' @param value_var column with value to aggregate
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' totalqtd_dbi <- function(.data,...,date_var,value_var){
#'
#'   # Validate inputs
#'   assertthat::assert_that(
#'     any(class(.data) %in% c("tbl_dbi"))
#'     , msg = "data must be a DBI"
#'   )
#'
#'   # Aggregate data based on provided time unit
#'   full_dbi <- .data |>
#'     make_aggregation_dbi(...,date_var={{date_var}},value_var={{value_var}},time_unit="day") |>
#'     pluck("dbi") |>
#'     dplyr::mutate(
#'       year=lubridate::year(date)
#'       ,quarter=lubridate::quarter(date)
#'     )
#'
#'
#'   out_dbi <- full_dbi |>
#'     dplyr::group_by(year,quarter,...) |>
#'     dbplyr::window_order(date) |>
#'     dplyr::mutate(
#'       qtd=base::cumsum({{value_var}})
#'     ) |>
#'     dplyr::ungroup()
#'
#'   return(out_dbi)
#'
#' }
#'
#'
#'
#' #' Total month to date values
#' #'
#' #' @param .data tibble of values
#' #' @param ... optional columns to group by
#' #' @param date_var column with date var to aggregate by
#' #' @param value_var column with value to aggregate
#' #'
#' #' @return tibble
#' #' @export
#' #'
#' #' @examples
#' #' totalmtd(fpaR:sales,date_var = order_date,value_var = quantity)
#' # totalmtd <- function(.data,...,date_var,value_var){
#' #
#' #   # Validate inputs
#' #   assertthat::assert_that(base::is.data.frame(.data), msg = "data must be a data frame")
#' #
#' #   # Aggregate data based on provided time unit
#' #
#' #   full_tbl <-  .data |>
#' #     make_aggregation_tbl(...,date_var={{date_var}},value_var={{value_var}},time_unit="day") |>
#' #     dplyr::mutate(
#' #       month=lubridate::month(date)
#' #       ,year=lubridate::year(date)
#' #       ,.before=1
#' #     ) |>
#' #     arrange(date)
#' #
#' #   # Determine label for the time unit
#' #
#' #
#' #   # Calculate difference and proportional change
#' #
#' #   out_tbl <- full_tbl |>
#' #     dplyr::group_by(year,month,...) |>
#' #     dplyr::mutate(
#' #       mtd=base::cumsum({{value_var}})
#' #     ) |>
#' #     dplyr::ungroup()
#' #
#' #   return(out_tbl)
#' #
#' # }
#'
#' #' Total month to date values
#' #'
#' #' @param .data tibble of values
#' #' @param ... optional columns to group by
#' #' @param date_var column with date var to aggregate by
#' #' @param value_var column with value to aggregate
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' totalmtd_dbi <- function(.data,...,date_var,value_var){
#'
#'   # Validate inputs
#'   assertthat::assert_that(
#'     any(class(.data) %in% c("tbl_dbi"))
#'     , msg = "data must be a DBI"
#'   )
#'
#'   # Aggregate data based on provided time unit
#'   full_dbi <- .data |>
#'     make_aggregation_dbi(...,date_var={{date_var}},value_var={{value_var}},time_unit="day") |>
#'     pluck("dbi") |>
#'     dplyr::mutate(
#'       year=lubridate::year(date)
#'       ,month=lubridate::month(date)
#'     )
#'
#'
#'
#'   out_dbi <- full_dbi |>
#'     dplyr::group_by(year,month,...) |>
#'     dbplyr::window_order(date) |>
#'     dplyr::mutate(
#'       mtd=base::cumsum({{value_var}})
#'     ) |>
#'     dplyr::ungroup()
#'
#'   return(out_dbi)
#'
#' }
#'
#'
#' #' Total Week to date values
#' #'
#' #' @param .data tibble of values
#' #' @param ... optional columns to group by
#' #' @param date_var column with date var to aggregate by
#' #' @param value_var column with value to aggregate
#' #'
#' #' @return tibble
#' #' @export
#' #'
#' #' @examples
#' #' totalwtd(fpaR:sales,date_var = order_date,value_var = quantity)
#' # totalwtd <- function(.data,...,date_var,value_var){
#' #
#' #   # Validate inputs
#' #   assertthat::assert_that(base::is.data.frame(.data), msg = "data must be a data frame")
#' #
#' #   # Aggregate data based on provided time unit
#' #
#' #   full_tbl <-  .data |>
#' #     make_aggregation_tbl(...,date_var={{date_var}},value_var={{value_var}},time_unit="day") |>
#' #     mutate(
#' #       week=lubridate::week(date)
#' #       ,month=lubridate::month(date)
#' #       ,year=lubridate::year(date)
#' #       ,.before = 1
#' #     )
#' #
#' #   # Determine label for the time unit
#' #
#' #
#' #   # Calculate difference and proportional change
#' #
#' #   out_tbl <- full_tbl |>
#' #     dplyr::group_by(week,month,year,... ) |>
#' #     dplyr::mutate(
#' #       wtd=base::cumsum({{value_var}})
#' #     ) |>
#' #     dplyr::ungroup()
#' #
#' #   return(out_tbl)
#' #
#' # }
#'
#' #' Total week to date values
#' #'
#' #' @param .data tibble of values
#' #' @param ... optional columns to group by
#' #' @param date_var column with date var to aggregate by
#' #' @param value_var column with value to aggregate
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' totalwtd_dbi <- function(.data,...,date_var,value_var){
#'
#'   # Validate inputs
#'   assertthat::assert_that(
#'     any(class(.data) %in% c("tbl_dbi"))
#'     , msg = "data must be a DBI"
#'   )
#'
#'   # Aggregate data based on provided time unit
#'   full_dbi <- .data |>
#'     make_aggregation_dbi(...,date_var={{date_var}},value_var={{value_var}},time_unit="day") |>
#'     pluck("dbi") |>
#'     dplyr::mutate(
#'       year=lubridate::year(date)
#'       ,month=lubridate::month(date)
#'       ,week=sql("EXTRACT(WEEK FROM date)")
#'     )
#'
#'
#'   out_dbi <- full_dbi |>
#'     dplyr::group_by(year,month,week,...) |>
#'     dbplyr::window_order(date) |>
#'     dplyr::mutate(
#'       wtd=base::cumsum({{value_var}})
#'     ) |>
#'     dplyr::ungroup()
#'
#'   return(out_dbi)
#'
#' }
#'
#'
#' #' Total since inception
#' #'
#' #' @param .data tibble of values
#' #' @param ... optional columns to group by
#' #' @param date_var column with date var to aggregate by
#' #' @param value_var column with value to aggregate
#' #'
#' #' @return tibble
#' #' @export
#' #'
#' #' @examples
#' #' totalatd(fpaR:sales,date_var = order_date,value_var = quantity)
#' # totalatd <- function(.data,...,date_var,value_var){
#' #
#' #   # Validate inputs
#' #   assertthat::assert_that(base::is.data.frame(.data), msg = "data must be a data frame")
#' #
#' #   # Aggregate data based on provided time unit
#' #
#' #   full_tbl <-  .data |>
#' #     make_aggregation_tbl(...,date_var={{date_var}},value_var={{value_var}},time_unit="day") |>
#' #     dplyr::arrange(date)
#' #
#' #   # Determine label for the time unit
#' #
#' #
#' #   # Calculate difference and proportional change
#' #
#' #   out_tbl <- full_tbl |>
#' #     group_by(...) |>
#' #     dplyr::mutate(
#' #       atd=base::cumsum({{value_var}})
#' #     )
#' #
#' #   return(out_tbl)
#' #
#' # }
#'
#' #' Total since inception
#' #'
#' #' @param .data tibble of values
#' #' @param ... optional columns to group by
#' #' @param date_var column with date var to aggregate by
#' #' @param value_var column with value to aggregate
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' totalatd_dbi <- function(.data,...,date_var,value_var){
#'
#'   # Validate inputs
#'   assertthat::assert_that(
#'     any(class(.data) %in% c("tbl_dbi"))
#'     , msg = "data must be a DBI"
#'   )
#'
#'   # Aggregate data based on provided time unit
#'   full_dbi <- .data |>
#'     make_aggregation_dbi(...,date_var={{date_var}},value_var={{value_var}},time_unit="day") |>
#'     pluck("dbi") |>
#'     dplyr::arrange(date)
#'
#'
#'   out_dbi <- full_dbi |>
#'     dplyr::group_by(...) |>
#'     dbplyr::window_order(date) |>
#'     dplyr::mutate(
#'       atd=base::cumsum({{value_var}})
#'     ) |>
#'     dplyr::ungroup()
#'
#'   return(out_dbi)
#'
#' }
#'
#'
#' #' Day over day valus
#' #'
#' #' @param .data
#' #' @param ...
#' #' @param date_var
#' #' @param value_var
#' #' @param lag_n
#' #' @param time_unit
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' # dod <- function(.data,...,date_var,value_var,lag_n=1,time_unit="day"){
#' #
#' #   # Validate inputs
#' #   assertthat::assert_that(base::is.data.frame(.data), msg = "data must be a data frame")
#' #   assertthat::assert_that(time_unit %in% base::c("day","quarter","month", "year"), msg = "Time frame must be one of 'day', 'month',;quarter' or 'year'.")
#' #   # Aggregate data based on provided time unit
#' #
#' #   full_tbl <-  .data |>
#' #     make_aggregation_tbl(...,date_var={{date_var}},value_var={{value_var}},time_unit=time_unit) |>
#' #     arrange(
#' #       date
#' #     )
#' #
#' #
#' #   ## multiplication factor
#' #
#' #   multiply_options <- c("day"=1,"month"=12,"quarter"=4,"year"=1)
#' #
#' #
#' #
#' #   multiply_vec <- multiply_options[time_unit] |> base::unname()
#' #
#' #
#' #
#' #   if(time_unit %in% c("day")){
#' #
#' #     # Calculate difference and proportional change
#' #
#' #
#' #     lag_tbl <- full_tbl |>
#' #       dplyr::group_by(...) |>
#' #       dplyr::mutate(
#' #         date_lag=date %m+% lubridate::years(lag_n)
#' #         ,"{{value_var}}_dod":={{value_var}}
#' #       ) |>
#' #       dplyr::select(-c(date,{{value_var}})) |>
#' #       dplyr::ungroup()
#' #
#' #     out_tbl <-  dplyr::left_join(
#' #       full_tbl
#' #       ,lag_tbl
#' #       ,by=dplyr::join_by(date==date_lag,...)
#' #     ) |>
#' #       mutate(
#' #         "{{value_var}}_dod" := dplyr::coalesce(.data[[rlang::englue("{{value_var}}_dod")]],0)
#' #       )
#' #     return(out_tbl)
#' #
#' #   } else {
#' #
#' #     out_tbl <-  full_tbl |>
#' #       group_by(...) |>
#' #       dplyr::mutate(
#' #         "{{value_var}}_dod":=dplyr::lag({{value_var}},n=(lag_n*multiply_vec))
#' #       ) |>
#' #       dplyr::ungroup()
#' #
#' #     return(out_tbl)
#' #
#' #   }
#' #
#' #
#' # }
#'
#' #' calculate day over day values on a complete calendar table
#' #'
#' #' @param .data
#' #' @param ...
#' #' @param date_var
#' #' @param value_var
#' #' @param lag_n
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' dod_dbi <- function(.data,...,date_var,value_var,lag_n=1){
#'
#'   ## create variables
#'
#'   value_var_old <- deparse(substitute(value_var))
#'   value_var_interim<- paste0(value_var_old,"_","dod")
#'   value_var_final <- paste0(value_var_interim,"_",lag_n)
#'
#'
#'   full_dbi <- .data |>
#'     make_aggregation_dbi(...,date_var={{date_var}},value_var={{value_var}},time_unit="day") |>
#'     purrr::pluck("dbi")
#'
#'   if(!missing(...)){
#'     # create lags
#'     lag_table <-  full_dbi |>
#'       dplyr::group_by(...) |>
#'       dbplyr::window_order(date) |>
#'       dplyr::mutate(
#'         !!value_var_interim:={{value_var}}
#'         ,date_lag=dplyr::sql(glue::glue("DATE + INTERVAL {lag_n} DAY"))
#'       ) |>
#'       dplyr::select(-c(date,all_of(value_var_old))) |>
#'       dplyr::ungroup() |>
#'       mutate(
#'         date_lag=as.Date(date_lag)
#'       )
#'   }else{
#'     lag_table <-  full_dbi |>
#'       dbplyr::window_order(date) |>
#'       dplyr::mutate(
#'         !!value_var_interim:={{value_var}}
#'         ,date_lag=dplyr::sql(glue::glue("DATE + INTERVAL {lag_n} DAY"))
#'       ) |>
#'       dplyr::select(-c(date,all_of(value_var_old))) |>
#'       dplyr::ungroup() |>
#'       mutate(
#'         date_lag=as.Date(date_lag)
#'       )
#'   }
#'
#'   # join tables
#'   out_tbl <-  dplyr::left_join(
#'     full_dbi
#'     ,lag_table
#'     ,by=dplyr::join_by(date==date_lag,...)
#'   ) |>
#'     mutate(
#'       !!value_var_final:=dplyr::sql(glue::glue("COALESCE({value_var_interim},0)"))
#'     ) |>
#'     select(-c(all_of(value_var_interim))) |>
#'     arrange(date)
#'
#'   return(out_tbl)
#'
#' }
#'
#' #'
#' #'
#' #' #' Week over week values
#' #' #' @description
#' #' #' For datasets with daily granularity, this will calculate year over year values with some simple descriptive functions
#' #' #'
#' #' #' @param .data tibble of values
#' #' #' @param ... optional columns to group by
#' #' #' @param date_var column with date var to aggregate by
#' #' #' @param value_var column with value to aggregate
#' #' #' @param lag_n the number of weeks to lag
#' #' #'
#' #' #' @return tibble
#' #' #' @export
#' #' #'
#' #' #' @examples
#' #' #' wow(fpaR:sales,date_var = order_date,value_var = quantity)
#' #' wow <- function(.data,...,date_var,value_var,lag_n=1){
#' #'
#' #'   # Validate inputs
#' #'   assertthat::assert_that(base::is.data.frame(.data), msg = "data must be a data frame")
#' #'
#' #'   # Aggregate data based on provided time unit
#' #'
#' #'   full_tbl <-  .data |>
#' #'     make_aggregation_tbl(...,date_var={{date_var}},value_var={{value_var}},time_unit="day")
#' #'
#' #'   # Determine label for the time unit
#' #'
#' #'
#' #'   # Calculate difference and proportional change
#' #'
#' #'   lag_table <- full_tbl |>
#' #'     dplyr::group_by(...) |>
#' #'     dplyr::mutate(
#' #'       date_lag=date %m+% lubridate::weeks(lag_n)
#' #'       ,"{{value_var}}_wow":={{value_var}}
#' #'     ) |>
#' #'     dplyr::select(-c(date,{{value_var}})) |>
#' #'     dplyr::ungroup()
#' #'
#' #'
#' #'   out_tbl <-  dplyr::left_join(
#' #'     full_tbl
#' #'     ,lag_table
#' #'     ,by=dplyr::join_by(date==date_lag,...)
#' #'   ) |>
#' #'     mutate(
#' #'       "{{value_var}}_wow" := dplyr::coalesce(.data[[rlang::englue("{{value_var}}_wow")]],0)
#' #'     )
#' #'
#' #'   return(out_tbl)
#' #'
#' #' }
#'
#'
#' #' week over week comparison for DBI objects
#' #'
#' #' @param .data tibble of values
#' #' @param ... optional columns to group by
#' #' @param date_var column with date var to aggregate by
#' #' @param value_var column with value to aggregate
#' #' @param lag_n the number of weeks to lag
#' #'
#' #' @return DBI object
#' #' @export
#' #'
#' #' @examples
#' wow_dbi <- function(.data,...,date_var,value_var,lag_n=1){
#'
#'   ## create variables
#'
#'   value_var_old <- deparse(substitute(value_var))
#'   value_var_interim<- paste0(value_var_old,"_","wow")
#'   value_var_final <- paste0(value_var_interim,"_",lag_n)
#'
#'
#'   full_dbi <- .data |>
#'     make_aggregation_dbi(...,date_var={{date_var}},value_var={{value_var}},time_unit="day") |>
#'     purrr::pluck("dbi")
#'
#'   if(!missing(...)){
#'     # create lags
#'     lag_table <-  full_dbi |>
#'       dplyr::group_by(...) |>
#'       dbplyr::window_order(date) |>
#'       dplyr::mutate(
#'         !!value_var_interim:={{value_var}}
#'         ,date_lag=dplyr::sql(glue::glue("DATE + INTERVAL {lag_n} WEEK"))
#'       ) |>
#'       dplyr::select(-c(date,all_of(value_var_old))) |>
#'       dplyr::ungroup() |>
#'       mutate(
#'         date_lag=as.Date(date_lag)
#'       )
#'   }else{
#'     lag_table <-  full_dbi |>
#'       dbplyr::window_order(date) |>
#'       dplyr::mutate(
#'         !!value_var_interim:={{value_var}}
#'         ,date_lag=dplyr::sql(glue::glue("DATE + INTERVAL {lag_n} WEEK"))
#'       ) |>
#'       dplyr::select(-c(date,all_of(value_var_old))) |>
#'       dplyr::ungroup() |>
#'       mutate(
#'         date_lag=as.Date(date_lag)
#'       )
#'   }
#'
#'   # join tables
#'   out_tbl <-  dplyr::left_join(
#'     full_dbi
#'     ,lag_table
#'     ,by=dplyr::join_by(date==date_lag,...)
#'   ) |>
#'     mutate(
#'       !!value_var_final:=dplyr::sql(glue::glue("COALESCE({value_var_interim},0)"))
#'     ) |>
#'     select(-c(all_of(value_var_interim))) |>
#'     arrange(date)
#'
#'   return(out_tbl)
#'
#' }
#' #'
#' #'
#' #' #' Month over month values
#' #' #' @description
#' #' #' For datasets with daily granularity, this will calculate year over year values with some simple descriptive functions
#' #' #'
#' #' #' @param .data tibble of values
#' #' #' @param ... optional columns to group by
#' #' #' @param date_var column with date var to aggregate by
#' #' #' @param value_var column with value to aggregate
#' #' #' @param lag_n the number of weeks to lag
#' #' #'
#' #' #' @return tibble
#' #' #' @export
#' #' #'
#' #' #' @examples
#' #' #' mom(sales_tbl,date_var = order_date,value_var = quantity)
#' #' mom <- function(.data,...,date_var,value_var,lag_n=1){
#' #'
#' #'   # Validate inputs
#' #'   assertthat::assert_that(base::is.data.frame(.data), msg = "data must be a data frame")
#' #'   # Aggregate data based on provided time unit
#' #'
#' #'   full_tbl <-  .data |>
#' #'     make_aggregation_tbl(...,date_var={{date_var}},value_var={{value_var}},time_unit="day")
#' #'
#' #'   # Determine label for the time unit
#' #'
#' #'
#' #'   # Calculate difference and proportional change
#' #'
#' #'   lag_table <- full_tbl |>
#' #'     dplyr::group_by(...) |>
#' #'     dplyr::mutate(
#' #'       date_lag=date %m+% base::months(lag_n)
#' #'       ,"{{value_var}}_mom":={{value_var}}
#' #'     ) |>
#' #'     dplyr::select(-c(date,{{value_var}})) |>
#' #'     dplyr::ungroup()
#' #'
#' #'   print(full_tbl)
#' #'
#' #'
#' #'  out_tbl <-  dplyr::left_join(
#' #'     full_tbl
#' #'     ,lag_table
#' #'     ,by=dplyr::join_by(date==date_lag,...)
#' #'   ) |>
#' #'    mutate(
#' #'      "{{value_var}}_mom" := dplyr::coalesce(.data[[rlang::englue("{{value_var}}_mom")]],0)
#' #'    )
#' #'
#' #'   return(out_tbl)
#' #'
#' #' }
#'
#'
#' #' Month over month values
#' #'
#' #' @param .data tibble of values
#' #' @param ... optional columns to group by
#' #' @param date_var column with date var to aggregate by
#' #' @param value_var column with value to aggregate
#' #' @param lag_n the number of months to lag
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' mom_dbi <- function(.data,...,date_var,value_var,lag_n=1){
#'
#'   ## create variables
#'
#'   value_var_old <- deparse(substitute(value_var))
#'   value_var_interim<- paste0(value_var_old,"_","mom")
#'   value_var_final <- paste0(value_var_interim,"_",lag_n)
#'
#'
#'   full_dbi <- .data |>
#'     make_aggregation_dbi(...,date_var={{date_var}},value_var={{value_var}},time_unit="day") |>
#'     purrr::pluck("dbi")
#'
#'   if(!missing(...)){
#'     # create lags
#'     lag_table <-  full_dbi |>
#'       dplyr::group_by(...) |>
#'       dbplyr::window_order(date) |>
#'       dplyr::mutate(
#'         !!value_var_interim:={{value_var}}
#'         ,date_lag=dplyr::sql(glue::glue("DATE + INTERVAL {lag_n} WEEK"))
#'       ) |>
#'       dplyr::select(-c(date,all_of(value_var_old))) |>
#'       dplyr::ungroup() |>
#'       mutate(
#'         date_lag=as.Date(date_lag)
#'       )
#'   }else{
#'     lag_table <-  full_dbi |>
#'       dbplyr::window_order(date) |>
#'       dplyr::mutate(
#'         !!value_var_interim:={{value_var}}
#'         ,date_lag=dplyr::sql(glue::glue("DATE + INTERVAL {lag_n} MONTH"))
#'       ) |>
#'       dplyr::select(-c(date,all_of(value_var_old))) |>
#'       dplyr::ungroup() |>
#'       mutate(
#'         date_lag=as.Date(date_lag)
#'       )
#'   }
#'
#'   # join tables
#'   out_tbl <-  dplyr::left_join(
#'     full_dbi
#'     ,lag_table
#'     ,by=dplyr::join_by(date==date_lag,...)
#'   ) |>
#'     mutate(
#'       !!value_var_final:=dplyr::sql(glue::glue("COALESCE({value_var_interim},0)"))
#'     ) |>
#'     select(-c(all_of(value_var_interim))) |>
#'     arrange(date)
#'
#'   return(out_tbl)
#'
#' }
#'
#'
#'
#' #'
#' #'
#' #'
#' #' #' Year over year values
#' #' #' @description
#' #' #' For datasets with daily granularity, this will calculate year over year values with some simple descriptive functions
#' #' #'
#' #' #' @param .data tibble of values
#' #' #' @param ... optional columns to group by
#' #' #' @param date_var column with date var to aggregate by
#' #' #' @param value_var column with value to aggregate
#' #' #' @param lag_n
#' #' #' @param time_unit
#' #' #'
#' #' #' @return tibble
#' #' #' @export
#' #' #'
#' #' #' @examples
#' #' #' yoy(fpaR::sales,date_var = order_date,value_var = quantity)
#' #' yoy <- function(.data,...,date_var,value_var,lag_n=1,time_unit="day"){
#' #'
#' #'   # Validate inputs
#' #'   assertthat::assert_that(base::is.data.frame(.data), msg = "data must be a data frame")
#' #'   assertthat::assert_that(time_unit %in% base::c("day","quarter","month", "year"), msg = "Time frame must be one of 'day', 'month',;quarter' or 'year'.")
#' #'   # Aggregate data based on provided time unit
#' #'
#' #'   full_tbl <-  .data |>
#' #'     make_aggregation_tbl(...,date_var={{date_var}},value_var={{value_var}},time_unit=time_unit) |>
#' #'     arrange(
#' #'       date
#' #'     )
#' #'
#' #'
#' #'   ## multiplication factor
#' #'
#' #'   multiply_options <- c("day"=1,"month"=12,"quarter"=4,"year"=1)
#' #'
#' #'
#' #'
#' #'   multiply_vec <- multiply_options[time_unit] |> base::unname()
#' #'
#' #'
#' #'
#' #'   if(time_unit %in% c("day")){
#' #'
#' #'   # Calculate difference and proportional change
#' #'
#' #'
#' #'   lag_tbl <- full_tbl |>
#' #'     dplyr::group_by(...) |>
#' #'     dplyr::mutate(
#' #'       date_lag=date %m+% lubridate::years(lag_n)
#' #'       ,"{{value_var}}_yoy":={{value_var}}
#' #'     ) |>
#' #'     dplyr::select(-c(date,{{value_var}})) |>
#' #'     dplyr::ungroup()
#' #'
#' #'   out_tbl <-  dplyr::left_join(
#' #'     full_tbl
#' #'     ,lag_tbl
#' #'     ,by=dplyr::join_by(date==date_lag,...)
#' #'   ) |>
#' #'     mutate(
#' #'       "{{value_var}}_yoy" := dplyr::coalesce(.data[[rlang::englue("{{value_var}}_yoy")]],0)
#' #'     )
#' #'     return(out_tbl)
#' #'
#' #'   } else {
#' #'
#' #'     out_tbl <-  full_tbl |>
#' #'       group_by(...) |>
#' #'       dplyr::mutate(
#' #'         "{{value_var}}_yoy":=dplyr::lag({{value_var}},n=(lag_n*multiply_vec))
#' #'       ) |>
#' #'       dplyr::ungroup()
#' #'
#' #'     return(out_tbl)
#' #'
#' #'   }
#' #'
#' #'
#' #' }
#'
#' #' Year over year values for DBI objects
#' #'
#' #' @param .data tibble of values
#' #' @param ... optional columns to group by
#' #' @param date_var column with date var to aggregate by
#' #' @param value_var column with value to aggregate
#' #' @param lag_n number of time units to lagg
#' #' @param time_unit to return aggregate data by 'day','week','month','quarter' or 'year'
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' yoy_dbi <- function(.data,...,date_var,value_var,lag_n=1,time_unit="day"){
#'
#'   ## create variables
#'
#'   value_var_old <- deparse(substitute(value_var))
#'   value_var_interim<- paste0(value_var_old,"_","mom")
#'   value_var_final <- paste0(value_var_interim,"_",lag_n)
#'
#'
#'   full_dbi <- .data |>
#'     make_aggregation_dbi(...,date_var={{date_var}},value_var={{value_var}},time_unit="day") |>
#'     purrr::pluck("dbi")
#'
#'   if(!missing(...)){
#'     # create lags
#'     lag_table <-  full_dbi |>
#'       dplyr::group_by(...) |>
#'       dbplyr::window_order(date) |>
#'       dplyr::mutate(
#'         !!value_var_interim:={{value_var}}
#'         ,date_lag=dplyr::sql(glue::glue("DATE + INTERVAL {lag_n} YEAR"))
#'       ) |>
#'       dplyr::select(-c(date,all_of(value_var_old))) |>
#'       dplyr::ungroup() |>
#'       mutate(
#'         date_lag=as.Date(date_lag)
#'       )
#'   }else{
#'     lag_table <-  full_dbi |>
#'       dbplyr::window_order(date) |>
#'       dplyr::mutate(
#'         !!value_var_interim:={{value_var}}
#'         ,date_lag=dplyr::sql(glue::glue("DATE + INTERVAL {lag_n} YEAR"))
#'       ) |>
#'       dplyr::select(-c(date,all_of(value_var_old))) |>
#'       dplyr::ungroup() |>
#'       mutate(
#'         date_lag=as.Date(date_lag)
#'       )
#'   }
#'
#'   # join tables
#'   out_tbl <-  dplyr::left_join(
#'     full_dbi
#'     ,lag_table
#'     ,by=dplyr::join_by(date==date_lag,...)
#'   ) |>
#'     mutate(
#'       !!value_var_final:=dplyr::sql(glue::glue("COALESCE({value_var_interim},0)"))
#'     ) |>
#'     select(-c(all_of(value_var_interim))) |>
#'     arrange(date)
#'
#'   return(out_tbl)
#' }
#'
#'
#'
#'
#'
#'
#' #' Create a calendar table in sql (standalone or part of CTE)
#' #'
#' #' @param start_date calendar start date in YYYY-MM-DD format
#' #' @param end_date calendar end date in YYYY-MM-DD format
#' #' @param time_unit calendar table unit in day, week, month, quarter or year
#' #' @param cte logical indicator generate sql query to be used as part of a CTE
#' #' @param con database connection
#' #'
#' #' @return DBI object
#' #' @export
#' #'
#' #' @examples
#' #' con <- DBI::dbConnect(drv = duckdb::duckdb())
#' #' seq_date_sql(start_date = "2015-01-01", end_date = "2024-04-20", time_unit = "day", cte = FALSE, con = con)
#' seq_date_sql <- function(start_date, end_date, time_unit, cte = TRUE, con) {
#'   ## prepare and validate inputs----------
#'
#'
#'
#'   start_date_vec <- start_date
#'   end_date_vec <- end_date
#'   time_unit_vec <- time_unit
#'   interval_key <- paste("1", time_unit_vec)
#'
#'
#'
#'   assertthat::assert_that(
#'     time_unit_vec %in% c("day", "week", "month", "quarter", "year"),
#'     msg = "Please have time unit match 'day', 'week','month','quarter' or 'year'"
#'   )
#'
#'   assertthat::assert_that(
#'     base::is.logical(cte),
#'     msg = "Please select TRUE or FALSE if the return query should be a CTE or standalone query"
#'   )
#'
#'
#'   assertthat::assert_that(
#'     is_yyyy_mm_dd(start_date_vec) & is_yyyy_mm_dd(end_date_vec),
#'     msg = "Please ensure dates are in YYYY-MM-DD format"
#'   )
#'
#'   assertthat::assert_that(
#'     lubridate::ymd(start_date_vec) < lubridate::ymd(end_date_vec),
#'     msg = "Please ensure end date is greater than start date"
#'   )
#'
#'   con_info <- paste0("connection: ", DBI::dbGetInfo(con)$dbname)
#'
#'   assertthat::assert_that(
#'     DBI::dbIsValid(con),
#'     msg = paste("Please check if your connection is valid", con_info)
#'   )
#'
#'   ## calendar CTE query----------
#'
#'
#'   ### generate series of dates
#'
#'   date_series_sql <- fpaR::with(
#'     query = glue::glue_sql("
#'
#'     SELECT
#'
#'     GENERATE_SERIES(
#'        MIN(DATE_TRUNC({time_unit_vec}, DATE {start_date_vec}))::DATE
#'       ,MAX(DATE_TRUNC({time_unit_vec}, DATE {end_date_vec}))::DATE
#'       ,INTERVAL {interval_key}
#'     ) AS DATE_LIST"
#'                            ,.con = con
#'     )
#'     ,query_name = DATE_SERIES
#'     ,order = "first"
#'   )
#'
#'   ## Expand series as last query
#'   calendar_tbl_last_sql <- fpaR::with(
#'     query=dplyr::sql(
#'       "
#'       SELECT
#'
#'       UNNEST(DATE_LIST)::DATE AS date
#'
#'       FROM DATE_SERIES
#'
#'       "
#'     )
#'     ,query_name = CALENDAR_TBL
#'     ,order = "last"
#'   )
#'
#'
#'   ## Expand series as middle query
#'
#'
#'   calendar_tbl_middle_sql <- fpaR::with(
#'     query=dplyr::sql(
#'       "
#'       SELECT
#'
#'       UNNEST(DATE_LIST)::DATE AS date
#'
#'       FROM DATE_SERIES
#'
#'       "
#'     )
#'     ,query_name = CALENDAR_TBL
#'     ,order = "middle"
#'   )
#'
#'   ## no CTE returns dbi
#'
#'   no_cte_sql <- cte(
#'     con = con
#'     ,dplyr::sql(date_series_sql)
#'     ,sql(calendar_tbl_last_sql)
#'     ,dplyr::sql("select * from CALENDAR_TBL")
#'   )
#'
#'   ## does not return dbi
#'
#'   cte_sql <- cte(
#'     con = con
#'     ,dplyr::sql(date_series_sql)
#'     ,sql(calendar_tbl_middle_sql)
#'   )
#'
#'   ## print alerts and return objects-------
#'
#'   cli::cli_alert_info("{con_info} database")
#'
#'   if (cte) {
#'     return(cte_sql)
#'   } else {
#'     return(no_cte_sql)
#'   }
#' }
