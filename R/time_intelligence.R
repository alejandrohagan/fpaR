#' Aggregate and expand date table
#'
#' @param data
#' @param ...
#' @param date_var
#' @param value_var
#' @param time_unit
#'
#' @return tibble
#' @export
#'
#' @examples
#' make_aggregation_tbl(fpaR::sales,date,quantity,"day")
make_aggregation_tbl <- function(.data,...,date_var,value_var, time_unit) {

  assertthat::assert_that(base::is.data.frame(.data), msg = "Data must be a data frame.")
  assertthat::assert_that(base::is.character(time_unit), msg = "Time unit must be a character string.")
  assertthat::assert_that(time_unit %in% base::c("day", "week","quarter", "month", "year"), msg = "Time frame must be one of 'day', 'week', 'month', or 'year'.")
  # assert_that(is.Date(date_var), msg = "The date column is not in Date format.")

  # # Check if the column follows the yyyy-mm-dd format
  # formatted_dates <- format(date_var, "%Y-%m-%d")
  # assert_that(all(date_var == as.Date(formatted_dates)), msg = "The date column does not follow the yyyy-mm-dd format.")

  date_lbl <- glue::glue("date_{time_unit}")

  # value_lbl <- deparse(enquo(value_var))

  # Floor the date to the specified time frame
  summary_tbl <- .data |>
    dplyr::mutate(
      date = lubridate::floor_date({{date_var}}, time_unit)
      ,time_unit=time_unit
    ) |>
    dplyr::group_by(date,...) |>
    dplyr::summarise(
      "{{value_var}}":= sum({{value_var}},na.rm=TRUE)
      ,.groups = "drop"
    )


  # Create a calendar table with all the dates in the specified time frame
  calendar_tbl <- tibble::tibble(
    date = base::seq.Date(from = base::min(summary_tbl$date,na.rm=TRUE), to = base::max(summary_tbl$date,na.rm = TRUE), by = time_unit)
  )

  # Perform a full join to ensure all time frames are represented
  full_tbl <- dplyr::full_join(
    calendar_tbl
    ,summary_tbl
    ,by = dplyr::join_by(date==date)
  ) |>
    dplyr::mutate(
      dplyr::across(dplyr::where(\(x) base::is.numeric(x)),\(x) tidyr::replace_na(x,0))
    )


  return(full_tbl)

}


#' Total year to date aggregtation
#'
#' @param .data table
#' @param ... dimensions to aggregrate by
#' @param date_var column with date var to aggregate by
#' @param value_var column with the value to aggregate
#' @param time_unit
#'
#' @return tibble
#' @export
#'
#' @examples
calculate <- function(.data,...,date_var,value_var,time_unit){

  # Validate inputs
  assertthat::assert_that(base::is.data.frame(.data), msg = "data must be a data frame")
  assertthat::assert_that(base::is.character(time_unit), msg = "time_unit must be a character")

  # Aggregate data based on provided time unit

  full_tbl <-  .data |>
    make_aggregation_tbl(...,date_var={{date_var}},value_var={{value_var}},time_unit=time_unit)

  # Determine label for the time unit

  lbl <-   base::names(
    base::match.arg(
      "week"
      ,choices = c("mom"="month","yoy"="year","wow"="week","dod"="day")
      ,several.ok = FALSE
    )
  )


  # Calculate difference and proportional change

  out_tbl <- full_tbl |>
    dplyr::mutate(
      !!lbl:= {{value_var}} - lag({{value_var}}, 1)
      ,prop_delta=  .data[[!!lbl]]/{{value_var}}
    )

  return(out_tbl)

}


#' Total year to date values
#'
#' @param .data tibble of values
#' @param ... optional columns to group by
#' @param date_var column with date var to aggregate by
#' @param value_var column with value to aggregate
#'
#' @return tibble
#' @export
#'
#' @examples
#' totaly  td(sales_tbl,date_var = order_date,value_var = quantity)
totalytd <- function(.data,...,date_var,value_var){

  # Validate inputs
  assertthat::assert_that(base::is.data.frame(.data), msg = "data must be a data frame")

  # Aggregate data based on provided time unit

  full_tbl <-  .data |>
    make_aggregation_tbl(...,date_var={{date_var}},value_var={{value_var}},time_unit="day") |>
    dplyr::mutate(
      year=lubridate::year(date)
      ,.before = 1
    )



  out_tbl <- full_tbl |>
    dplyr::group_by(year) |>
    dplyr::mutate(
    ytd=base::cumsum({{value_var}})
  ) |>
    dplyr::ungroup()

  return(out_tbl)

}



#' Total quarter to date values
#'
#' @param .data tibble of values
#' @param ... optional columns to group by
#' @param date_var column with date var to aggregate by
#' @param value_var column with value to aggregate
#'
#' @return tibble
#' @export
#'
#' @examples
#' totaly  td(sales_tbl,date_var = order_date,value_var = quantity)
totalqtd <- function(.data,...,date_var,value_var){

  # Validate inputs
  assertthat::assert_that(base::is.data.frame(.data), msg = "data must be a data frame")

  # Aggregate data based on provided time unit

  full_tbl <-  .data |>
    make_aggregation_tbl(...,date_var={{date_var}},value_var={{value_var}},time_unit="day") |>
    dplyr::mutate(
      year=lubridate::year(date)
      ,quarter=lubridate::quarter(date)
      ,.before = 1
    )



  out_tbl <- full_tbl |>
    dplyr::group_by(year,quarter) |>
    dplyr::mutate(
      ytd=base::cumsum({{value_var}})
    ) |>
    dplyr::ungroup()

  return(out_tbl)

}




#' Total month to date values
#'
#' @param .data tibble of values
#' @param ... optional columns to group by
#' @param date_var column with date var to aggregate by
#' @param value_var column with value to aggregate
#'
#' @return tibble
#' @export
#'
#' @examples
#' totalmtd(sales_tbl,date_var = order_date,value_var = quantity)
totalmtd <- function(.data,...,date_var,value_var){

  # Validate inputs
  assertthat::assert_that(base::is.data.frame(.data), msg = "data must be a data frame")

  # Aggregate data based on provided time unit

  full_tbl <-  .data |>
    make_aggregation_tbl(...,date_var={{date_var}},value_var={{value_var}},time_unit="day") |>
    dplyr::mutate(
      month=lubridate::month(date)
      ,year=lubridate::year(date)
      ,.before=1
    )

  # Determine label for the time unit


  # Calculate difference and proportional change

  out_tbl <- full_tbl |>
    dplyr::group_by(year,month) |>
    dplyr::mutate(
      mtd=base::cumsum({{value_var}})
    ) |>
    dplyr::ungroup()

  return(out_tbl)

}


#' Total Week to date values
#'
#' @param .data tibble of values
#' @param ... optional columns to group by
#' @param date_var column with date var to aggregate by
#' @param value_var column with value to aggregate
#'
#' @return tibble
#' @export
#'
#' @examples
#' totalwtd(sales_tbl,date_var = order_date,value_var = quantity)
totalwtd <- function(.data,...,date_var,value_var){

  # Validate inputs
  assertthat::assert_that(base::is.data.frame(.data), msg = "data must be a data frame")

  # Aggregate data based on provided time unit

  full_tbl <-  .data |>
    make_aggregation_tbl(...,date_var={{date_var}},value_var={{value_var}},time_unit="day") |>
    mutate(
      week=lubridate::week(date)
      ,month=lubridate::month(date)
      ,year=lubridate::year(date)
      ,.before = 1
    )

  # Determine label for the time unit


  # Calculate difference and proportional change

  out_tbl <- full_tbl |>
    dplyr::group_by(
      week
      ,month
      ,year
    ) |>
    dplyr::mutate(
      wtd=base::cumsum({{value_var}})
    ) |>
    dplyr::ungroup()

  return(out_tbl)

}



#' Total since inception
#'
#' @param .data tibble of values
#' @param ... optional columns to group by
#' @param date_var column with date var to aggregate by
#' @param value_var column with value to aggregate
#'
#' @return tibble
#' @export
#'
#' @examples
#' totalatd(sales_tbl,date_var = order_date,value_var = quantity)
totalatd <- function(.data,...,date_var,value_var){

  # Validate inputs
  assertthat::assert_that(base::is.data.frame(.data), msg = "data must be a data frame")

  # Aggregate data based on provided time unit

  full_tbl <-  .data |>
    make_aggregation_tbl(...,date_var={{date_var}},value_var={{value_var}},time_unit="day") |>
    dplyr::arrange(date)

  # Determine label for the time unit


  # Calculate difference and proportional change

  out_tbl <- full_tbl |>
    dplyr::mutate(
      atd=base::cumsum({{value_var}})
    )

  return(out_tbl)

}


#' Year over year values
#' @description
#' For datasets with daily granularity, this will calculate year over year values with some simple descriptive functions
#'
#' @param .data tibble of values
#' @param ... optional columns to group by
#' @param date_var column with date var to aggregate by
#' @param value_var column with value to aggregate
#'
#' @return tibble
#' @export
#'
#' @examples
#' yoy(sales_tbl,date_var = order_date,value_var = quantity)
yoy <- function(.data,...,date_var,value_var,lag_n=1){

  # Validate inputs
  assertthat::assert_that(base::is.data.frame(.data), msg = "data must be a data frame")

  # Aggregate data based on provided time unit

  full_tbl <-  .data |>
    make_aggregation_tbl(...,date_var={{date_var}},value_var={{value_var}},time_unit="day")

  # Determine label for the time unit


  # Calculate difference and proportional change

  lag_table <- full_tbl |>
    dplyr::mutate(
      date_lag=date %m-% lubridate::years(lag_n)
      ,"{{value_var}}_yoy":={{value_var}}
    ) |>
    select(-c(date,{{value_var}}))


 out_tbl <-  left_join(
    full_tbl
    ,lag_table
    ,by=join_by(date==date_lag)
  )

  return(out_tbl)

}





































#'
#' #' Augment time attributes
#' #'
#' #' @param .data
#' #' @param date_var
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' augment_time_attributes <- function(.data,date_var){
#'
#'
#'   # set up variables
#'
#'
#'   date_var <- enquo(date_var)
#'
#'   #create new vars
#'
#'   .data <-   .data %>%
#'     mutate(
#'       year=lubridate::year(!!date_var)
#'       ,year_abb=stringr::str_sub(as.character(year),start=3,end=4)
#'       ,quarter=lubridate::quarter(!!date_var)
#'       ,month_number=lubridate::month(!!date_var)
#'       ,month_number_padded=if_else(str_length(month_number)<2,paste0("0",month_number),as.character(month_number))
#'       ,month_name_short=lubridate::month(!!date_var,abbr = TRUE,label=TRUE)
#'       ,month_name_long=lubridate::month(!!date_var,label=TRUE,abbr = FALSE)
#'       ,day_of_week_number=wday(!!date_var,label=FALSE)
#'       ,day_of_week_label=lubridate::wday(!!date_var,label=TRUE)
#'       ,day_of_month=lubridate::day(!!date_var)
#'       ,days_in_month=lubridate::days_in_month(!!date_var)
#'       ,week_number_of_year=lubridate::week(!!date_var)
#'       ,leap_year_indicator=lubridate::leap_year(!!date_var)
#'       ,semester=lubridate::semester(!!date_var)
#'       ,semester_year=lubridate::semester(!!date_var,with_year=TRUE)
#'       ,year_month=paste0(year,"_",month_number)
#'       ,year_month_padded=paste0(year,"_",month_number_padded)
#'       ,year_wk=base::paste0(year,"_",1)
#'       ,year_quarter=lubridate::quarter(!!date_var,with_year = TRUE)
#'       ,quarter_year_full=base::paste0(quarter,"Q",year)
#'       ,quarter_year_abb=base::paste0(quarter,"Q",year_abb)
#'      )
#'   #returning object
#' return(.data)
#'
#' }
#'
#'
#' #' create 554 calendar
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' make_554_cal <- function(start_date,end_date){
#'
#' calendar_raw <- tibble(date=seq.Date(from=ymd("2018-02-04"),to=ymd("2023-12-31"),by="days"))
#'
#' calendar_tbl <- calendar_raw %>%
#'   mutate(
#'     date_id=row_number()
#'    ,year_id=
#'
#'   case_when(
#'     date < ymd("2019-02-03") ~ tibble(yr_key=1,yr_label=2018)
#'     ,date < ymd("2020-02-02") ~ tibble(yr_key=2,yr_label=2019)
#'     ,date < ymd("2021-01-31") ~ tibble(yr_key=3,yr_label=2020)
#'     ,date < ymd("2022-01-30") ~ tibble(yr_key=4,yr_label=2021)
#'     ,date < ymd("2023-01-29") ~ tibble(yr_key=5,yr_label=2022)
#'     ,TRUE ~ tibble(yr_key=6,yr_label=2023)
#'   )
#' ) %>%
#'   group_by(
#'     year_id
#'   ) %>%
#'   mutate(
#'     day_id=row_number()
#'     ,wk_period=
#'       # cumsum(
#'         case_when(
#'           day_id%%7==0 ~ day_id
#'           ,TRUE ~NA_integer_
#'           )
#'       # )+1
#'   ) %>%
#'   fill(wk_period,.direction = "up") %>%
#'   mutate(
#'     wk_period=wk_period/7
#'   ) %>%
#'   ungroup()
#' return(calendar_tbl)
#' }
