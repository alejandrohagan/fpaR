
#' Augment time attributes
#'
#' @param .data
#' @param date_var
#'
#' @return
#' @export
#'
#' @examples
augment_time_attributes <- function(.data,date_var){

  date_var <- enquo(date_var)
.data <-   .data %>%
    mutate(
      year=lubridate::year(!!date_var)
      ,quarter=lubridate::quarter(!!date_var,with_year = FALSE)
      ,year_quarter=lubridate::quarter(!!date_var,with_year = TRUE)
      ,month=lubridate::month(!!date_var)
      ,month_name_short=lubridate::month(!!date_var,abbr = TRUE,label=TRUE,)
      ,month_name_long=lubridate::month(!!date_var,label=TRUE,abbr = FALSE)
      ,year_month=paste0(year,"_",month)
      ,week=lubridate::week(!!date_var)
      ,leap_year_indicator=leap_year(!!date_var)
      ,semester=lubridate::semester(!!date_var)
      ,semester_year=lubridate::semester(!!date_var,with_year=TRUE)
    )
return(.data)

}


#' create 554 calendar
#'
#' @return
#' @export
#'
#' @examples
make_554_cal <- function(start_date,end_date){

calendar_raw <- tibble(date=seq.Date(from=ymd("2018-02-04"),to=ymd("2023-12-31"),by="days"))

calendar_tbl <- calendar_raw %>%
  mutate(
    date_id=row_number()
   ,year_id=

  case_when(
    date < ymd("2019-02-03") ~ 1
    ,date < ymd("2020-02-02") ~ 2
    ,date < ymd("2021-01-31") ~ 3
    ,date < ymd("2022-01-30") ~ 4
    ,date < ymd("2023-01-29") ~ 5
    ,TRUE ~ 6
  )
) %>%
  group_by(
    year_id
  ) %>%
  mutate(
    day_id=row_number()
    ,wk_period=
      # cumsum(
        case_when(
          day_id%%7==0 ~ day_id
          ,TRUE ~NA_integer_
          )
      # )+1
  ) %>%
  fill(wk_period,.direction = "up") %>%
  mutate(
    wk_period=wk_period/7
    ,yr_label=
      case_when(
        year_id==1 ~ 2018
        ,year_id==2 ~ 2029
        ,year_id==3 ~ 2020
        ,year_id==4 ~ 2022
        ,year_id==5 ~ 2023
        ,TRUE ~ 2024
        )
  )
return(calendar_tbl)
}





