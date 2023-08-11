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


  # set up variables


  date_var <- enquo(date_var)

  #create new vars

  .data <-   .data %>%
    mutate(
      year=lubridate::year(!!date_var)
      ,year_abb=stringr::str_sub(as.character(year),start=3,end=4)
      ,quarter=lubridate::quarter(!!date_var)
      ,month_number=lubridate::month(!!date_var)
      ,month_number_padded=if_else(str_length(month_number)<2,paste0("0",month_number),as.character(month_number))
      ,month_name_short=lubridate::month(!!date_var,abbr = TRUE,label=TRUE)
      ,month_name_long=lubridate::month(!!date_var,label=TRUE,abbr = FALSE)
      ,day_of_week_number=wday(!!date_var,label=FALSE)
      ,day_of_week_label=lubridate::wday(!!date_var,label=TRUE)
      ,day_of_month=lubridate::day(!!date_var)
      ,days_in_month=lubridate::days_in_month(!!date_var)
      ,week_number_of_year=lubridate::week(!!date_var)
      ,leap_year_indicator=lubridate::leap_year(!!date_var)
      ,semester=lubridate::semester(!!date_var)
      ,semester_year=lubridate::semester(!!date_var,with_year=TRUE)
      ,year_month=paste0(year,"_",month_number)
      ,year_month_padded=paste0(year,"_",month_number_padded)
      ,year_wk=base::paste0(year,"_",lubridate::week(!!date_var))
      ,year_quarter=lubridate::quarter(!!date_var,with_year = TRUE)
      ,quarter_year_full=base::paste0(quarter,"Q",year)
      ,quarter_year_abb=base::paste0(quarter,"Q",year_abb)
     )
  #returning object
return(.data)

}

contoso_sales_spk %>%
  augment_time_attributes(DateKey)




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
    date < ymd("2019-02-03") ~ tibble(yr_key=1,yr_label=2018)
    ,date < ymd("2020-02-02") ~ tibble(yr_key=2,yr_label=2019)
    ,date < ymd("2021-01-31") ~ tibble(yr_key=3,yr_label=2020)
    ,date < ymd("2022-01-30") ~ tibble(yr_key=4,yr_label=2021)
    ,date < ymd("2023-01-29") ~ tibble(yr_key=5,yr_label=2022)
    ,TRUE ~ tibble(yr_key=6,yr_label=2023)
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
  ) %>%
  ungroup()
return(calendar_tbl)
}
#
# df <- make_554_cal()
#
#
# df %>%
#   summarise(
#     min_date=min(date)
#     ,max_date=max(date)
#     ,.by = yr_label
#   ) %>%
#   mutate(
#     delta=min_date-max_date
#   )
#
#
# x <- 364
# x <- list(
# x=2018:1998
# ,y=1:21
# ) %>%
#   deframe()
#
#
# map(x,~ymd("2019-02-03")-364*.x)
#
#
# start_date=XXXX
# create year is XXX + 364
#
#
# start_date <- ymd("2019-02-03")
#
# end_date <- ymd("2024-01-01")
#
#
# delta <- as.numeric(end_date-start_date)
#
#
# delta_floor <- floor(delta/364)
#
#
# map(1:delta_floor,~ymd(start_date)-364*.x)
#
#
# calendar_raw <- tibble(date=seq.Date(from=ymd("2018-02-04"),to=ymd("2023-12-31"),by="days"))
#
# calendar_raw %>%
#   mutate(
#     date_id=row_number()
#     ,
#
#       case_when(
#         date < ymd("2019-02-03") ~ tibble(yr_key=1,yr_label=2019)
#         ,date < ymd("2020-02-02") ~ tibble(yr_key=2,yr_label=2020)
#         ,date < ymd("2021-01-31") ~ tibble(yr_key=3,yr_label=2021)
#         ,date < ymd("2022-01-30") ~ tibble(yr_key=4,yr_label=2022)
#         ,date < ymd("2023-01-29") ~ tibble(yr_key=5,yr_label=2023)
#         ,TRUE ~ tibble(yr_key=6,yr_label=2024)
#       )
#   )
#
