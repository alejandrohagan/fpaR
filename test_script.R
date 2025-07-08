


x <- wtd(sales,order_date,margin,"standard")


devtools::document()


x |> complete_calendar()

x |> class()

calendar_dbi<- x@data@data |>
  count(!!x@data@date_quo) |>
  select(-n)


date_vec <- x@data@date_vec

out <- calendar_dbi |>
  dplyr::mutate(
    year_start_date=lubridate::floor_date(!!x@data@date_quo,unit = "year")
    ,year_end_date=sql(glue::glue("date_trunc('year', {date_vec}) + INTERVAL '1' YEAR"))
    ,quarter_start_date=lubridate::floor_date(!!x@data@date_quo,unit = "quarter")
    ,quarter_end_date=sql(glue::glue("date_trunc('quarter', {date_vec}) + INTERVAL '1' quarter"))
    ,month_start_date=lubridate::floor_date(!!x@data@date_quo,unit = "month")
    ,month_end_date=sql(glue::glue("date_trunc('month', {date_vec}) + INTERVAL '1' month"))
    ,week_start_date=lubridate::floor_date(!!x@data@date_quo,unit = "week")
    ,week_end_date=sql(glue::glue("date_trunc('month', {date_vec}) + INTERVAL '1' month"))
    ,day_of_week=lubridate::wday(!!x@data@date_quo,label = FALSE)
    ,day_of_week_label=lubridate::wday(!!x@data@date_quo,label = TRUE)
    ,days_in_year=year_end_date-year_start_date
    ,days_in_quarter=quarter_end_date-quarter_start_date
    ,days_in_month=sql(glue::glue("last_day({date_vec})"))
    ,days_complete_in_week=!!x@data@date_quo-week_start_date
    ,days_remaining_in_week=week_end_date-!!x@data@date_quo
    ,days_remaining_in_quarter=quarter_end_date-!!x@data@date_quo
    ,days_remaining_in_month=month_end_date-!!x@data@date_quo
    ,days_remaining_in_year=year_end_date-!!x@data@date_quo
    ,days_complete_in_year=!!x@data@date_quo-year_start_date
    ,days_complete_in_quarter=!!x@data@date_quo-quarter_start_date
    ,days_complete_in_month=!!x@data@date_quo-month_start_date
    ,days_complete_in_year=!!x@data@date_quo-year_start_date
    ,weekend_indicator=if_else(day_of_week_label %in% c("Saturday","Sunday"),1,0)
  ) |>
  mutate(
    across(contains("date"),\(x) as.Date(x))
  )

return(out)

