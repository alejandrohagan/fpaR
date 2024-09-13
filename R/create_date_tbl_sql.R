#' Create table of dates in sql
#' @description
#' Create a sql script of date table range for use in downstream queries
#'
#' @param .data dbi object
#' @param start_date start date of date table range
#' @param end_date end date of date table range
#'
#' @return a sql query
#'
#' @examples
#' create_date_sql(mtcars_dbi,start_date="2021-01-01",end_date="2022-01-01")
create_date_sql <- function(.data,start_date,end_date){

  ## create validation tests



  ##---- assign variables


  start_date_vec <-  base::substitute(start_date)

  end_date_vec <- base::substitute(end_date)

  ## create sql

  date_sql <- glue::glue_sql(
    "


 WITH generate_date AS (

        SELECT CAST(RANGE AS DATE) AS date_key
        FROM RANGE(DATE {start_date_vec}, DATE {end_date_vec}, INTERVAL 1 DAY)

          )


   SELECT date_key AS date_key,
          DAYOFYEAR(date_key) AS day_of_year,
          YEARWEEK(date_key) AS week_key,
          WEEKOFYEAR(date_key) AS week_of_year,
          DAYOFWEEK(date_key) AS day_of_week,
          ISODOW(date_key) AS iso_day_of_week,
          DAYNAME(date_key) AS day_name,
          DATE_TRUNC('week', date_key) AS first_day_of_week,
          DATE_TRUNC('week', date_key) + 6 AS last_day_of_week,
          YEAR(date_key) || RIGHT('0' || MONTH(date_key), 2) AS month_key,
          MONTH(date_key) AS month_of_year,
          DAYOFMONTH(date_key) AS day_of_month,
          LEFT(MONTHNAME(date_key), 3) AS month_name_short,
          MONTHNAME(date_key) AS month_name,
          DATE_TRUNC('month', date_key) AS first_day_of_month,
          LAST_DAY(date_key) AS last_day_of_month,
          CAST(YEAR(date_key) || QUARTER(date_key) AS INT) AS quarter_key,
          QUARTER(date_key) AS quarter_of_year,
          CAST(date_key - DATE_TRUNC('Quarter', date_key) + 1 AS INT) AS day_of_quarter,
          ('Q' || QUARTER(date_key)) AS quarter_desc_short,
          ('Quarter ' || QUARTER(date_key)) AS quarter_desc,
          DATE_TRUNC('quarter', date_key) AS first_day_of_quarter,
          LAST_DAY(DATE_TRUNC('quarter', date_key) + INTERVAL 2 MONTH) as last_day_of_quarter,
          CAST(YEAR(date_key) AS INT) AS year_key,
          DATE_TRUNC('Year', date_key) AS first_day_of_year,
          DATE_TRUNC('Year', date_key) - 1 + INTERVAL 1 YEAR AS last_day_of_year,
          ROW_NUMBER() OVER (PARTITION BY YEAR(date_key), MONTH(date_key), DAYOFWEEK(date_key) ORDER BY date_key) AS ordinal_weekday_of_month
     FROM generate_date

    "
 ,.con = .data$src[[1]]

 )

  return(date_sql)

}


