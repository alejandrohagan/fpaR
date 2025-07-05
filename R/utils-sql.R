
## calendar table--------


#' Create a calendar table in sql
#'
#' @param start_date calendar start date in YYYY-MM-DD format
#' @param end_date calendar end date in YYYY-MM-DD format
#' @param time_unit calendar table unit in 'day', 'week', 'month', 'quarter' or 'year'
#' @param con database connection
#' @export
#' @return DBI object
#'
#' @examples
#' con <- DBI::dbConnect(drv = duckdb::duckdb())
#' seq_date_sql(start_date = "2015-01-01", end_date = "2024-04-20", time_unit = "day", con = con)
seq_date_sql <- function(start_date,end_date,time_unit,con){

  # start_date <- "2022-01-01"
  # end_date <- "2023-01-01"
  # time_unit <- "day"

  # error check

    assertthat::assert_that(
      time_unit %in% c("day", "week", "month", "quarter", "year"),
      msg = "Please have time unit match 'day', 'week','month','quarter' or 'year'"
    )

    assertthat::assert_that(
      is_yyyy_mm_dd(start_date) & is_yyyy_mm_dd(end_date),
      msg = "Please ensure dates are in YYYY-MM-DD format"
    )

    assertthat::assert_that(
      lubridate::ymd(start_date) < lubridate::ymd(end_date),
      msg = "Please ensure end date is greater than start date"
    )

    con_info <- paste0("connection: ", DBI::dbGetInfo(con)$dbname)

    assertthat::assert_that(
      DBI::dbIsValid(con),
      msg = paste("Please check if your connection is valid", con_info)
    )

    # create variables


    time_interval <- paste("1",time_unit)

    date_seq_sql <- glue::glue_sql("
  WITH DATE_SERIES AS (
  SELECT

  GENERATE_SERIES(
     MIN(DATE_TRUNC({time_unit}, DATE {start_date}::date))::DATE
    ,MAX(DATE_TRUNC({time_unit}, DATE {end_date}::date))::DATE
    ,INTERVAL {time_interval}
  ) AS DATE_LIST),

  CALENDAR_TBL AS (
        SELECT

        UNNEST(DATE_LIST)::DATE AS date

        FROM DATE_SERIES

        )
  SELECT *
  FROM CALENDAR_TBL

",.con=con)

    out <- dplyr::tbl(con,dplyr::sql(date_seq_sql))

    return(out)

}


#' Make an in memory database from a table
#'
#' @param x tibble or dbi object
#' @export
#' @returns dbi object
#'
make_db_tbl <- function(x){


  assertthat::assert_that(
    any(class(x) %in% c("tbl_dbi","data.frame"))
    ,msg = "Please use class dbi or tibble"
  )



  if(any(class(x) %in% c("tbl_dbi"))){

    return(x)

  }

  if(is.data.frame(x)){

    groups_lst <- dplyr::groups(x)

    con <- DBI::dbConnect(duckdb::duckdb(tempfile()))

    duckdb::duckdb_register(con,name = "x",df = x,overwrite = TRUE)


    out <- dplyr::tbl(con,"x") |>
      dplyr::group_by(groups_lst)

    return(out)

  }


}

