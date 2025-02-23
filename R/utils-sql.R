#' Helps creates CTE queries
#'
#' @param query sql query
#' @param query_name CTE name
#' @param order CTE order either first, middle, last or single
#'
#' @return sql text object
#' @export
#'
#' @examples
#' query <- dplyr::sql("select * from sales")
#' with(query=query,query_name=sales_cte,order='first')
with <- function(query,query_name,order=c("middle")){


  query_name <- rlang::as_label(rlang::enquo(query_name))

  initial <- dplyr::sql(
    paste0("WITH ",query_name," AS ","(",query,"),\n")
  )

  middle <- dplyr::sql(
    paste0("\n",query_name," AS ","(",query,"),\n")
  )


  last <- dplyr::sql(
    paste0("\n",query_name," AS ","(",query,")\n")
  )

  single <- dplyr::sql(
    paste0("WITH ",query_name," AS ","(",query,")")
  )



  ordec_vec <- match.arg(
    stringr::str_to_lower(order)
    ,choices=c("first","last","middle","single")
  )

  if(ordec_vec=="first"){

    return(initial)

  }else if(ordec_vec=="middle"){

    return(middle)

  }else if(ordec_vec=="single"){

    return(single)

  }else{

    return(last)

  }
}

#' Capture previous query steps
#'
#' @param .data dbi object
#'
#' @return sql object
#' @export
#'
#' @examples
#' sales_db |> capture_original_query()
capture_original_query <- function(.data) {
  original_sql <- dplyr::sql(
    paste(
      "(",
      dbplyr::remote_query(.data),
      ")"
    )
  )

  return(original_sql)
}


## calendar table--------






#' Create a calendar table in sql
#'
#' @param start_date calendar start date in YYYY-MM-DD format
#' @param end_date calendar end date in YYYY-MM-DD format
#' @param time_unit calendar table unit in 'day', 'week', 'month', 'quarter' or 'year'
#' @param con database connection
#'
#' @return DBI object
#' @export
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


#' CTE
#'
#' @param con the DBI connection
#' @param ...
#'
#' @return a list of DBI objects and SQL queries
#' @export
#'
#' @examples
#' cte(with(sql1,sales),with(sql2,summary),dplyr::sql("select * from summary"))
cte <- function(con,...){

  safe_tbl <- purrr::possibly(dplyr::tbl,otherwise = NA)

  vars_quos <- rlang::enquos(...)

  vars_exprs <- lapply(vars_quos, rlang::eval_tidy)


  # Combine the queries into a single SQL statement
  combined_query <- paste(vars_exprs, collapse = " ",sep = "\n")

  # Execute the combined query
    dbi <- safe_tbl(con,dplyr::sql(combined_query))

  out <- list(
    dbi=dbi
    ,sql=dplyr::sql(combined_query)
  )
  return(out)
}







#' Create duckdb versions of Contoso datasets
#'
#' @return DBI objects
#' @export
#'
#' @examples
#' create_contonso_duckdb()
create_contonso_duckdb <- function(){

  con <- suppressWarnings(DBI::dbConnect(duckdb::duckdb()))


  duckdb::duckdb_register(con,"sales",fpaR::sales,overwrite = TRUE)
  duckdb::duckdb_register(con,"product",fpaR::product,overwrite = TRUE)
  duckdb::duckdb_register(con,"customer",fpaR::customer,overwrite = TRUE)
  duckdb::duckdb_register(con,"date",fpaR::date,overwrite = TRUE)
  duckdb::duckdb_register(con,"fx",fpaR::fx,overwrite = TRUE)
  duckdb::duckdb_register(con,"store",fpaR::store,overwrite = TRUE)


  sales <- dplyr::tbl(con,dplyr::sql("select * from sales"))
  product <- dplyr::tbl(con,dplyr::sql("select * from product"))
  customer <- dplyr::tbl(con,dplyr::sql("select * from customer"))
  store <- dplyr::tbl(con,dplyr::sql("select * from store"))
  fx <- dplyr::tbl(con,dplyr::sql("select * from fx"))
  date <- dplyr::tbl(con,dplyr::sql("select * from date"))


  out <- base::list(
    sales=sales
    ,product=product
    ,customer=customer
    ,store=store
    ,fx=fx
    ,date=date
  )


  return(out)
}

#' Title
#'
#' @param x tibble or dbi object
#'
#' @returns dbi object
#'
#' @examples
#' make_db_tbl(sales)
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
