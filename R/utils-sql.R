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






#' Create a calendar table in sql (standalone or part of CTE)
#'
#' @param start_date calendar start date in YYYY-MM-DD format
#' @param end_date calendar end date in YYYY-MM-DD format
#' @param time_unit calendar table unit in day, week, month, quarter or year
#' @param cte logical indicator generate sql query to be used as part of a CTE
#' @param con database connection
#'
#' @return DBI object
#' @export
#'
#' @examples
#' con <- DBI::dbConnect(drv = duckdb::duckdb())
#' seq_date_sql(start_date = "2015-01-01", end_date = "2024-04-20", time_unit = "day", cte = FALSE, con = con)
seq_date_sql <- function(start_date, end_date, time_unit, cte = TRUE, con) {
  ## prepare and validate inputs----------



  start_date_vec <- convert_input_to_string({{ start_date }})
  end_date_vec <- convert_input_to_string({{ end_date }})
  time_unit_vec <- convert_input_to_string({{ time_unit }})
  interval_key <- paste("1", time_unit_vec)



  assertthat::assert_that(
    time_unit_vec %in% c("day", "week", "month", "quarter", "year"),
    msg = "Please have time unit match 'day', 'week','month','quarter' or 'year'"
  )

  assertthat::assert_that(
    base::is.logical(cte),
    msg = "Please select TRUE or FALSE if the return query should be a CTE or standalone query"
  )


  assertthat::assert_that(
    is_yyyy_mm_dd(start_date_vec) & is_yyyy_mm_dd(end_date_vec),
    msg = "Please ensure dates are in YYYY-MM-DD format"
  )

  assertthat::assert_that(
    lubridate::ymd(start_date_vec) < lubridate::ymd(end_date_vec),
    msg = "Please ensure end date is greater than start date"
  )

  con_info <- paste0("connection: ", DBI::dbGetInfo(con)$dbname)

  assertthat::assert_that(
    DBI::dbIsValid(con),
    msg = paste("Please check if your connection is valid", con_info)
  )

  ## calendar CTE query----------


  ### generate series of dates

  date_series_sql <- fpaR::with(
    query = glue::glue_sql("

    SELECT

    GENERATE_SERIES(
       MIN(DATE_TRUNC({time_unit_vec}, DATE {start_date_vec}))::DATE
      ,MAX(DATE_TRUNC({time_unit_vec}, DATE {end_date_vec}))::DATE
      ,INTERVAL {interval_key}
    ) AS DATE_LIST"
    ,.con = con
    )
    ,query_name = DATE_SERIES
    ,order = "first"
  )

  ## Expand series as last query
  calendar_tbl_last_sql <- fpaR::with(
    query=dplyr::sql(
      "
      SELECT

      UNNEST(DATE_LIST)::DATE AS date

      FROM DATE_SERIES

      "
    )
    ,query_name = CALENDAR_TBL
    ,order = "last"
  )


  ## Expand series as middle query


  calendar_tbl_middle_sql <- fpaR::with(
    query=dplyr::sql(
      "
      SELECT

      UNNEST(DATE_LIST)::DATE AS date

      FROM DATE_SERIES

      "
    )
    ,query_name = CALENDAR_TBL
    ,order = "middle"
  )

  ## no CTE returns dbi

  no_cte_sql <- cte(
    con = con
    ,dplyr::sql(date_series_sql)
    ,sql(calendar_tbl_last_sql)
    ,dplyr::sql("select * from CALENDAR_TBL")
  )

  ## does not return dbi

  cte_sql <- cte(
    con = con
    ,dplyr::sql(date_series_sql)
    ,sql(calendar_tbl_middle_sql)
  )

  ## print alerts and return objects-------

  cli::cli_alert_info("{con_info} database")

  if (cte) {
    return(cte_sql)
  } else {
    return(no_cte_sql)
  }
}


#' sql_query_select
#'
#' @param .data DBI object
#' @param previous_query Logical indicator if the from value should reference the previous .data steps
#' @param select the select values
#' @param from the from caluse
#' @param where the where clause
#' @param group_by the group by clause
#' @param having the having clause
#' @param window the window operation
#' @param order_by the order by caluse
#' @param limit the limit
#' @param distinct the distinct values
#'
#' @return sql and con object
#' @export
#'
#' @examples
#' sql_query_select(db_sales,previous_query=TRUE,select=sql(select *))
sql_query_select <- function(
    .data,
    previous_query,
    select,
    from,
    where = NULL,
    group_by = NULL,
    having = NULL,
    window = NULL,
    order_by = NULL,
    limit = NULL,
    distinct = FALSE){

  con <- dbplyr::remote_con(.data)
  previous_query_sql=dbplyr::remote_query(.data)


  if(previous_query){

    sql <-  dbplyr::sql_query_select(
      con=con
      ,from=dplyr::sql(paste0("(",previous_query_sql,")"))
      ,where=where
      ,group_by=group_by
      ,select = select
      ,having = having
      ,window = window
      ,order_by = order_by
      ,limit = limit
      ,distinct = distinct
    )
out <- list(con=con,sql=sql)

    return(out)

  }else{

    sql <- dbplyr::sql_query_select(
      con=con
      ,from=from
      ,where=where
      ,group_by=group_by
      ,select = select
      ,having = having
      ,window = window
      ,order_by = order_by
      ,limit = limit
      ,distinct = distinct
    )
    out <- list(
      con=con
      ,sql=sql
      )

    return(out)

  }
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


