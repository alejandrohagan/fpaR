library(tidyverse)
devtools::document()
devtools::load_all()




db <-  create_database_data()








sql1 <- "select * from sales"

sql2 <- "select * from sales_cte limit 1000"

sql3 <- "select customer_key, sum(quantity) as quant from abb group by all"


sql_query_select_x <- function(
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

 out <-  dbplyr::sql_query_select(
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

 return(out)

  }else{

    out <- dbplyr::sql_query_select(
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

    return(out)

  }
}


first_sql <- sql_query_select_x(
  sales_db
  ,previous_query = TRUE
  ,select=sql("sum(quantity) as quantity,customer_key")
  ,group_by=sql("ALL")
  )


second_sql <- sql_query_select_x(
  sales_db
  ,previous_query = TRUE
  ,select=sql("mean(net_price) as mean_price, customer_key")
  ,group_by=sql("ALL")
)


third_sql <- sql_query_select_x(
  sales_db
  ,previous_query = TRUE
  ,select=sql("mean(unit_cost) as mean_cost, customer_key")
  ,group_by=sql("ALL")
)



cte <- function(con,...){

  vars_quos <- rlang::enquos(...)

  vars_exprs <- lapply(vars_quos, rlang::eval_tidy)

  print(vars_exprs)


  # Combine the queries into a single SQL statement
  combined_query <- paste(vars_exprs, collapse = " ",sep = "\n")
  print(combined_query)

  # Execute the combined query
  out <- dplyr::tbl(con, dplyr::sql(combined_query))
  cat(combined_query)
  return(out)
}





with(
  query=
    "select * from {previous_query}"
)



cte(
  con=con
  ,with(
    query = first_sql
    ,query_name = quant
    ,order = "first")
  ,with(query = second_sql,query_name = price,order = "middle")
  ,with(query = third_sql,query_name = cost,order = "last")
  ,sql("
       select
       q.customer_key
       ,q.quantity
       ,p.mean_price

       from quant q
       left join price p on

       q.customer_key=p.customer_key

       ")
)



tbl(con,test_sql)

dbplyr::remote_query(sales_db)

db1 <- DBI::dbConnect(duckdb::duckdb())
f <- tempfile("mtcars", fileext = ".csv")
write.csv(mtcars, f)

library(tidyverse)

usethis::use_r("utils-misc")


#' Create a calendar table in sql (standalone or part of CTE)
#'
#' @param start_date calendar start date in YYYY-MM-DD format
#' @param end_date calendar end date in YYYY-MM-DD format
#' @param time_unit calendar table unit in day, week, month, quarter or year
#' @param cte logical indicator generate sql query to be used as part of a CTE
#' @param con database connection
#'
#' @return
#' @export
#'
#' @examples
#' con <- DBI::dbConnect(drv = duckdb::duckdb())
#' create_calendar_sql(start_date = "2015-01-01", end_date = "2024-04-20", time_unit = "day", cte = FALSE, con = con)
create_calendar_sql <- function(start_date, end_date, time_unit, cte = TRUE, con) {
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

  assertthat::assert_that(
    DBI::dbIsValid(con),
    msg = paste("Please check if your connection is valid", con_info)
  )

  con_info <- paste0("connection: ", DBI::dbGetInfo(con)$dbname)


  ## calendar CTE query----------

  calendar_sql_cte <- glue::glue_sql("

WITH DATE_SERIES AS (

    SELECT

    GENERATE_SERIES(
       MIN(DATE_TRUNC({time_unit_vec}, DATE {start_date_vec}))::DATE
      ,MAX(DATE_TRUNC({time_unit_vec}, DATE {end_date_vec}))::DATE
      ,INTERVAL {interval_key}
    ) AS DATE_LIST

),

CALENDAR_TBL AS (

SELECT

UNNEST(DATE_LIST)::DATE AS date

FROM DATE_SERIES
),
", .con = con)

  ## calendar SQL no CTE------------

  calendar_sql_no_cte <- glue::glue_sql("

WITH DATE_SERIES AS (

    SELECT

    GENERATE_SERIES(
       MIN(DATE_TRUNC({time_unit_vec}, DATE {start_date_vec}))::DATE
      ,MAX(DATE_TRUNC({time_unit_vec}, DATE {end_date_vec}))::DATE
      ,INTERVAL {interval_key}
    ) AS DATE_LIST

),

CALENDAR_TBL AS (

SELECT

UNNEST(DATE_LIST)::DATE AS date

FROM DATE_SERIES
)

SELECT *
FROM CALENDAR_TBL

", .con = con)

  ## print alerts and return objects-------

  cli::cli_alert_info("{con_info} database")

  if (cte) {
    return(calendar_sql_cte)
  } else {
    return(calendar_sql_no_cte)
  }
}


create_calendar_sql(start_date = "2015-01-01", end_date = "2024-04-20", time_unit = "day", cte = TRUE, con = con)

#' Title
#'
#' @param .data
#'
#' @return
#' @export
#'
#' @examples
capture_original_query <- function(.data) {
  original_sql <- sql(
    paste(
      "(",
      dbplyr::remote_query(.data),
      ")"
    )
  )

  return(original_sql)
}

make_aggregation_sql <- function(.data, ..., date_var, value_var, time_unit) {
  ## create inputs----------

  original_query <- capture_original_query(.data)

  min_date <- .data |>
    summarize(
      min = min({{ date_var }}, na.rm = TRUE)
    ) |>
    pull(min)

  print(min_date)

  max_date <- .data |>
    summarize(
      max = max({{ date_var }}, na.rm = TRUE)
    ) |>
    pull(max)
  print(max_date)

  date_var <- convert_input_to_string({{ date_var }})
  print(date_var)
  value_var <- convert_input_to_string({{ value_var }})
  time_unit <- convert_input_to_string({{ time_unit }})

  interval_key <- paste("1", time_unit)

  args <- substitute(list(...))[-1]
  group_var <- sapply(args, convert_input_to_string)

  con <- dbplyr::remote_con(sales_db)



  ## no group summary table-------

  no_group_sql1 <- glue::glue_sql("


SUMMARY_TBL AS (

    SELECT

      DATE_TRUNC({time_unit}, {`date_var`})::DATE AS date

     ,SUM({`value_var`}) AS {`value_var`}

    FROM
",
    .con = con
  )

  no_group_sql2 <- glue::glue_sql("
    GROUP BY ALL

  )


  SELECT

   CALENDAR_TBL.date
  ,COALESCE(SUMMARY_TBL.{`value_var`}, 0) AS {`value_var`}

  FROM

  CALENDAR_TBL

  LEFT JOIN

  SUMMARY_TBL ON

  CALENDAR_TBL.date = SUMMARY_TBL.date

  ORDER BY

  SUMMARY_TBL.date
", .con = con)






  ## no group summary table--------


  group_sql1 <- glue::glue_sql("

SUMMARY_TBL AS (

     SELECT

     DATE_TRUNC({time_unit}, {`date_var`}) AS date

    ,SUM({`value_var`}) AS {`value_var`}

    ,{`group_var`*}

    FROM

",
    .con = con
  )


  group_sql2 <- glue::glue_sql("
    GROUP BY ALL
)

  SELECT

  CALENDAR_TBL.date
  ,SUMMARY_TBL.{`group_var`*}
  ,COALESCE(SUMMARY_TBL.{`value_var`}, 0) AS {`value_var`}

  FROM
  CALENDAR_TBL
  LEFT JOIN
  SUMMARY_TBL
  ON
  CALENDAR_TBL.date = SUMMARY_TBL.date
  ORDER BY
  CALENDAR_TBL.date", .con = con)


  ## putting it all together --------------

  group_sql <- dplyr::sql(base::paste(group_sql1, original_query, group_sql2))

  no_group_sql <- base::paste(no_group_sql1, original_query, no_group_sql2)

  calendar_sql <- create_calendar_sql(start_date = {{ min_date }}, end_date = {{ max_date }}, time_unit = {{ time_unit }}, cte = TRUE, con = con)


  ## return logic---------------

  if (missing(...)) {
    out <- paste0(
      calendar_sql,
      no_group_sql
    )
  } else {
    out <- paste0(
      calendar_sql,
      group_sql
    )
  }
  return(dplyr::tbl(con, dplyr::sql(out)))
}



make_aggregation_sql2test <- function(.data, ..., date_var, value_var, time_unit) {
  ## create inputs----------

  original_query <- capture_original_query(.data)


  group_vars <- convert_dots_to_string(...)


  summary_group_vars <- paste0("SUMMARY_TBL.",group_vars)
  unique_group_vars <- paste0("UNIQUE_GROUPS.",group_vars)
  cross_joined_group_vars <- paste0("CROSS_JOINED.",group_vars)


  initial_join_sql <- glue_sql("LEFT JOIN
  SUMMARY_TBL
  ON
  CROSS_JOINED.date               = SUMMARY_TBL.date"
  ,.con=con)

  join_group_vars <-dplyr::sql(paste0("AND ",cross_joined_group_vars,"=",summary_group_vars))



  min_date <- .data |>
    summarize(
      min = min({{ date_var }}, na.rm = TRUE)
    ) |>
    pull(min)

  print(min_date)

  max_date <- .data |>
    summarize(
      max = max({{ date_var }}, na.rm = TRUE)
    ) |>
    pull(max)
  print(max_date)

  date_var <- convert_input_to_string({{ date_var }})
  print(date_var)
  value_var <- convert_input_to_string({{ value_var }})
  time_unit <- convert_input_to_string({{ time_unit }})

  interval_key <- paste("1", time_unit)

  con <- dbplyr::remote_con(sales_db)



  ## no group summary table-------

  no_group_sql1 <- glue::glue_sql("


SUMMARY_TBL AS (

    SELECT

      DATE_TRUNC({time_unit}, {`date_var`})::DATE AS date

     ,SUM({`value_var`}) AS {`value_var`}

    FROM
",
.con = con
  )

  no_group_sql2 <- glue::glue_sql("
    GROUP BY ALL

  )


  SELECT

   CALENDAR_TBL.date
  ,COALESCE(SUMMARY_TBL.{`value_var`}, 0) AS {`value_var`}

  FROM

  CALENDAR_TBL

  LEFT JOIN

  SUMMARY_TBL ON

  CALENDAR_TBL.date = SUMMARY_TBL.date

  ORDER BY

  SUMMARY_TBL.date
", .con = con)






  ## no group summary table--------


  group_sql1 <- glue::glue_sql("

SUMMARY_TBL AS (

     SELECT

     DATE_TRUNC({time_unit}, {`date_var`}) AS date

    ,SUM({`value_var`}) AS {`value_var`}

    ,{`group_vars`*}

    FROM

",
.con = con
  )


  group_sql2 <- glue::glue_sql("
    GROUP BY ALL
),

UNIQUE_GROUPS AS (
    SELECT DISTINCT {summary_group_vars*}
    FROM SUMMARY_TBL
),

CROSS_JOINED AS (
    SELECT
        CALENDAR_TBL.date,
        {unique_group_vars*}
    FROM
        CALENDAR_TBL
    CROSS JOIN
        UNIQUE_GROUPS
)

SELECT
    CROSS_JOINED.date,
    {summary_group_vars*},
    COALESCE(SUMMARY_TBL.{`value_var`}, 0) AS {`value_var`}
FROM
    CROSS_JOINED
  ", .con = con)


## putting it all together --------------

group_sql <-base::paste(group_sql1, original_query, group_sql2)

no_group_sql <- base::paste(no_group_sql1, original_query, no_group_sql2)

calendar_sql <- create_calendar_sql(start_date = {{ min_date }}, end_date = {{ max_date }}, time_unit = {{ time_unit }}, cte = TRUE, con = con)


## return logic---------------

if (missing(...)) {
  out <- paste0(
    calendar_sql,
    no_group_sql
  )
} else {

  out <- paste0(
    calendar_sql
    ,group_sql
    ,initial_join_sql
    ,join_group_vars
  )
  print(sql(out))
}
return(dplyr::tbl(con, dplyr::sql(out)))


}




out_dbi <- make_aggregation_sql2test(.data = sales_db, customer_key, product_key, store_key, date_var = order_date, value_var = quantity, time_unit = "day")
out_dbi <- make_aggregation_sql(.data = sales_db,date_var = order_date, value_var = quantity, time_unit = "day")


test_sql <- sql("
    WITH DATE_SERIES AS (

    SELECT

    GENERATE_SERIES(
       MIN(DATE_TRUNC('day', DATE '2015-01-01'::date))::DATE
      ,MAX(DATE_TRUNC('day', DATE '2024-04-20'::date))::DATE
      ,INTERVAL '1 day'
    ) AS DATE_LIST

),

CALENDAR_TBL AS (

SELECT

UNNEST(DATE_LIST)::DATE AS date

FROM DATE_SERIES
),
SUMMARY_TBL AS (

     SELECT

     DATE_TRUNC('day', order_date) AS date

    ,SUM(quantity) AS quantity

    ,customer_key
    ,product_key
    ,store_key

    FROM
 ( select * from sales )
GROUP BY ALL
),

UNIQUE_GROUPS AS (
    SELECT DISTINCT
SUMMARY_TBL.customer_key
,SUMMARY_TBL.product_key
,SUMMARY_TBL.store_key
    FROM SUMMARY_TBL
),

CROSS_JOINED AS (
    SELECT
        CALENDAR_TBL.date
      ,UNIQUE_GROUPS.customer_key
      ,UNIQUE_GROUPS.product_key
       ,UNIQUE_GROUPS.store_key
    FROM
        CALENDAR_TBL
    CROSS JOIN
        UNIQUE_GROUPS
)

SELECT
    CROSS_JOINED.date
    ,SUMMARY_TBL.customer_key
    ,SUMMARY_TBL.product_key
    ,SUMMARY_TBL.store_key
    ,COALESCE(SUMMARY_TBL.quantity, 0) AS quantity
FROM
    CROSS_JOINED
LEFT JOIN
SUMMARY_TBL
ON
CROSS_JOINED.date               = SUMMARY_TBL.date
AND CROSS_JOINED.store_key      = SUMMARY_TBL.store_key
AND CROSS_JOINED.product_key    = SUMMARY_TBL.product_key
AND CROSS_JOINED.customer_key   = SUMMARY_TBL.customer_key



    ")

dplyr::tbl(con,test_sql)

totalytd_sql <- function(.data, ..., date_var, value_var) {
  full_tbl <- make_aggregation_sql(.data, ..., date_var = {{ date_var }}, value_var = {{ value_var }}, time_unit = "day")
  # date_var  <- convert_input_to_string({{date_var}})
  # value_var <- convert_input_to_string({{value_var}})
  # print(date_var)
  # print(value_var)

  out <- full_tbl |>
    mutate(
      year = year(date)
    ) |>
    dbplyr::window_order(..., date) |>
    group_by(year, ...) |>
    mutate(
      ytd = cumsum({{ value_var }})
    )
  return(out)
}

totalytd_sql(.data=sales_db,store_key,date_var=order_date,value_var=quantity)




dplyr::tbl(con,test_sql)

con <- dbplyr::remote_con(sales_db)

capture_original_query(sales_db)



    # Create two SQL objects
    sql1 <- sql("with t as (select * from")
    sql2 <- sql(paste("(",sql(remote_query(sales_db)),")"))
    sql3 <- sql(")")
    sql4 <- sql("select * from t limit 5")

    # Combine the SQL objects using paste0 or paste
    combined_sql <- paste(sql1, sql2,sql3,sql4)

    dplyr::tbl(con,dplyr::sql(combined_sql))

    DBI::dbGetQuery(con,dplyr::sql(combined_sql))

    tst_sql <- glue_sql("

  select *

  from
  {sql2}

  where
  true


    ",.con=con)


tst_sql <- sales_db %>%
  glue::glue_data_sql("
  select
  *

  from
  ({`dbplyr::ident_q((remote_query(.))`})

  where
  true


    ",.con=con
  )


out <- dbplyr::remote_query(sales_db)



tbl <- dbplyr::remote_query(sales_db)



tbl <- "sales"

dbplyr::ident(
  glue_sql("
  {sql({`tbl`})}
  ", .con = con
  ,tbl=tbl
  ,.literal = FALSE
  )
)


tbl <- "sales"

sub_query <- glue_sql("
  SELECT *
  FROM {`tbl`}
  ", .con = con)

glue_sql("
  SELECT s.{`value_var`}
  FROM ({sub_query}) AS s
  ", .con = con)




tbl(con,tst_sql)


# returns results

DBI::dbGetQuery(conn = con,tbl_sql)

test <- dplyr::tbl(con,sql(tbl_sql))

dplyr::tbl(con,sql(no_group_sql))

##
dbplyr::remote_query(sales_db)

dbplyr::sql_render(query = dbplyr::remote_query(sales_db),con=con)


test <- select_query(from=dbplyr::remote_query(sales_db),select=sql(tbl_sql))

# send statements

DBI::dbSendQuery(conn = con,group_sql)

# returns zero
DBI::dbExecute(conn = con,group_sql)


# similiar to dbsend query
DBI::dbSendStatement(conn = con,group_sql)

DBI::dbGetQuery(conn = con,group_sql)

DBI::Id()

dbplyr::build_sql(con=con,group_sql)

dbplyr::db_connection_describe(con)


## test tbl with cte

tbl(con,no_group_sql)

## maybe a way to pass a table path

dbplyr::as_table_path(group_sql,con)

dbplyr::sql_render(
  query = dbplyr::build_sql("select * from sales",con = con)
  ,con = con,sql_options =sql_options(cte = TRUE)
)




convert_dots_to_string(hello,how,are,you)


paste0("hello.",test_fn(hello,how,are,you))


library(rlang)

# Define some variables
x <- 10
y <- 20

# Capture an expression using expr()
my_expr <- expr(10+10+x+as.numeric("1"))

# Evaluate the expression using eval_tidy() in the current environment
eval(my_expr)

# Output the result
print(result)



my_call <- expr(mean(x = z, na.rm = FALSE))

# Modify the function call: change na.rm to TRUE
modified_call <- call_modify(my_call,x=x)

# Print the modified call
print(modified_call)

# You can then evaluate the modified call
z <- c(1, 2, 3, NA)
result <- eval(modified_call)
print(result)


value <- 10
# Capture an expression with a placeholder
my_expr <- expr(value + 5)

# Interpolate the value into the expression
interpolated_expr <- expr_interp(my_expr)

# Print the interpolated expression
print(interpolated_expr)

# Evaluate the interpolated expression
result <- eval(interpolated_expr)


library(rlang)
my_quo <- quo(x + y)
env <- new_environment(list(x = 3, y = 4))
result <- eval_tidy(my_quo, env)
print(result)


nested_quo <- quo(quo(x + y))
flat_quo <- quo_squash(nested_quo)
print(flat_quo)

text |> str()
my_quo <- quo(mean(x))
name <- quo_name(my_quo)
text <- quo_text(my_quo)
print(name)
print(text)


rlang::parse_quo("my_quo",env)
my_quo <- quo(mean(x, na.rm = TRUE))
expr <- quo_get_expr(my_quo)
env <- quo_get_env(my_quo)
print(expr)
print(env)


