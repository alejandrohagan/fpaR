library(tidyverse)
devtools::document()
devtools::load_all()

library(glue)

db <- fpaR::create_contonso_duckdb()

sales_db <- db$sales

con <- dbplyr::remote_con(sales_db)


## rewrite make_aggregation_tbl to the below functions


calendar_sql <- seq_date_sql(start_date="2024-01-01",end_date = "2024-12-31",time_unit="month",con=con,cte=TRUE )


make_aggregation_sql <- function(.data, ..., date_var, value_var, time_unit) {

  ## create inputs----------

  original_query <- fpaR::capture_original_query(.data)

  min_date <- .data |>
    dplyr::summarize(
      min = base::min({{ date_var }}, na.rm = TRUE)
    ) |>
    dplyr::pull(min)


  max_date <- .data |>
    dplyr::summarize(
      max = base::max({{ date_var }}, na.rm = TRUE)
    ) |>
    dplyr::pull(max)


  date_var <- fpaR::convert_input_to_string({{ date_var }})
  value_var <- fpaR::convert_input_to_string({{ value_var }})
  time_unit <- fpaR::convert_input_to_string({{ time_unit }})

  interval_key <- base::paste("1", time_unit)

  args <- substitute(list(...))[-1]

  group_var <- sapply(args, convert_input_to_string)


  con <- dbplyr::remote_con(sales_db)


  ## create calendar
  calendar_sql <- fpaR::seq_date_sql(start_date = {{min_date}},end_date = {{max_date}},time_unit = {{time_unit}},con = con,cte=TRUE)$sql



  no_group_summary_sql <-
    fpaR::with(
    query=fpaR::sql_query_select(
      select = glue::glue_sql("
                       DATE_TRUNC({time_unit}, {`date_var`})::DATE AS date
                       ,SUM({`value_var`}) AS {`value_var`}

                       ",.con=con)
      ,group_by=dplyr::sql("ALL")
      ,previous_query = TRUE
      ,.data = sales_db
    )$sql
    ,query_name=summary_tbl
    ,order="last"

  )





  no_group_collect_sql <-
    glue::glue_sql("
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
",.con = con)






## Group SQL

  group_summary_sql <- fpaR::with(
    query=fpaR::sql_query_select(
      select = glue::glue_sql("
                       DATE_TRUNC({time_unit}, {`date_var`})::DATE AS date
                       ,SUM({`value_var`}) AS {`value_var`}
                      ,{`group_var`*}

                       ",.con=con)
      ,group_by=dplyr::sql("ALL")
      ,previous_query = TRUE
      ,.data = sales_db
    )$sql
    ,query_name=summary_tbl
    ,order="last"

  )



  group_collect_sql <- glue::glue_sql("

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





  ## return logic---------------

  if (missing(...)) {

    no_group_cte <- fpaR::cte(
      con = con
      ,calendar_sql
      ,no_group_summary_sql
      ,no_group_collect_sql
    )


    return(no_group_cte$dbi)

  } else {



    group_cte <- fpaR::cte(
      con=con
      ,calendar_sql
      ,group_summary_sql
      ,group_collect_sql
    )

    return(group_cte$dbi)

  }
}


test_dbi <- make_aggregation_sql(sales_db,date_var=order_date,value_var=quantity,time_unit="month")


make_aggregation_sql2test <- function(.data, ..., date_var, value_var, time_unit){

  ## create inputs----------

  original_query <- capture_original_query(.data)

  min_date <- .data |>
    dplyr::summarize(
      min = min({{ date_var }}, na.rm = TRUE)
    ) |>
    pull(min)

  max_date <- .data |>
    dplyr::summarize(
      max = max({{ date_var }}, na.rm = TRUE)
    ) |>
    pull(max)

  date_var <- fpaR::convert_input_to_string({{ date_var }})
  value_var <- fpaR::convert_input_to_string({{ value_var }})
  time_unit <- fpaR::convert_input_to_string({{ time_unit }})
  interval_key <- paste("1", time_unit)
  con <- dbplyr::remote_con(sales_db)

  # Create conditional variables-----------------


   if(!missing(...)){



  # declare vars
  group_var              <- fpaR::convert_dots_to_string(...)
  summary_group_id      <- DBI::Id("SUMMARY_TBL",group_var)
  summary_group_var    <- paste0("SUMMARY_TBL.",group_var)
  unique_group_id       <- DBI::Id("UNIQUE_GROUPS",group_var)
  cross_joined_group_id <- DBI::Id("CROSS_JOINED",group_var)
  cross_joined_group_var <- paste0("CROSS_JOINED.",group_var)

  #create join sql

  initial_join_sql <- glue::glue_sql(
    "LEFT JOIN
    SUMMARY_TBL
    ON
    CROSS_JOINED.date = SUMMARY_TBL.date"
  ,.con=con)




  join_group_var <- dplyr::sql(paste0("AND ",cross_joined_group_var,"=",summary_group_var))

}


  ## create calendar---------------

  calendar_sql <- fpaR::seq_date_sql(start_date = {{min_date}},end_date = {{max_date}},time_unit = {{time_unit}},con = con,cte=TRUE)$sql

  ## no group summary table-------

  no_group_summary_sql <-
    fpaR::with(
      query=fpaR::sql_query_select(
        select = glue::glue_sql("
                       DATE_TRUNC({time_unit}, {`date_var`})::DATE AS date
                       ,SUM({`value_var`}) AS {`value_var`}

                       ",.con=con)
        ,group_by=dplyr::sql("ALL")
        ,previous_query = TRUE
        ,.data = .data
      )$sql
      ,query_name=summary_tbl
      ,order="last"

    )


  no_group_collect_sql <-
    glue::glue_sql("
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
",.con = con)


  ## group summary table--------

if(!missing(...)){

print("step 1")

group_summary_sql <- fpaR::with(

    query=
      fpaR::sql_query_select(
        select = glue::glue_sql("
                       DATE_TRUNC({time_unit}, {`date_var`})::DATE AS date
                       ,SUM({`value_var`}) AS {`value_var`}
                      ,{`group_var`*}",.con=con)

      ,group_by=dplyr::sql("ALL")
      ,previous_query = TRUE
      ,.data = sales_db
    )$sql
    ,query_name=summary_tbl
    ,order="middle"
  )


print("step 2")

unique_groups_sql <- fpaR::with(
    query=glue_sql("

        SELECT DISTINCT
        {`summary_group_id`*}
        FROM SUMMARY_TBL
"
,.con=con)
,query_name=UNIQUE_GROUPS
,order = "middle"
)


print("step 3")

cross_join_sql <- cross_join_sql <- fpaR::with(
  query=glue_sql("

    SELECT
        CALENDAR_TBL.date,
        {`unique_group_id`*}
    FROM
        CALENDAR_TBL
    CROSS JOIN
        UNIQUE_GROUPS"
,.con=con)
,query_name=CROSS_JOINED
,order = "last"
)

print("step 4")

group_collect_sql <- glue::glue_sql("

SELECT
    CROSS_JOINED.date,
    {`summary_group_id`*},
    COALESCE(SUMMARY_TBL.{`value_var`}, 0) AS {`value_var`}
FROM
    CROSS_JOINED
  ", .con = con)
}


print("step 5")

## return logic---------------

if (missing(...)) {

  no_group_cte <- fpaR::cte(
    con = con
    ,calendar_sql
    ,no_group_summary_sql
    ,no_group_collect_sql
  )

  return(no_group_cte)

} else {

print("step 6")

  group_cte <- cte(
    con=con
    ,calendar_sql
    ,group_summary_sql
    ,unique_groups_sql
    ,cross_join_sql
    ,group_collect_sql
    ,initial_join_sql
    ,join_group_var
  )

  print("step 7")

  out <- list(
    group_cte=group_cte
    ,group_collect_sql=group_collect_sql
    ,cross_join_sql=cross_join_sql
    ,unique_groups_sql=unique_groups_sql
    ,group_summary_sql=group_summary_sql
    ,join_group_vars=join_group_var
    ,initial_join_sql=initial_join_sql
    ,group_vars=group_var
    ,summary_group_vars=summary_group_var
    ,unique_group_vars=unique_group_var
    ,cross_joined_group_vars=cross_joined_group_var
  )


  return(out)
  }

}

make_aggregation_sql2safe <- purrr::safely(make_aggregation_sql2test,otherwise = NA)


(out <- make_aggregation_sql2safe(sales_db,customer_key,date_var=order_date,value_var=quantity,time_unit="month"))

out$result$group_cte$dbi |>
  filter(
    !is.na(customer_key)
  )

time_unit <- "month"
date_var  <- "order_date"
group_var <- "customer_key"
value_var <- "quantity"


summary_group_var <- DBI::Id("SUMMARY_TBL",group_var)
unique_group_id <- DBI::Id("UNIQUE_GROUPS",group_var)
cross_joined_group_var <- paste0("CROSS_JOINED.",group_var)
nicknames_species <- DBI::Id("test", "species")


#create join sql

initial_join_sql <- glue_sql(
  "LEFT JOIN
    SUMMARY_TBL
    ON
    CROSS_JOINED.date = SUMMARY_TBL.date"
  ,.con=con)


join_group_var <- dplyr::sql(paste0("AND ",cross_joined_group_var,"=",summary_group_var))

cross_join_sql <- cross_join_sql <- fpaR::with(
  query=glue_sql("

    SELECT
        CALENDAR_TBL.date,
        {`unique_group_id`}
    FROM
        CALENDAR_TBL
    CROSS JOIN
        UNIQUE_GROUPS"
    ,.con=con)
  ,query_name=CROSS_JOINED
  ,order = "last"
)

fpaR::with(

  query=
    fpaR::sql_query_select(
      select = glue::glue_sql("
                       DATE_TRUNC({time_unit}, {`date_var`})::DATE AS date
                       ,SUM({`value_var`}) AS {`value_var`}
                      ,{`group_var`*}",.con=con)

      ,group_by=dplyr::sql("ALL")
      ,previous_query = TRUE
      ,.data = sales_db
    )$sql
  ,query_name=summary_tbl
  ,order="middle"
)



fpaR::with(
  query=glue_sql("

        SELECT DISTINCT {`summary_group_var`}
        FROM SUMMARY_TBL
"
,.con=con)
,query_name=UNIQUE_GROUPS
,order = "middle"
)


fpaR::with(
  query=glue_sql("
        \n
        SELECT sum({`nicknames_species`})
        FROM SUMMARY_TBL
        \n
",.con=con
,summary_group_var=summary_group_var
)
,query_name=UNIQUE_GROUPS
,order = "middle"
)


