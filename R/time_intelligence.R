
#' Aggregate and expand date table
#'
#' @param .data tibble
#' @param ... dimensions to group by
#' @param date_var dimension with main date column
#' @param value_var dimension with values to aggregate by
#' @param time_unit which time unit to aggregate to
#'
#' @return tibble
#' @export
#'
#' @examples
#' make_aggregation_tbl(fpaR::sales,date=date,value=quantity,time_unit="day")
make_aggregation_tbl <- function(.data,date,value,time_unit) {


  assertthat::assert_that(base::is.data.frame(.data), msg = "Data must be a data frame.")
  assertthat::assert_that(base::is.character(time_unit), msg = "Time unit must be a character string.")
  assertthat::assert_that(time_unit %in% base::c("day", "week","quarter","semester","month", "year"), msg = "Time frame must be one of 'day', 'week','semester', 'month', or 'year'.")
  assertthat::assert_that(lubridate::is.Date(.data |> pull({{date}})), msg = "The date column is not in Date format.")
  #
  # # # Check if the column follows the yyyy-mm-dd format
  # formatted_dates <- format(date, "%Y-%m-%d")
  # assertthat::assert_that(base::all(date == base::as.Date(formatted_dates)), msg = "The date column does not follow the yyyy-mm-dd format.")
  #

  existing_groups <- dplyr::groups(.data) |>
    map(\(x) rlang::as_label(x)) |> purrr::simplify()

  # Floor the date to the specified time frame
  summary_tbl <- .data |>
    dplyr::mutate(
      date = lubridate::floor_date({{date}},time_unit)
      ,time_unit=time_unit
    ) |>
    dplyr::group_by(date,!!!existing_groups) |>
    dplyr::summarise(
      "{{value}}":= sum({{value}},na.rm=TRUE)
      ,.groups = "drop"
    )


  # Create a calendar table with all the dates in the specified time frame
  calendar_tbl <- tibble::tibble(
    date = base::seq.Date(from = base::min(summary_tbl$date,na.rm=TRUE), to = base::max(summary_tbl$date,na.rm = TRUE), by = time_unit)
  )

  # create crossing table of groups

  if(!existing_groups|> is_empty()){

    calendar_tbl <- dplyr::left_join(
      summary_tbl |> dplyr::distinct(pick(existing_groups)) |> dplyr::mutate(id="id")
      ,calendar_tbl |> dplyr::mutate(id="id")
      ,by=dplyr::join_by(id)
      ,relationship = "many-to-many"
    ) |>
      dplyr::select(-id)

  }

  # Perform a full join to ensure all time frames are represented
  full_tbl <- dplyr::full_join(
    calendar_tbl
    ,summary_tbl
    ,by = dplyr::join_by(date,!!!existing_groups)
  ) |>
    dplyr::mutate(
      # dplyr::across(dplyr::where(\(x) base::is.numeric(x)),\(x) tidyr::replace_na(x,0))
      "{{value}}":= dplyr::coalesce({{value}}, 0)
    )



  return(full_tbl)

}

#' Aggregate and expand date table
#'
#' @param .data tibble
#' @param ... dimensions to group by
#' @param date_var dimension with main date column
#' @param value_var dimension with values to aggregate by
#' @param time_unit which time unit to aggregate to


#' Aggregate and expand date table
#'
#' @param .data dbi object
#' @param ... dimensions to group by
#' @param date_var dimension with main date column
#' @param value_var dimension with values to aggregate by
#' @param time_unit which time unit to aggregate to
#'
#' @return list of dbi and sql object
#' @export
#'
#' @examples
make_aggregation_dbi <- function(.data, ..., date_var, value_var, time_unit){

  ## create inputs----------
  original_query <- fpaR::capture_original_query(.data)

  min_date <- .data |>
    dplyr::summarize(
      min = min({{ date_var }}, na.rm = TRUE)
    ) |>
    dplyr::pull(min)

  max_date <- .data |>
    dplyr::summarize(
      max = max({{ date_var }}, na.rm = TRUE)
    ) |>
    dplyr::pull(max)

  date_var     <- fpaR::convert_input_to_string({{ date_var }})
  value_var    <- fpaR::convert_input_to_string({{ value_var }})
  time_unit    <- fpaR::convert_input_to_string({{ time_unit }})
  interval_key <- base::paste("1", time_unit)
  con          <- dbplyr::remote_con(.data)

  # Create conditional variables-----------------

  if(!missing(...)){



    # declare vars
    group_var              <- fpaR::convert_dots_to_string(...)


    summary_group_var      <- paste0("SUMMARY_TBL.",group_var)
    unique_group_var       <- paste0("UNIQUE_GROUPS.",group_var)
    cross_joined_group_var <- paste0("CROSS_JOINED.",group_var)

    if(length(group_var)>1){

    summary_group_id       <- purrr::map(group_var,\(x) DBI::Id("SUMMARY_TBL"  ,x))
    unique_group_id        <- purrr::map(group_var,\(x) DBI::Id("UNIQUE_GROUPS",x))
    cross_joined_group_id  <- purrr::map(group_var,\(x) DBI::Id("CROSS_JOINED" ,x))

    }else{

      summary_group_id       <- DBI::Id("SUMMARY_TBL"  ,group_var)
      unique_group_id        <- DBI::Id("UNIQUE_GROUPS",group_var)
      cross_joined_group_id  <- DBI::Id("CROSS_JOINED" ,group_var)


    }
    #create join sql queries


    initial_join_sql <- glue::glue_sql(
      "LEFT JOIN
    SUMMARY_TBL
    ON
    CROSS_JOINED.date = SUMMARY_TBL.date"
    ,.con=con)


    if(length(group_var)>1){


    cross_joined_group_id <- purrr::map(group_var,\(x) DBI::Id("CROSS_JOINED" ,x))
    summary_group_id      <- purrr::map(group_var,\(x) DBI::Id("SUMMARY_TBL"  ,x))
    join_group_var        <- glue::glue_sql("AND {`cross_joined_group_id`}={`summary_group_id`}",.con=con)
    join_group_sql        <- dplyr::sql(base::paste0(join_group_var) |> stringr::str_flatten(collapse = " "))

    }else{

    cross_joined_group_id <- DBI::Id("CROSS_JOINED" ,group_var)
    summary_group_id      <- DBI::Id("SUMMARY_TBL"  ,group_var)
    join_group_sql        <- glue::glue_sql("AND {`cross_joined_group_id`}={`summary_group_id`}",.con=con)
    }
  }

  ## create calendar---------------

  calendar_sql <- fpaR::seq_date_sql(start_date = {{min_date}},end_date = {{max_date}},time_unit = {{time_unit}},con = con,cte=TRUE)$sql

  ## no group summary table-------

  no_group_summary_sql <-
    fpaR::with(
      query=fpaR::sql_query_select(
        select = glue::glue_sql("
                       DATE_TRUNC({time_unit}, {`date_var`})::DATE AS date
                       ,SUM({`value_var`}) AS {`value_var`}",.con=con)
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
  SUMMARY_TBL.date",.con = con)


  ## group summary table--------

  if(!missing(...)){


    group_summary_sql <- fpaR::with(

      query=
        fpaR::sql_query_select(

          select = glue::glue_sql("
                       DATE_TRUNC({time_unit}, {`date_var`})::DATE AS date
                       ,SUM({`value_var`}) AS {`value_var`}
                      ,{`group_var`*}",.con=con)

          ,group_by=dplyr::sql("ALL")
          ,previous_query = TRUE
          ,.data = .data
        )$sql
      ,query_name=SUMMARY_TBL
      ,order="middle"
    )



    unique_groups_sql <- fpaR::with(
      query=glue_sql("
        SELECT DISTINCT {`summary_group_id`*}
        FROM SUMMARY_TBL",.con=con)
      ,query_name=UNIQUE_GROUPS
      ,order = "middle"
    )


    cross_join_sql <- fpaR::with(
      query=
        glue_sql("
    SELECT
    CALENDAR_TBL.date,
    {`unique_group_id`*}

    FROM
    CALENDAR_TBL
    CROSS JOIN
    UNIQUE_GROUPS",.con=con)
    ,query_name=CROSS_JOINED
    ,order = "last"
    )

    group_collect_sql <-
      glue::glue_sql("
    SELECT
    CROSS_JOINED.date,
    {`cross_joined_group_id`*},
    COALESCE(SUMMARY_TBL.{`value_var`}, 0) AS {`value_var`}

    FROM
    CROSS_JOINED",.con = con)
  }

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


    group_cte <- cte(
      con=con
      ,calendar_sql
      ,group_summary_sql
      ,unique_groups_sql
      ,cross_join_sql
      ,group_collect_sql
      ,initial_join_sql
      ,join_group_sql
    )
    return(group_cte)
  }
}






# calculate <- function(.data,...,date_var,value_var,time_unit){
#
#   # Validate inputs
#   assertthat::assert_that(base::is.data.frame(.data), msg = "data must be a data frame")
#   assertthat::assert_that(base::is.character(time_unit), msg = "time_unit must be a character")
#
#   # Aggregate data based on provided time unit
#
#   full_tbl <-  .data |>
#     make_aggregation_tbl(...,date_var={{date_var}},value_var={{value_var}},time_unit=time_unit)
#
#   # Determine label for the time unit
#
#   lbl <-   base::names(
#     base::match.arg(
#       "week"
#       ,choices = c("mom"="month","yoy"="year","wow"="week","dod"="day")
#       ,several.ok = FALSE
#     )
#   )
#
#
#   # Calculate difference and proportional change
#
#   out_tbl <- full_tbl |>
#     dplyr::mutate(
#       !!lbl:= {{value_var}} - lag({{value_var}}, 1)
#       ,prop_delta=  .data[[!!lbl]]/{{value_var}}
#     )
#
#   return(out_tbl)
#
# }

#' Year-to-date
#ea
#' @param .data either a dataframe or lazy DBI object
#' @param date the date column to aggregate
#' @param value the value column to summarize
#' @param calendar_type either 'standard' or '5-5-4' calendar
#'
#' @returns
#' @export
#'
#' @examples
ytd <- function(.data,date,value,calendar_type){

  # Validate inputs
  assertthat::assert_that(base::is.data.frame(.data), msg = "data must be a data frame")



  out <- ytd_tbl(
    calendar_tbl(
      data=.data
      ,calendar_type=calendar_type
      ,date_vec = rlang::as_label(rlang::enquo(date))
    )
    ,time_unit = time_unit("day")
    ,action=action("aggregate")
    ,value_vec = rlang::as_label(rlang::enquo(value))
    ,new_column_name_prefix = "ytd"
    ,sort_logic = TRUE
    ,fn="ytd"
    ,new_date_column_name = "Year"
  )

}


#' Title
#'
#' @param .data
#' @param date
#' @param value
#' @param type
#'
#' @returns
#' @export
#'
#' @examples
qtd <- function(.data,date,value,calendar_type){

  # Validate inputs
  assertthat::assert_that(base::is.data.frame(.data), msg = "data must be a data frame")

  # Aggregate data based on provided time unit


  out <- qtd_tbl(
    calendar_tbl(
      data      = .data
      ,calendar_type=calendar_type
      ,date_vec = rlang::as_label(rlang::enquo(date))
    )
    ,time_unit  = time_unit("day")
    ,action     = action("aggregate")
    ,value_vec  = rlang::as_label(rlang::enquo(value))
    ,new_column_name_prefix = "qtd"
    ,sort_logic = TRUE
    ,fn         = "totalqtd"
    ,new_date_column_name = c("year","quarter")
  )

  return(out)

}


#' Title
#'
#' @param .data
#' @param date
#' @param value
#' @param type
#'
#' @returns
#' @export
#'
#' @examples
mtd <- function(.data,date,value,calendar_type){

    # Validate inputs
    assertthat::assert_that(base::is.data.frame(.data), msg = "data must be a data frame")



    out <- mtd_tbl(
      calendar_tbl(
        data=.data
        ,calendar_type=calendar_type
        ,date_vec = rlang::as_label(rlang::enquo(date))
      )
      ,time_unit = time_unit("day")
      ,action=action("aggregate")
      ,value_vec = rlang::as_label(rlang::enquo(value))
      ,new_column_name_prefix = "mtd"
      ,sort_logic = TRUE
      ,fn="totalmtd"
      ,new_date_column_name = c("year","month")
    )


  return(out)
}

#' Title
#'
#' @param .data
#' @param date
#' @param value
#' @param type
#'
#' @returns
#' @export
#'
#' @examples
wtd <- function(.data,date,value,calendar_type){

  # Validate inputs
  assertthat::assert_that(base::is.data.frame(.data), msg = "data must be a data frame")



  out <- wtd_tbl(
    calendar_tbl(
      data=.data
      ,calendar_type=calendar_type
      ,date_vec = rlang::as_label(rlang::enquo(date))
    )
    ,time_unit = time_unit("day")
    ,action=action("aggregate")
    ,value_vec = rlang::as_label(rlang::enquo(value))
    ,new_column_name_prefix = "wtd"
    ,sort_logic = TRUE
    ,fn="totalwtd"
    ,new_date_column_name = c("year","month","week")
  )


  return(out)
}


#' Title
#'
#' @param .data
#' @param date
#' @param value
#' @param type
#'
#' @returns
#' @export
#'
#' @examples
atd <- function(.data,date,value,calendar_type){

  # Validate inputs
  assertthat::assert_that(base::is.data.frame(.data), msg = "data must be a data frame")



  out <- atd_tbl(
    calendar_tbl(
      data=.data
      ,calendar_type=calendar_type
      ,date_vec = rlang::as_label(rlang::enquo(date))
    )
    ,time_unit = time_unit("day")
    ,action=action("aggregate")
    ,value_vec = rlang::as_label(rlang::enquo(value))
    ,new_column_name_prefix = "atd"
    ,sort_logic = TRUE
    ,fn="totalatd"
    ,new_date_column_name = NA_character_
  )


  return(out)

}


#' Title
#'
#' @param .data
#' @param date
#' @param value
#' @param type
#'
#' @returns
#' @export
#'
#' @examples
dod <- function(.data,date,value,calendar_type,lag_n){


  out <- dod_tbl(
    calendar_tbl(
      data=.data
      ,calendar_type=calendar_type
      ,date_vec = rlang::as_label(rlang::enquo(date))
    )
    ,time_unit = time_unit("day")
    ,action=action(c("aggregate","shift","compare"))
    ,value_vec = rlang::as_label(rlang::enquo(value))
    ,new_column_name_prefix = "dod"
    ,sort_logic = TRUE
    ,fn="dod"
    ,new_date_column_name = NA_character_
    ,lag_n=lag_n
  )
  return(out)
}

#' Title
#'
#' @param .data
#' @param date
#' @param value
#' @param type
#'
#' @returns
#' @export
#'
#' @examples
wow <- function(.data,date,value,calendar_type,lag_n){


  out <- wow_tbl(
    calendar_tbl(
      data=.data
      ,calendar_type=calendar_type
      ,date_vec = rlang::as_label(rlang::enquo(date))
    )
    ,time_unit = time_unit("week")
    ,action=action(c("aggregate","shift","compare"))
    ,value_vec = rlang::as_label(rlang::enquo(value))
    ,new_column_name_prefix = "wow"
    ,sort_logic = TRUE
    ,fn="wow"
    ,new_date_column_name = NA_character_
    ,lag_n=lag_n
  )
  return(out)
}




#' Title
#'
#' @param .data
#' @param date
#' @param value
#' @param type
#'
#' @returns
#' @export
#'
#' @examples
mom <- function(.data,date,value,calendar_type,lag_n){


  out <- mom_tbl(
    calendar_tbl(
      data=.data
      ,calendar_type=calendar_type
      ,date_vec = rlang::as_label(rlang::enquo(date))
    )
    ,time_unit = time_unit("month")
    ,action=action(c("aggregate","shift","compare"))
    ,value_vec = rlang::as_label(rlang::enquo(value))
    ,new_column_name_prefix = "mom"
    ,sort_logic = TRUE
    ,fn="mom"
    ,new_date_column_name = NA_character_
    ,lag_n=lag_n
  )
  return(out)
}


#' Title
#'
#' @param .data
#' @param date
#' @param value
#' @param type
#'
#' @returns
#' @export
#'
#' @examples
yoy <- function(.data,date,value,calendar_type,lag_n){

  out <- yoy_tbl(
    calendar_tbl(
      data=.data
      ,calendar_type=calendar_type
      ,date_vec = rlang::as_label(rlang::enquo(date))
    )
    ,time_unit = time_unit("year")
    ,action=action(c("aggregate","shift","compare"))
    ,value_vec = rlang::as_label(rlang::enquo(value))
    ,new_column_name_prefix = "yoy"
    ,sort_logic = TRUE
    ,fn="yoy"
    ,new_date_column_name = NA_character_
    ,lag_n=lag_n
  )

  return(out)

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
#' totalytd(fpaR:sales,date_var = order_date,value_var = quantity)
# totalytd <- function(.data,...,date_var,value_var){
#
#   # Validate inputs
#   assertthat::assert_that(base::is.data.frame(.data), msg = "data must be a data frame")
#
#   # Aggregate data based on provided time unit
#
#   full_tbl <-  .data |>
#     make_aggregation_tbl(...,date_var={{date_var}},value_var={{value_var}},time_unit="day") |>
#     dplyr::mutate(
#       year=lubridate::year(date)
#       ,.before = 1
#     )
#
#
#
#   out_tbl <- full_tbl |>
#     dplyr::group_by(year,...) |>
#     dplyr::arrange(date,.by_group = TRUE) |>
#     dplyr::mutate(
#     ytd=base::cumsum({{value_var}})
#   ) |>
#     dplyr::ungroup()
#
#   return(out_tbl)
#
# }

#' Total year to date values
#'
#' @param .data tibble of values
#' @param ... optional columns to group by
#' @param date_var column with date var to aggregate by
#' @param value_var column with value to aggregate
#'
#' @return dbi object
#' @export
#'
#' @examples
totalytd_dbi <- function(.data,...,date_var,value_var){

  # Validate inputs
  assertthat::assert_that(
    any(class(.data) %in% c("tbl_dbi"))
    , msg = "data must be a DBI"
  )

  # Aggregate data based on provided time unit
  full_dbi <- .data |>
    make_aggregation_dbi(...,date_var={{date_var}},value_var={{value_var}},time_unit="day") |>
    pluck("dbi") |>
    dplyr::mutate(
      year=lubridate::year(date)
    )


  out_dbi <- full_dbi |>
    dplyr::group_by(...,year) |>
    dbplyr::window_order(date) |>
    dplyr::mutate(
      ytd=base::cumsum({{value_var}})
    ) |>
    dplyr::ungroup()

  return(out_dbi)

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
#' totalqtd(fpaR:sales,date_var = order_date,value_var = quantity)
# totalqtd <- function(.data,...,date_var,value_var){
#
#   # Validate inputs
#   assertthat::assert_that(base::is.data.frame(.data), msg = "data must be a data frame")
#
#   # Aggregate data based on provided time unit
#
#   full_tbl <-  .data |>
#     make_aggregation_tbl(...,date_var={{date_var}},value_var={{value_var}},time_unit="day") |>
#     dplyr::mutate(
#       year=lubridate::year(date)
#       ,quarter=lubridate::quarter(date)
#       ,.before = 1
#     )
#
#
#
#   out_tbl <- full_tbl |>
#     dplyr::group_by(year,quarter,...) |>
#     dplyr::mutate(
#       qtd=base::cumsum({{value_var}})
#     ) |>
#     dplyr::ungroup()
#
#   return(out_tbl)
#
# }

#' Total quarter to date values
#'
#' @param .data tibble of values
#' @param ... optional columns to group by
#' @param date_var column with date var to aggregate by
#' @param value_var column with value to aggregate
#'
#' @return
#' @export
#'
#' @examples
totalqtd_dbi <- function(.data,...,date_var,value_var){

  # Validate inputs
  assertthat::assert_that(
    any(class(.data) %in% c("tbl_dbi"))
    , msg = "data must be a DBI"
  )

  # Aggregate data based on provided time unit
  full_dbi <- .data |>
    make_aggregation_dbi(...,date_var={{date_var}},value_var={{value_var}},time_unit="day") |>
    pluck("dbi") |>
    dplyr::mutate(
      year=lubridate::year(date)
      ,quarter=lubridate::quarter(date)
    )


  out_dbi <- full_dbi |>
    dplyr::group_by(year,quarter,...) |>
    dbplyr::window_order(date) |>
    dplyr::mutate(
      qtd=base::cumsum({{value_var}})
    ) |>
    dplyr::ungroup()

  return(out_dbi)

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
#' totalmtd(fpaR:sales,date_var = order_date,value_var = quantity)
# totalmtd <- function(.data,...,date_var,value_var){
#
#   # Validate inputs
#   assertthat::assert_that(base::is.data.frame(.data), msg = "data must be a data frame")
#
#   # Aggregate data based on provided time unit
#
#   full_tbl <-  .data |>
#     make_aggregation_tbl(...,date_var={{date_var}},value_var={{value_var}},time_unit="day") |>
#     dplyr::mutate(
#       month=lubridate::month(date)
#       ,year=lubridate::year(date)
#       ,.before=1
#     ) |>
#     arrange(date)
#
#   # Determine label for the time unit
#
#
#   # Calculate difference and proportional change
#
#   out_tbl <- full_tbl |>
#     dplyr::group_by(year,month,...) |>
#     dplyr::mutate(
#       mtd=base::cumsum({{value_var}})
#     ) |>
#     dplyr::ungroup()
#
#   return(out_tbl)
#
# }

#' Total month to date values
#'
#' @param .data tibble of values
#' @param ... optional columns to group by
#' @param date_var column with date var to aggregate by
#' @param value_var column with value to aggregate
#'
#' @return
#' @export
#'
#' @examples
totalmtd_dbi <- function(.data,...,date_var,value_var){

  # Validate inputs
  assertthat::assert_that(
    any(class(.data) %in% c("tbl_dbi"))
    , msg = "data must be a DBI"
  )

  # Aggregate data based on provided time unit
  full_dbi <- .data |>
    make_aggregation_dbi(...,date_var={{date_var}},value_var={{value_var}},time_unit="day") |>
    pluck("dbi") |>
    dplyr::mutate(
      year=lubridate::year(date)
      ,month=lubridate::month(date)
    )



  out_dbi <- full_dbi |>
    dplyr::group_by(year,month,...) |>
    dbplyr::window_order(date) |>
    dplyr::mutate(
      mtd=base::cumsum({{value_var}})
    ) |>
    dplyr::ungroup()

  return(out_dbi)

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
#' totalwtd(fpaR:sales,date_var = order_date,value_var = quantity)
# totalwtd <- function(.data,...,date_var,value_var){
#
#   # Validate inputs
#   assertthat::assert_that(base::is.data.frame(.data), msg = "data must be a data frame")
#
#   # Aggregate data based on provided time unit
#
#   full_tbl <-  .data |>
#     make_aggregation_tbl(...,date_var={{date_var}},value_var={{value_var}},time_unit="day") |>
#     mutate(
#       week=lubridate::week(date)
#       ,month=lubridate::month(date)
#       ,year=lubridate::year(date)
#       ,.before = 1
#     )
#
#   # Determine label for the time unit
#
#
#   # Calculate difference and proportional change
#
#   out_tbl <- full_tbl |>
#     dplyr::group_by(week,month,year,... ) |>
#     dplyr::mutate(
#       wtd=base::cumsum({{value_var}})
#     ) |>
#     dplyr::ungroup()
#
#   return(out_tbl)
#
# }

#' Total week to date values
#'
#' @param .data tibble of values
#' @param ... optional columns to group by
#' @param date_var column with date var to aggregate by
#' @param value_var column with value to aggregate
#'
#' @return
#' @export
#'
#' @examples
totalwtd_dbi <- function(.data,...,date_var,value_var){

  # Validate inputs
  assertthat::assert_that(
    any(class(.data) %in% c("tbl_dbi"))
    , msg = "data must be a DBI"
  )

  # Aggregate data based on provided time unit
  full_dbi <- .data |>
    make_aggregation_dbi(...,date_var={{date_var}},value_var={{value_var}},time_unit="day") |>
    pluck("dbi") |>
    dplyr::mutate(
      year=lubridate::year(date)
      ,month=lubridate::month(date)
      ,week=sql("EXTRACT(WEEK FROM date)")
    )


  out_dbi <- full_dbi |>
    dplyr::group_by(year,month,week,...) |>
    dbplyr::window_order(date) |>
    dplyr::mutate(
      wtd=base::cumsum({{value_var}})
    ) |>
    dplyr::ungroup()

  return(out_dbi)

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
#' totalatd(fpaR:sales,date_var = order_date,value_var = quantity)
# totalatd <- function(.data,...,date_var,value_var){
#
#   # Validate inputs
#   assertthat::assert_that(base::is.data.frame(.data), msg = "data must be a data frame")
#
#   # Aggregate data based on provided time unit
#
#   full_tbl <-  .data |>
#     make_aggregation_tbl(...,date_var={{date_var}},value_var={{value_var}},time_unit="day") |>
#     dplyr::arrange(date)
#
#   # Determine label for the time unit
#
#
#   # Calculate difference and proportional change
#
#   out_tbl <- full_tbl |>
#     group_by(...) |>
#     dplyr::mutate(
#       atd=base::cumsum({{value_var}})
#     )
#
#   return(out_tbl)
#
# }

#' Total since inception
#'
#' @param .data tibble of values
#' @param ... optional columns to group by
#' @param date_var column with date var to aggregate by
#' @param value_var column with value to aggregate
#'
#' @return
#' @export
#'
#' @examples
totalatd_dbi <- function(.data,...,date_var,value_var){

  # Validate inputs
  assertthat::assert_that(
    any(class(.data) %in% c("tbl_dbi"))
    , msg = "data must be a DBI"
  )

  # Aggregate data based on provided time unit
  full_dbi <- .data |>
    make_aggregation_dbi(...,date_var={{date_var}},value_var={{value_var}},time_unit="day") |>
    pluck("dbi") |>
    dplyr::arrange(date)


  out_dbi <- full_dbi |>
    dplyr::group_by(...) |>
    dbplyr::window_order(date) |>
    dplyr::mutate(
      atd=base::cumsum({{value_var}})
    ) |>
    dplyr::ungroup()

  return(out_dbi)

}


#' Day over day valus
#'
#' @param .data
#' @param ...
#' @param date_var
#' @param value_var
#' @param lag_n
#' @param time_unit
#'
#' @return
#' @export
#'
#' @examples
# dod <- function(.data,...,date_var,value_var,lag_n=1,time_unit="day"){
#
#   # Validate inputs
#   assertthat::assert_that(base::is.data.frame(.data), msg = "data must be a data frame")
#   assertthat::assert_that(time_unit %in% base::c("day","quarter","month", "year"), msg = "Time frame must be one of 'day', 'month',;quarter' or 'year'.")
#   # Aggregate data based on provided time unit
#
#   full_tbl <-  .data |>
#     make_aggregation_tbl(...,date_var={{date_var}},value_var={{value_var}},time_unit=time_unit) |>
#     arrange(
#       date
#     )
#
#
#   ## multiplication factor
#
#   multiply_options <- c("day"=1,"month"=12,"quarter"=4,"year"=1)
#
#
#
#   multiply_vec <- multiply_options[time_unit] |> base::unname()
#
#
#
#   if(time_unit %in% c("day")){
#
#     # Calculate difference and proportional change
#
#
#     lag_tbl <- full_tbl |>
#       dplyr::group_by(...) |>
#       dplyr::mutate(
#         date_lag=date %m+% lubridate::years(lag_n)
#         ,"{{value_var}}_dod":={{value_var}}
#       ) |>
#       dplyr::select(-c(date,{{value_var}})) |>
#       dplyr::ungroup()
#
#     out_tbl <-  dplyr::left_join(
#       full_tbl
#       ,lag_tbl
#       ,by=dplyr::join_by(date==date_lag,...)
#     ) |>
#       mutate(
#         "{{value_var}}_dod" := dplyr::coalesce(.data[[rlang::englue("{{value_var}}_dod")]],0)
#       )
#     return(out_tbl)
#
#   } else {
#
#     out_tbl <-  full_tbl |>
#       group_by(...) |>
#       dplyr::mutate(
#         "{{value_var}}_dod":=dplyr::lag({{value_var}},n=(lag_n*multiply_vec))
#       ) |>
#       dplyr::ungroup()
#
#     return(out_tbl)
#
#   }
#
#
# }

#' calculate day over day values on a complete calendar table
#'
#' @param .data
#' @param ...
#' @param date_var
#' @param value_var
#' @param lag_n
#'
#' @return
#' @export
#'
#' @examples
dod_dbi <- function(.data,...,date_var,value_var,lag_n=1){

  ## create variables

  value_var_old <- deparse(substitute(value_var))
  value_var_interim<- paste0(value_var_old,"_","dod")
  value_var_final <- paste0(value_var_interim,"_",lag_n)


  full_dbi <- .data |>
    make_aggregation_dbi(...,date_var={{date_var}},value_var={{value_var}},time_unit="day") |>
    purrr::pluck("dbi")

  if(!missing(...)){
    # create lags
    lag_table <-  full_dbi |>
      dplyr::group_by(...) |>
      dbplyr::window_order(date) |>
      dplyr::mutate(
        !!value_var_interim:={{value_var}}
        ,date_lag=dplyr::sql(glue::glue("DATE + INTERVAL {lag_n} DAY"))
      ) |>
      dplyr::select(-c(date,all_of(value_var_old))) |>
      dplyr::ungroup() |>
      mutate(
        date_lag=as.Date(date_lag)
      )
  }else{
    lag_table <-  full_dbi |>
      dbplyr::window_order(date) |>
      dplyr::mutate(
        !!value_var_interim:={{value_var}}
        ,date_lag=dplyr::sql(glue::glue("DATE + INTERVAL {lag_n} DAY"))
      ) |>
      dplyr::select(-c(date,all_of(value_var_old))) |>
      dplyr::ungroup() |>
      mutate(
        date_lag=as.Date(date_lag)
      )
  }

  # join tables
  out_tbl <-  dplyr::left_join(
    full_dbi
    ,lag_table
    ,by=dplyr::join_by(date==date_lag,...)
  ) |>
    mutate(
      !!value_var_final:=dplyr::sql(glue::glue("COALESCE({value_var_interim},0)"))
    ) |>
    select(-c(all_of(value_var_interim))) |>
    arrange(date)

  return(out_tbl)

}

#'
#'
#' #' Week over week values
#' #' @description
#' #' For datasets with daily granularity, this will calculate year over year values with some simple descriptive functions
#' #'
#' #' @param .data tibble of values
#' #' @param ... optional columns to group by
#' #' @param date_var column with date var to aggregate by
#' #' @param value_var column with value to aggregate
#' #' @param lag_n the number of weeks to lag
#' #'
#' #' @return tibble
#' #' @export
#' #'
#' #' @examples
#' #' wow(fpaR:sales,date_var = order_date,value_var = quantity)
#' wow <- function(.data,...,date_var,value_var,lag_n=1){
#'
#'   # Validate inputs
#'   assertthat::assert_that(base::is.data.frame(.data), msg = "data must be a data frame")
#'
#'   # Aggregate data based on provided time unit
#'
#'   full_tbl <-  .data |>
#'     make_aggregation_tbl(...,date_var={{date_var}},value_var={{value_var}},time_unit="day")
#'
#'   # Determine label for the time unit
#'
#'
#'   # Calculate difference and proportional change
#'
#'   lag_table <- full_tbl |>
#'     dplyr::group_by(...) |>
#'     dplyr::mutate(
#'       date_lag=date %m+% lubridate::weeks(lag_n)
#'       ,"{{value_var}}_wow":={{value_var}}
#'     ) |>
#'     dplyr::select(-c(date,{{value_var}})) |>
#'     dplyr::ungroup()
#'
#'
#'   out_tbl <-  dplyr::left_join(
#'     full_tbl
#'     ,lag_table
#'     ,by=dplyr::join_by(date==date_lag,...)
#'   ) |>
#'     mutate(
#'       "{{value_var}}_wow" := dplyr::coalesce(.data[[rlang::englue("{{value_var}}_wow")]],0)
#'     )
#'
#'   return(out_tbl)
#'
#' }


#' week over week comparison for DBI objects
#'
#' @param .data tibble of values
#' @param ... optional columns to group by
#' @param date_var column with date var to aggregate by
#' @param value_var column with value to aggregate
#' @param lag_n the number of weeks to lag
#'
#' @return DBI object
#' @export
#'
#' @examples
wow_dbi <- function(.data,...,date_var,value_var,lag_n=1){

  ## create variables

  value_var_old <- deparse(substitute(value_var))
  value_var_interim<- paste0(value_var_old,"_","wow")
  value_var_final <- paste0(value_var_interim,"_",lag_n)


  full_dbi <- .data |>
    make_aggregation_dbi(...,date_var={{date_var}},value_var={{value_var}},time_unit="day") |>
    purrr::pluck("dbi")

  if(!missing(...)){
  # create lags
  lag_table <-  full_dbi |>
    dplyr::group_by(...) |>
    dbplyr::window_order(date) |>
    dplyr::mutate(
      !!value_var_interim:={{value_var}}
      ,date_lag=dplyr::sql(glue::glue("DATE + INTERVAL {lag_n} WEEK"))
    ) |>
    dplyr::select(-c(date,all_of(value_var_old))) |>
    dplyr::ungroup() |>
    mutate(
      date_lag=as.Date(date_lag)
    )
  }else{
    lag_table <-  full_dbi |>
      dbplyr::window_order(date) |>
      dplyr::mutate(
        !!value_var_interim:={{value_var}}
        ,date_lag=dplyr::sql(glue::glue("DATE + INTERVAL {lag_n} WEEK"))
      ) |>
      dplyr::select(-c(date,all_of(value_var_old))) |>
      dplyr::ungroup() |>
      mutate(
        date_lag=as.Date(date_lag)
      )
    }

  # join tables
out_tbl <-  dplyr::left_join(
  full_dbi
    ,lag_table
    ,by=dplyr::join_by(date==date_lag,...)
  ) |>
    mutate(
      !!value_var_final:=dplyr::sql(glue::glue("COALESCE({value_var_interim},0)"))
    ) |>
  select(-c(all_of(value_var_interim))) |>
  arrange(date)

  return(out_tbl)

}
#'
#'
#' #' Month over month values
#' #' @description
#' #' For datasets with daily granularity, this will calculate year over year values with some simple descriptive functions
#' #'
#' #' @param .data tibble of values
#' #' @param ... optional columns to group by
#' #' @param date_var column with date var to aggregate by
#' #' @param value_var column with value to aggregate
#' #' @param lag_n the number of weeks to lag
#' #'
#' #' @return tibble
#' #' @export
#' #'
#' #' @examples
#' #' mom(sales_tbl,date_var = order_date,value_var = quantity)
#' mom <- function(.data,...,date_var,value_var,lag_n=1){
#'
#'   # Validate inputs
#'   assertthat::assert_that(base::is.data.frame(.data), msg = "data must be a data frame")
#'   # Aggregate data based on provided time unit
#'
#'   full_tbl <-  .data |>
#'     make_aggregation_tbl(...,date_var={{date_var}},value_var={{value_var}},time_unit="day")
#'
#'   # Determine label for the time unit
#'
#'
#'   # Calculate difference and proportional change
#'
#'   lag_table <- full_tbl |>
#'     dplyr::group_by(...) |>
#'     dplyr::mutate(
#'       date_lag=date %m+% base::months(lag_n)
#'       ,"{{value_var}}_mom":={{value_var}}
#'     ) |>
#'     dplyr::select(-c(date,{{value_var}})) |>
#'     dplyr::ungroup()
#'
#'   print(full_tbl)
#'
#'
#'  out_tbl <-  dplyr::left_join(
#'     full_tbl
#'     ,lag_table
#'     ,by=dplyr::join_by(date==date_lag,...)
#'   ) |>
#'    mutate(
#'      "{{value_var}}_mom" := dplyr::coalesce(.data[[rlang::englue("{{value_var}}_mom")]],0)
#'    )
#'
#'   return(out_tbl)
#'
#' }


#' Month over month values
#'
#' @param .data tibble of values
#' @param ... optional columns to group by
#' @param date_var column with date var to aggregate by
#' @param value_var column with value to aggregate
#' @param lag_n the number of months to lag
#'
#' @return
#' @export
#'
#' @examples
mom_dbi <- function(.data,...,date_var,value_var,lag_n=1){

  ## create variables

  value_var_old <- deparse(substitute(value_var))
  value_var_interim<- paste0(value_var_old,"_","mom")
  value_var_final <- paste0(value_var_interim,"_",lag_n)


  full_dbi <- .data |>
    make_aggregation_dbi(...,date_var={{date_var}},value_var={{value_var}},time_unit="day") |>
    purrr::pluck("dbi")

  if(!missing(...)){
    # create lags
    lag_table <-  full_dbi |>
      dplyr::group_by(...) |>
      dbplyr::window_order(date) |>
      dplyr::mutate(
        !!value_var_interim:={{value_var}}
        ,date_lag=dplyr::sql(glue::glue("DATE + INTERVAL {lag_n} WEEK"))
      ) |>
      dplyr::select(-c(date,all_of(value_var_old))) |>
      dplyr::ungroup() |>
      mutate(
        date_lag=as.Date(date_lag)
      )
  }else{
    lag_table <-  full_dbi |>
      dbplyr::window_order(date) |>
      dplyr::mutate(
        !!value_var_interim:={{value_var}}
        ,date_lag=dplyr::sql(glue::glue("DATE + INTERVAL {lag_n} MONTH"))
      ) |>
      dplyr::select(-c(date,all_of(value_var_old))) |>
      dplyr::ungroup() |>
      mutate(
        date_lag=as.Date(date_lag)
      )
  }

  # join tables
  out_tbl <-  dplyr::left_join(
    full_dbi
    ,lag_table
    ,by=dplyr::join_by(date==date_lag,...)
  ) |>
    mutate(
      !!value_var_final:=dplyr::sql(glue::glue("COALESCE({value_var_interim},0)"))
    ) |>
    select(-c(all_of(value_var_interim))) |>
    arrange(date)

  return(out_tbl)

}



#'
#'
#'
#' #' Year over year values
#' #' @description
#' #' For datasets with daily granularity, this will calculate year over year values with some simple descriptive functions
#' #'
#' #' @param .data tibble of values
#' #' @param ... optional columns to group by
#' #' @param date_var column with date var to aggregate by
#' #' @param value_var column with value to aggregate
#' #' @param lag_n
#' #' @param time_unit
#' #'
#' #' @return tibble
#' #' @export
#' #'
#' #' @examples
#' #' yoy(fpaR::sales,date_var = order_date,value_var = quantity)
#' yoy <- function(.data,...,date_var,value_var,lag_n=1,time_unit="day"){
#'
#'   # Validate inputs
#'   assertthat::assert_that(base::is.data.frame(.data), msg = "data must be a data frame")
#'   assertthat::assert_that(time_unit %in% base::c("day","quarter","month", "year"), msg = "Time frame must be one of 'day', 'month',;quarter' or 'year'.")
#'   # Aggregate data based on provided time unit
#'
#'   full_tbl <-  .data |>
#'     make_aggregation_tbl(...,date_var={{date_var}},value_var={{value_var}},time_unit=time_unit) |>
#'     arrange(
#'       date
#'     )
#'
#'
#'   ## multiplication factor
#'
#'   multiply_options <- c("day"=1,"month"=12,"quarter"=4,"year"=1)
#'
#'
#'
#'   multiply_vec <- multiply_options[time_unit] |> base::unname()
#'
#'
#'
#'   if(time_unit %in% c("day")){
#'
#'   # Calculate difference and proportional change
#'
#'
#'   lag_tbl <- full_tbl |>
#'     dplyr::group_by(...) |>
#'     dplyr::mutate(
#'       date_lag=date %m+% lubridate::years(lag_n)
#'       ,"{{value_var}}_yoy":={{value_var}}
#'     ) |>
#'     dplyr::select(-c(date,{{value_var}})) |>
#'     dplyr::ungroup()
#'
#'   out_tbl <-  dplyr::left_join(
#'     full_tbl
#'     ,lag_tbl
#'     ,by=dplyr::join_by(date==date_lag,...)
#'   ) |>
#'     mutate(
#'       "{{value_var}}_yoy" := dplyr::coalesce(.data[[rlang::englue("{{value_var}}_yoy")]],0)
#'     )
#'     return(out_tbl)
#'
#'   } else {
#'
#'     out_tbl <-  full_tbl |>
#'       group_by(...) |>
#'       dplyr::mutate(
#'         "{{value_var}}_yoy":=dplyr::lag({{value_var}},n=(lag_n*multiply_vec))
#'       ) |>
#'       dplyr::ungroup()
#'
#'     return(out_tbl)
#'
#'   }
#'
#'
#' }

#' Year over year values for DBI objects
#'
#' @param .data tibble of values
#' @param ... optional columns to group by
#' @param date_var column with date var to aggregate by
#' @param value_var column with value to aggregate
#' @param lag_n number of time units to lagg
#' @param time_unit to return aggregate data by 'day','week','month','quarter' or 'year'
#'
#' @return
#' @export
#'
#' @examples
yoy_dbi <- function(.data,...,date_var,value_var,lag_n=1,time_unit="day"){

  ## create variables

  value_var_old <- deparse(substitute(value_var))
  value_var_interim<- paste0(value_var_old,"_","mom")
  value_var_final <- paste0(value_var_interim,"_",lag_n)


  full_dbi <- .data |>
    make_aggregation_dbi(...,date_var={{date_var}},value_var={{value_var}},time_unit="day") |>
    purrr::pluck("dbi")

  if(!missing(...)){
    # create lags
    lag_table <-  full_dbi |>
      dplyr::group_by(...) |>
      dbplyr::window_order(date) |>
      dplyr::mutate(
        !!value_var_interim:={{value_var}}
        ,date_lag=dplyr::sql(glue::glue("DATE + INTERVAL {lag_n} YEAR"))
      ) |>
      dplyr::select(-c(date,all_of(value_var_old))) |>
      dplyr::ungroup() |>
      mutate(
        date_lag=as.Date(date_lag)
      )
  }else{
    lag_table <-  full_dbi |>
      dbplyr::window_order(date) |>
      dplyr::mutate(
        !!value_var_interim:={{value_var}}
        ,date_lag=dplyr::sql(glue::glue("DATE + INTERVAL {lag_n} YEAR"))
      ) |>
      dplyr::select(-c(date,all_of(value_var_old))) |>
      dplyr::ungroup() |>
      mutate(
        date_lag=as.Date(date_lag)
      )
  }

  # join tables
  out_tbl <-  dplyr::left_join(
    full_dbi
    ,lag_table
    ,by=dplyr::join_by(date==date_lag,...)
  ) |>
    mutate(
      !!value_var_final:=dplyr::sql(glue::glue("COALESCE({value_var_interim},0)"))
    ) |>
    select(-c(all_of(value_var_interim))) |>
    arrange(date)

  return(out_tbl)
}

