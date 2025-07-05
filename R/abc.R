#' @title ABC classification function
#' @name abc
#'
#' @param .data tibble or dbi object (either grouped or ungrouped)
#' @param category_values vector of break points between 0 and 1
#' @param .value optional: if left blank,[abc()] will use the number of rows per group to categorize, alternatively you can pass a column name to categorize
#'
#' @description
#' -  For your group variable, [abc()]  will categorize which groups make up what proportion of the totals according to the category_values that you have entered
#' -  The function returns a segment object which prints out the execution steps and actions that it will take to categorize your data
#' -  Use [calculate] to return the results
#' @details
#' -  This function is helpful to understand which groups of make up what proportion of the cumulative contribution
#' -  If you do not provide a `.value` then it will count the transactions per group, if you provide `.value` then it will [sum()] the `.value` per group
#' -  The function creates a `segment` object, which pre-processes the data into its components
#'
#' @returns segment object
#' @export
#'
#' @examples
#' abc(sales,c(.1,.5,.7,.96,1),.value=margin)
abc <- function(.data,category_values,.value){

  # capture value as text
    # .data <- sales
    # value_vec <- value_vec <- deparse(substitute(margin))
    # category_values <- c(.4,.7,.8,.96,1)


  if(!missing(.value)){

  value_vec <- deparse(substitute(.value))

  }else{

    value_vec="n"

  }


  x <-   segment(
    data                      = data(.data,date_vec = NA,calendar_type = NA)
    ,value                    = value(value_vec = value_vec,new_column_name_vec = "abc")
    ,category                 = category(category_values=category_values)
    ,fn = fn(
      fn_exec                 = abc_fn
      ,fn_name                = "ABC"
      ,fn_long_name           = "ABC Classification"
      ,lag_n                  = NA_integer_
      ,new_date_column_name  = NA_character_
      )
    ,action=action(
      value = c("proportion of total","Aggregate")
      ,method= "This calculates a rolling cumulative distribution of variable
      and segments each group member's contribution by the break points provided.
      Helpful to know which group member's proportational contribution to the total.
      ")
    )

  assertthat::assert_that(
    x@data@group_indicator,msg=cli::format_error(message="{.fn abc} expects a grouped tibble or dbi object. Please use {.fn group_by} to pass a grouped objected")
  )


return(x)

}


#' Classify a group by proportion of a variable (A,B,C,...)
#'
#' @param x segment object
#'
#' @description
#' This returns a table that will segment your data into A,B or C segments based on custom
#' thresholds
#'
#' @return a dbi objection
#' @export
#'
abc_fn <- function(x){

  #example

  # .data <- fpaR::sales |>
  #   group_by(customer_key)
  # value_chr <- "margin"
  # category_values <- c(.2,.5,.3)
  # fn="n"

  # create summary table by the group

  if(x@value@value_vec!="n"){

  summary_dbi  <- x@data@data |>
      dplyr::summarize(
        !!x@value@new_column_name_vec:=sum(!!x@value@value_quo,na.rm=TRUE)
        ,.groups="drop"
      ) |>
      dbplyr::window_order(desc(!!x@value@new_column_name_quo))

  } else {

  summary_dbi <- x@data@data |>
    dplyr::summarize(
      !!x@value@new_column_name_vec:=dplyr::n()
      ,.groups="drop"
    ) |>
      dbplyr::window_order(desc(!!x@value@new_column_name_quo))

  }


  # category_tbl <-  abc_obj@category_values |>
  #    stack() |>
  #    tibble::as_tibble() |>
  #    dplyr::rename(
  #      category_value=values,category_name=ind
  #    )


   ## create summary stats table

  stats_dbi <- summary_dbi |>
    dplyr::mutate(
      cum_sum=cumsum(!!x@value@new_column_name_quo)
      ,prop_total=!!x@value@new_column_name_quo/max(cum_sum,na.rm=TRUE)
      ,cum_prop_total=cumsum(prop_total)
      ,row_id=dplyr::row_number()
      ,max_row_id=max(row_id,na.rm=TRUE)
      ,cum_unit_prop=row_id/max_row_id
    )


  # assign names to category list
  names(x@category@category_values) <- x@category@category_names

  ## create sql scripts for category CTE----------

  category_values_vec <- glue::glue("({x@category@category_values}")

  category_names_vec <- glue::glue("'{names(x@category@category_values)}')")


  sql_values <- stringr::str_flatten_comma(paste0(category_values_vec,", ",category_names_vec))

  sql_base <- "WITH my_cte (category_value, category_name) AS (
    VALUES "

  sql_end <- ") select * from my_cte"

  sql_category_dbi <- paste0(sql_base,sql_values,sql_end)

  ## grab connection from the summary tbl

  con <- dbplyr::remote_con(stats_dbi)

  ## create category table to be used later
  category_dbi <-  dplyr::tbl(con,dplyr::sql(sql_category_dbi))

  # join together stats table and category table and then filter to reduce duplicate matches
  out <- stats_dbi |>
    dplyr::left_join(
      category_dbi
      ,by=dplyr::join_by(cum_unit_prop<=category_value)
    ) |>
  dplyr::mutate(
      delta=category_value-cum_unit_prop
    ) |>
  dplyr::mutate(
      row_id_rank=rank(delta)
      ,.by=row_id
    ) |>
    dplyr::filter(
      row_id_rank==1
    ) |>
  dplyr::select(-c(row_id_rank,delta))

  ## previous ------
#
#  out <- stats_tbl |>
#     dplyr::left_join(
#       category_tbl
#       ,by=dplyr::join_by(dplyr::closest(cum_unit_prop<=category_value))
#     ) |>
#    arrange(category_name)

  return(out)


}



#' Title
#'
#' @param .data tibble or dbi object
#' @param .date date column
#' @param .value id column
#' @param calendar_type standard calendar
#' @param time_unit do you want daily, montly, quarterly or annual view
#' @description
#' A database is remake of 'https://github.com/PeerChristensen/cohorts' cohort package combining
#' chort_table_month, cohort_table_year, cohort_table_day into a single package.
#' Re-written to be database friendly tested against snowflake and duckdb databases
#'
#' @returns segment object
#' @export
#'
cohort <- function(.data,.date,.value,calendar_type,time_unit="month",period_label=FALSE){

  # .data <- sales
  # .date <- "order_date"
  # .value <- "customer_key"


  x <-  segment(
    data=data(
      .data
      ,calendar_type = "standard"
      ,date_vec = rlang::as_label(rlang::enquo(.date))
    )
    ,category = category(category_values = 0)
    ,fn = fn(
      fn_exec = cohort_fn
      ,fn_name = "cohort"
      ,fn_long_name = "Time Based Cohort"
      ,new_date_column_name = "cohort_date"
      ,shift = NA_character_
      ,compare = NA_character_
      ,lag_n = NA_integer_
      ,label=period_label
    )
    ,time_unit = time_unit(value=time_unit)
    ,value = value(
      value_vec = rlang::as_label(rlang::enquo(.value))
      ,new_column_name_vec = "cohort"
    )
    ,action = action(
      value="count distinct"
      ,method="
      This segments groups based on a shared time related dimension
      so you can track a cohort's trend over time
      "

      )
  )

  return(x)
}



#' Title
#'
#' @param x segment object
#' @param period_label optional label
#'
#' @returns function
#'
cohort_fn <- function(x){

  # .data <- cohorts::online_cohorts |> janitor::clean_names()


  # x <- cohort(.data,.date=invoice_date,calendar_type = "standard",.value = customer_id,time_unit = "day")


  ## validation tests

  #




  summary_dbi <-   x@data@data  |>
    dplyr::mutate(date = lubridate::floor_date(!!x@data@date_quo,unit=!!x@time_unit@value)) |>
    dplyr::group_by(!!x@value@value_quo) |>
    dplyr::mutate(cohort_date = min(date,na.rm=TRUE)) |>
    # dbplyr::window_order(date) |>
    dplyr::group_by(cohort_date, date) |>
    dplyr::summarise(
      !!x@value@new_column_name_vec:= dplyr::n_distinct(!!x@value@value_quo)
      ,.groups = "drop"
    ) |>
    dplyr::mutate(period_id=dplyr::sql("DENSE_RANK() OVER (ORDER BY date)"))


  # complete_summary_dbi <- fpaR::seq_date_sql(
  #   start_date = x@data@min_date
  #   ,end_date = x@data@max_date
  #   ,time_unit = x@time_unit@value
  #   ,con=dbplyr::remote_con(x@data@data)
  #   ) |>
  #   dplyr::cross_join(
  #     summary_dbi |> select(-date)
  #   )

  # complete_summary_dbi

  # min_date <- min(complete_summary_dbi |> pull(date),na.rm=TRUE)
  #
  # max_date <- max(complete_summary_dbi |> pull(date),na.rm=TRUE)


  if(!x@fn@label){

    out <- summary_dbi |>
      dplyr::select(-period_id) |>
      tidyr::pivot_wider(
        names_from=date
        ,values_from=!!x@value@new_column_name_quo
        ,values_fill=0
      ) |>
      dbplyr::window_order(cohort_date) |>
      dplyr::mutate(
        cohort_id = dplyr::row_number()
      ) |>
      dplyr::relocate(
        cohort_date
        ,cohort_id
        ,tidyselect::any_of(
          as.character(
            as.Date(x@data@min_date:x@data@max_date)
          )
        )
      ) |>
      janitor::clean_names()

  }else{

    out <- summary_dbi |>
      dplyr::select(-date) |>
      tidyr::pivot_wider(
        names_from=period_id
        ,values_from=x@value@new_column_name_quo
        ,names_prefix = "p_"
        ,values_fill = 0
      ) |>
      dplyr::ungroup() |>
      # dplyr::compute() |>
      dbplyr::window_order(cohort_date) |>
      # dplyr::arrange(cohort_date) |>
      dplyr::mutate(cohort_id = dplyr::row_number()) |>
      dplyr::relocate(
        cohort_date
        ,cohort_id
        ,tidyselect::num_range(prefix="p_",1:last_col())
      )

  }
  return(out)

}



