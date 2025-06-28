abc <- function(.data,.value,category_values,type=c("sum")){

  # capture value as text

  value_quo <- rlang::enquo(.value)
  value_chr <- rlang::as_name(value_quo)

  abc_obj <-   segment(data = .data, value_vec = value_chr,category_values = category_values,type=type,fn = fn(fn_exec=abc_fn))


return(abc_obj)
}


#' Classify a group by proportion of a variable (A,B,C,...)
#' @description
#' This returns a table that will segment your data into A,B or C segments based on custom
#' thresholds
#'
#' @param .value the column to aggregate
#' @param category_values the break points you want for your categories, each should be less than or equal to 1
#' @param fn 'sum' to aggregate the value or 'n' to do the count of the group
#' @param .data tibble or DBI object
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

  if(x@type=="sum"){

  summary_tbl  <- x@data |>
      dplyr::summarize(
        value=sum(!!x@value_quo)
        ,.groups="drop"
      ) |>
      dbplyr::window_order(desc(value))

  } else {

  summary_tbl <- x@data |>
    dplyr::summarize(
      value=dplyr::n()
      ,.groups="drop"
    ) |>
      dbplyr::window_order(desc(value))

  }


  # category_tbl <-  abc_obj@category_values |>
  #    stack() |>
  #    tibble::as_tibble() |>
  #    dplyr::rename(
  #      category_value=values,category_name=ind
  #    )


   ## create summary stats table

  stats_dbi <- summary_tbl |>
    dplyr::mutate(
      cum_sum=cumsum(value)
      ,prop_total=value/max(cum_sum,na.rm=TRUE)
      ,cum_prop_total=cumsum(prop_total)
      ,row_id=dplyr::row_number()
      ,max_row_id=max(row_id,na.rm=TRUE)
      ,cum_unit_prop=row_id/max_row_id
    )


  # assign names to category list
  names(x@category_values) <- x@category_names

  ## create sql scripts for category CTE----------

  category_values_vec <- glue::glue("({x@category_values}")

  category_names_vec <- glue::glue("'{names(x@category_values)}')")


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


