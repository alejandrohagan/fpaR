


## need to make break points logic to not sum to one (can't be greater htan one)
## need to check category values so it returns one greater than the variances
## need to change logic so you either pass a column or if not it defaults to count
## consider changing it so it joins in r
## use cte logic -- create cte
## then create a new table with the category arguments (maybe in the abc class??)


#' Classify a group by proportion of a variable (A,B,C)
#' @description
#' This returns a table that will segment your data into A,B or C segments based on custom
#' thresholds
#'
#' @param .value the column to aggregate
#' @param category_values the break points you want for your categories
#' @param fn 'sum' to aggregate the value or 'n' to do the count of the group
#' @param .data tibble or DBI object
#'
#' @return a tibble
#' @export
#'
#' @examples
#'
#' customer_abc <- abc(fpaR::sales,customer_key,dim=quantity,a=.7,b=.2,c=.1)
abc_fn <- function(.data,.value,category_values,fn=c("sum")){

  value_quo <- enquo(.value)
  value_chr <- as_name(value_quo)

 abc_obj <-   abc(data = .data, value_vec = value_chr,category_values = category_values)

  ## function check
   fn_vec <-  rlang::arg_match(
      arg=fn
      ,values= c("sum","n")
      ,multiple = FALSE
    )

  # create summary table

  if(fn_vec=="sum"){

  summary_tbl  <- abc_obj@data |>
      dplyr::summarize(
        value=sum(!!abc_obj@value_quo)
        ,.groups="drop"
      ) |>
      dbplyr::window_order(desc(value))

  } else {

  summary_tbl <- abc_obj@data |>
    dplyr::summarize(
      value=dplyr::n()
      ,.groups="drop"
    ) |>
      dbplyr::window_order(desc(value))

  }


   names(abc_obj@category_values) <- abc_obj@category_names

  category_tbl <-  abc_obj@category_values |>
     stack() |>
     tibble::as_tibble() |>
     dplyr::rename(
       category_value=values,category_name=ind
     )

  stats_tbl <- summary_tbl |>
    dplyr::mutate(
      cum_sum=cumsum(value)
      ,prop_total=value/max(cum_sum)
      ,cum_prop_total=cumsum(prop_total)
      ,row_id=dplyr::row_number()
      ,max_row_id=max(row_id)
      ,cum_unit_prop=row_id/max_row_id
    ) |>
    dplyr::collect()

 out <- stats_tbl |>
    dplyr::left_join(
      category_tbl
      ,by=dplyr::join_by(dplyr::closest(cum_unit_prop<=category_value))
    ) |>
   arrange(category_name)




  return(out)


}

sales |>
  group_by(customer_key) |>
  abc_fn(.value = margin,category_values = c(.05,.10,.85),fn = "sum") |>
  arrange(cum_unit_prop) |> print(n=100)
