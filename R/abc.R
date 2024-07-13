
#' Classify a group by proportion of a variable (A,B,C)
#' @description
#' This returns a table that will segment your data into A,B or C segments based on custom
#' thresholds
#'
#' @param .data tibble or DBI object
#' @param dim dimension to classify by (should be additive and positive)
#' @param a initial segment threshold expressed as a percentage eg .7
#' @param b incremental segment threshold expressed as a percentage eg .26
#' @param c final segment threshold expressed as a percentage eg .04
#' @param func either "sum" or "n"; sum to sum your dim or n for row count
#' @param ... columns  to group by
#'
#' @return a tibble or DBI object
#' @export
#'
#' @examples
#'
#' customer_abc <- abc(contoso,customer_key,dim=margin,a=.7,b=.2,c=.1)
abc <- function(.data,...,dim,a=.7,b=.26,c=.04,func=c("sum")){

  ## function check

 func_vec <-  match.arg(
    func
    ,choices = c("sum","n")
  )

  # input validation

  assertthat::assert_that(a+b+c==1,msg = "A, B, and C must sum to 1")

  # numeric dim validation check

  # declare variables

  dim_str <- base::deparse(base::substitute(dim))


  dbi_flag <- dplyr::if_else(
    base::any(base::class(.data) %in% "tbl_dbi")
    ,TRUE
    ,FALSE
    )


  ## sql
  if(dbi_flag){

    assertthat::assert_that(

      column_type_vec <- .data |> dplyr::select({{dim}}) |>  utils::head(1) |> dplyr::collect() |> dplyr::pull({{dim}}) |> base::is.numeric()

      ,msg = "dim must be a numeric column"
      )

  } else{

  ## non-sql

  assertthat::assert_that(base::is.numeric(.data[[dim_str]]),msg = "dim must be a numeric column")

  }

  ## begin function


  if(func_vec=="sum"){

    step1 <- .data |>
      dplyr::group_by(pick(...)) |>
      dplyr::summarize(
        var=sum({{dim}})
        ,.groups="drop"
      )

  } else {

     step1 <- .data |>

    dplyr::group_by(pick(...)) |>
    dplyr::summarize(
      var=dplyr::n()
      ,.groups="drop"
    )
  }

    if(dbi_flag){

     temp <-  step1 |>
      dbplyr::window_order(desc(var))

    } else{
      temp <-  step1 |>
        dplyr::arrange(dplyr::desc(var))

    }

  out <-   temp |>
    dplyr::mutate(
      cum_sum=cumsum(var)
      ,prop_total=var/max(cum_sum)
      ,cum_prop_total=base::cumsum(prop_total)
      ,row_id=dplyr::row_number()
      ,max_row_id=max(row_id)
      ,cum_unit_prop=row_id/max_row_id
      ,dim_category=
        dplyr::case_when(
          cum_prop_total  <=a      ~ "A"
          ,cum_prop_total <=(a+b)  ~ "B"
          ,.default                = "C"
        ),
      dim_threshold=
        dplyr::case_when(
          dim_category=="A"~a
          ,dim_category=="B"~(a+b)
          ,.default                        = c
        )
    ) |>
    dplyr::select(-c(prop_total,cum_sum,max_row_id))

  return(out)


}
