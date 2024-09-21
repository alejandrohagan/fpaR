
#' Title
#'
#' @param .data
#' @param column
#' @param fn
#'
#' @return
#' @export
#'
#' @examples
pp <- function(.data=sales,column,fn="row") {

  column_names <- colnames(.data)

  column_str <- rlang::as_name(rlang::enquo(column))

  # Capture the column name and aggregation function

  column_adj_str <- rlang::sym(paste0("lag_",column_str))

  # calculate a few tests

  assertthat::assert_that(
    column_str %in% column_names
    ,msg = "Please ensure that the column exists in the dataset"
  )


  assertthat::assert_that(
    .data |> dplyr::pull(column_str) |> is.numeric()
    ,msg = "Please ensure column is numeric"
  )

  if(fn=="row"){

    return(column_adj_str)

  }

  # Create a dynamic call to wrap the aggregation function around the column

  fn_column <- rlang::call2(fn, column_adj_str,na.rm=TRUE)

  return(fn_column)

}


#' Title
#'
#' @param .data
#' @param column
#' @param fn
#'
#' @return
#' @export
#'
#' @examples
cp <- function(.data=sales,column,fn="row") {

  column_names <- colnames(.data)

  column_str <- rlang::as_name(rlang::enquo(column))


  # calculate a few tests

  assertthat::assert_that(
    column_str %in% column_names
    ,msg = "Please ensure that the column exists in the dataset"
  )


  assertthat::assert_that(
    .data |> dplyr::pull(column_str) |> is.numeric()
    ,msg = "Please ensure column is numeric"
  )

  if(fn=="row"){

    return(rlang::sym(column_str))

  }

  # Create a dynamic call to wrap the aggregation function around the column

  fn_column <- rlang::call2(fn, rlang::sym(column_str),na.rm=TRUE)

  return(fn_column)

}

#' Title
#'
#' @param .data
#' @param column
#' @param fn
#'
#' @return
#' @export
#'
#' @examples
d <- function(.data=sales,column,fn="row") {

  column_names <- colnames(.data)

  column_str <- rlang::as_name(rlang::enquo(column))

  # Capture the column name and aggregation function

  column_adj_str <- rlang::sym(paste0("delta_",column_str))

  # calculate a few tests

  assertthat::assert_that(
    column_str %in% column_names
    ,msg = "Please ensure that the column exists in the dataset"
  )


  assertthat::assert_that(
    .data |> dplyr::pull(column_str) |> is.numeric()
    ,msg = "Please ensure column is numeric"
  )

  if(fn=="row"){

    return(column_adj_str)

  }

  # Create a dynamic call to wrap the aggregation function around the column

  fn_column <- rlang::call2(fn, column_adj_str,na.rm=TRUE)

  return(fn_column)

}

#' Title
#'
#' @param formula
#' @param data
#'
#' @return
#' @export
#'
#' @examples
factor <- function(formula,data){


  formula  <- as.formula(formula)
  lhs      <- rlang::f_lhs(formula)

  lhs_text <- rlang::expr_text(lhs)
  rhs_text <- rlang::f_text(formula)

  #pattern to split by arthmetic operators
  call_pattern     <- "(?<!\\w)[\\*\\+/](?!\\w)\\s*|\\s+-\\s*"
  operator_pattern <- "[-*\\+]+"

  rhs_text_lst <- strsplit(rhs_text,pattern, perl = TRUE) |> purrr::pluck(1)

  colnames_lst <- purrr::map(rhs_text_lst,rlang::parse_expr) |> purrr::map(rlang::eval_tidy)


  # replace each variable one by one

  str_out <- purrr::reduce2(
    .x=rhs_text_lst
    ,.y=colnames_lst
    ,.f = \(prev,.x,.y){

      prev |> gsub(.x,.y,x=_,fixed = TRUE)

    }
    ,.init = rhs_text
  )

  # add to formula new column
  out_obj <- data |>
    dplyr::mutate(!!lhs_text:=!!parse_expr(str_out))

  return(out_obj)
}

