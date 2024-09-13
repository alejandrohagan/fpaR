#' Validate an input is YYYY-MM-DD format
#'
#' @param x date column
#'
#' @return logical
#' @export
#'
#' @examples
#' is_yyyy_mm_dd("2024-01-01")
is_yyyy_mm_dd <- function(x) {
  grepl("^\\d{4}-\\d{2}-\\d{2}$", x)
}



#' Convert quoted or unquoted input to string
#'
#' @param x quoted or unquoted input
#'
#' @return string
#' @export
#'
#' @examples
#' convert_input_to_string(hello)
convert_input_to_string <- function(x) {


  var_quo <- rlang::enquo(x)
  var_expr <- rlang::quo_get_expr(var_quo)



    if (rlang::is_symbolic(var_expr) || rlang::is_call(var_expr)) {

    out <- as_label(var_expr)

    return(out)

  } else {
    out <- x

    return(out)
  }

}



#' Capture dot arguments and turn into strings
#'
#' @param ... dot args
#'
#' @return strings
#' @export
#'
#' @examples
#' convert_dots_to_string(hello,how,are,you)
convert_dots_to_string <- function(...){

  args <- rlang::enquos(...)

  group_var <- map_chr(args,\(x) convert_input_to_string({{x}}))

  return(group_var)

}
