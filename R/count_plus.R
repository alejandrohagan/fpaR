
#' Dplyr's count() function but with cumulative totals
#'
#' @param df dataframe
#' @param dim dimension to count
#' @param wt weighting dimension
#' @param sort sort results
#' @param round digits to round by
#'
#' @return
#' @export
#'
#' @examples
count_plus <- function(df,dim,wt=NULL,sort=TRUE,round=3){

  df %>%
    dplyr::count({{dim}},wt={{wt}},sort=sort,...) %>%
    dplyr::mutate(prop_total=
                    base::round(
                      base::cumsum(
                        n/base::max(base::cumsum(n))
                        ),
                      digits=round),
           unit_prop=
             base::round(
               dplyr::row_number()/base::max(dplyr::row_number()),
               digits=round),
           row_number=dplyr::row_number()
    )
}
