
#' Proportion and Cumsum Portion
#'
#' @param .data dataframe
#' @param ... columns you want to groupby
#' @param .wt if you want to weight it by a variable
#' @param .sort if you want to sort from largest (1), reverse(-1) or nothing 0
#'
#' @return a tibble of values
#' @export
#'
#' @examples
#' ggplot2::diamonds %>% count_plus(color,cut,.wt=price)
count_plus <- function(.data,...,.wt=FALSE,.sort=1){

  if(.wt!=FALSE) {
temp_data1 <-   .data %>%
   dplyr::count(
     dplyr::pick(...)
     ,wt={{.wt}}
     )
    }

  temp_data1 <-   .data %>%
    dplyr::count(
      pick(...)
    )


    if(.sort==1) {
      tempdata_2 <- temp_data1 %>%   dplyr::arrange(dplyr::desc(n))
    } else if (.sort==-1) {
      tempdata_2 <-temp_data1 %>% dplyr::arrange(n)
    } else {
      tempdata_2 <-temp_data1

    }

  tempdata_3 <- tempdata_2 %>% mutate(
    row_id=dplyr::row_number()
    ,prop_n=n/base::sum(n)
    ,cumsum_prop_n=base::cumsum(prop_n)
  ) %>% dplyr::relocate(row_id)


  return(tempdata_3)
}







