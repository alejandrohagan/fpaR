
#' Augments count function give proportion and cumulative proportion and sorting options (dplyr and dbplyr friendly)
#'
#' @param .data data frame or DBI object
#' @param ... columns you want to group by
#' @param .sort if you want to sort from largest (1), reverse(-1) or nothing 0
#' @param wt if you want to weight the count (eg. value or price)
#'
#' @return a tibble of values
#' @export
#'
#' @examples
#' ggplot2::diamonds %>% count_plus(color,cut,wt=price)
count_plus <- function(.data,...,wt,.sort=1,na.rm=TRUE){


# first mimics dplyr::count() functionality
  if(!missing(wt)) {
temp_data1 <-
   dplyr::count(
     x=.data
     ,wt={{wt}}
     ,...=...
     )
    } else{

  temp_data1 <-
    dplyr::count(
      x=.data
      ,...=...
    )
    }


    if(.sort==1) {
      tempdata_2 <- temp_data1 %>%   dplyr::arrange(dplyr::desc(n))
    } else if (.sort==-1) {
      tempdata_2 <-temp_data1 %>% dplyr::arrange(n)
    } else {
      tempdata_2 <-temp_data1

    }


  tempdata_3 <- tempdata_2 %>%
    dplyr::mutate(
      row_id=dplyr::row_number()
      ,prop_n=n/sum(n,na.rm=na.rm)
      ,cumsum_prop_n=cumsum(prop_n)
      ) %>%
    dplyr::relocate(row_id)


  obj_class <-  stringr::str_flatten_comma(base::class(tempdata_3),last = " or ")

  cli::cli_alert_info("returning obj of '{obj_class}' class")

  return(tempdata_3)

}



