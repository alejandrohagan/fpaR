
#' Augments count funiton to give proportion and cumlative proportion and sorting options (dplyr,sparklyr friendly)
#'
#' @param .data dataframe
#' @param ... columns you want to groupby
#' @param .sort if you want to sort from largest (1), reverse(-1) or nothing 0
#' @param wt if you want to weight the count (eg. value or price)
#'
#' @return a tibble of values
#' @export
#'
#' @examples
#' ggplot2::diamonds %>% count_plus(color,cut,wt=price)
count_plus <- function(.data,...,wt,.sort=1){

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


  tempdata_3 <- tempdata_2 %>% mutate(
    row_id=dplyr::row_number()
    ,prop_n=n/base::sum(n)
    ,cumsum_prop_n=base::cumsum(prop_n)
  ) %>% dplyr::relocate(row_id)


  obj_class <-  stringr::str_flatten_comma(base::class(tempdata_3),last = " or ")
cli::cli_alert_info("returning obj of '{obj_class}' class")

  return(tempdata_3)
}



library(tidyverse)




#
# compare <- function(.groups){
#
#   tbl1 <- head(diamonds,10)
#   tbl2 <- head(diamonds,30) %>% rename(cut2=cut,color2=color)
#
#
#   con <- DBI::dbConnect(duckdb::duckdb())
#
#   duckdb::duckdb_register(con,"t1",tbl1)
#   duckdb::duckdb_register(con,"t2",tbl2)
#
#   DBI::dbListTables(con)
#   tbl1_db <- tbl(con,"t1")
#   tbl2_db <- tbl(con,"t2")
#
#   lookup_vec <- tibble(id=c("cut_id","color_id")
#                        ,tbl1_col=c("cut","color")
#                        ,tbl2_col=c("cut2","color2")
#   )
#   selected_vec <- filter(lookup_vec,id %in% .groups)
#
#
#
#
#   temp_1 <- tbl1_db %>%
#     group_by(pick(selected_vec$tbl1_col)) %>%
#     summarize(mean3=mean(price)
#               ,.groups = "drop")
#
#   temp_2 <- tbl2_db %>%
#     group_by(pick(selected_vec$tbl2_col)) %>%
#     summarize(mean_2=mean(price),
#               .groups = "drop")
#
#   join_args <-   map2(
#     .x=selected_vec$tbl1_col
#     ,.y=selected_vec$tbl2_col
#     ,~glue::glue("{.x}=={.y}")
#   ) %>% str_flatten_comma()
#
#   join_form <- paste0("join_by(",join_args,")")
#
#
#   join_tbl <-  temp_1 %>%
#     left_join(
#       temp_2
#       ,by = eval(parse_expr(join_form))
#     )
#   return(join_tbl)
#
# }
#
#
# compare(c("cut_id","color_id"))
#
# price <- 10
#
# jana <- 100
#
# diamonds %>%
#   mutate(
#     test=cumsum(.data[["price"]])
#     ,test3=cumsum(.data$price)
#     ,test2=cumsum(.env$jana)
#     ,test4=cumsum(.env[["price"]])
#
#   )

