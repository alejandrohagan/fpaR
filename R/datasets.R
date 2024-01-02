
#' Create duckdb objects of contoso objects
#'
#' @return
#' @export
#'
#' @examples
create_duckdb_data <- function(){

sales_tbl <- fpaR::contoso_fact_sales %>%
  mutate(DateKey=lubridate::mdy(DateKey))

dates_tbl <- fpaR::contoso_dim_date

channel_tbl <- fpaR::contoso_dim_channel

product_tbl <- fpaR::contoso_dim_product

subcategory_tbl <- fpaR::contoso_dim_product_subcategory

promotion_tbl <- fpaR::contoso_dim_promotion

con <- DBI::dbConnect(duckdb::duckdb())

list_tbls <- list(
  sales_tbl
  ,dates_tbl
  ,channel_tbl
  ,product_tbl
  ,subcategory_tbl
  ,promotion_tbl

)

tbl_names <- list("sales","dates","channel","product","subcategory","promotion")

purrr::map2(.x=list_tbls,.y=tbl_names, ~duckdb::duckdb_register(con,.y,.x,overwrite = TRUE))

objects_db <- purrr::map(.x=tbl_names,~dplyr::tbl(con,.x))


contoso_sales_db <<- objects_db[1] %>% pluck(1)
contoso_dates_db <<- objects_db[2]%>% pluck(1)
contoso_channel_db <<- objects_db[3]%>% pluck(1)
contoso_product_db <<- objects_db[4]%>% pluck(1)
contoso_subcategory_db <<- objects_db[5]%>% pluck(1)
contoso_promotion_db <<- objects_db[6]%>% pluck(1)

  # return(contoso_sales_db)
  # return(contoso_dates_db)
  # return(contoso_channel_db)
  # return(contoso_product_db )
  # return(contoso_promotion_db)
  # return(contoso_subcategory_db)
}


#' Create spark data objs of the contoso model
#'
#' @return
#' @export
#'
#' @examples
create_spark_data <- function(){

  sales_tbl <- fpaR::contoso_fact_sales %>% dplyr::mutate(DateKey=lubridate::mdy(DateKey))

  dates_tbl <- fpaR::contoso_dim_date

  channel_tbl <- fpaR::contoso_dim_channel

  product_tbl <- fpaR::contoso_dim_product

  subcategory_tbl <- fpaR::contoso_dim_product_subcategory

  promotion_tbl <- fpaR::contoso_dim_promotion

  sc <- sparklyr::spark_connect(master="local")

  # take a data object and copy it too a spark object

  list_tbls <- list(
    sales_tbl
    ,dates_tbl
    ,channel_tbl
    ,product_tbl
    ,subcategory_tbl
    ,promotion_tbl
  )


  tbl_names <- list("sales_spk","dates_spk","channel_spk","product_spk","subcategory_spk","promotion_spk")

  obj_spark=purrr::map2(.x=list_tbls,.y=tbl_names, ~sparklyr::copy_to(sc,df = .x,name = .y,overwrite = TRUE))

  contoso_sales_spk <<- obj_spark[1] %>% pluck(1)
  contoso_dates_spk <<- obj_spark[2] %>% pluck(1)
  contoso_channel_spk <<- obj_spark[3] %>% pluck(1)
  contoso_product_spk <<- obj_spark[4] %>% pluck(1)
  contoso_subcategory_spk <<- obj_spark[5] %>% pluck(1)
  contoso_promotion_spk <<- obj_spark[6] %>% pluck(1)

  # return(contoso_sales_spk)
  # return(contoso_dates_spk)
  # return(contoso_channel_spk)
  # return(contoso_product_spk)
  # return(contoso_subcategory_spk)
  # return(contoso_promotion_spk)
}

