#' Helps creates CTE queries
#'
#' @param query sql query
#' @param query_name CTE name
#' @param order CTE order either first, middle, last or single
#'
#' @return sql text object
#' @export
#'
#' @examples
#' query <- dplyr::sql("select * from sales")
#' with(query=query,query_name=sales_cte,order='first')
with <- function(query,query_name,order=c("middle")){


  query_name <- rlang::as_label(rlang::enquo(query_name))

  initial <- dplyr::sql(
    paste0("WITH ",query_name," AS ","(",query,"),")
  )

  middle <- dplyr::sql(
    paste0(query_name," AS ","(",query,"),")
  )


  last <- dplyr::sql(
    paste0(query_name," AS ","(",query,")")
  )

  single <- dplyr::sql(
    paste0("WITH ",query_name," AS ","(",query,")")
  )



  ordec_vec <- match.arg(
    stringr::str_to_lower(order)
    ,choices=c("first","last","middle","single")
  )

  if(ordec_vec=="first"){

    return(initial)

  }else if(ordec_vec=="middle"){

    return(middle)

  }else if(ordec_vec=="single"){

    return(single)

  }else{

    return(last)

  }
}




#' Create duckdb versions of Contoso datasets
#'
#' @return DBI objects
#' @export
#'
#' @examples
#' contonso_duckdb()
contonso_duckdb <- function(){

  con <- suppressWarnings(DBI::dbConnect(duckdb::duckdb()))


  duckdb::duckdb_register(con,"sales",fpaR::sales,overwrite = TRUE)
  duckdb::duckdb_register(con,"product",fpaR::product,overwrite = TRUE)
  duckdb::duckdb_register(con,"customer",fpaR::customer,overwrite = TRUE)
  duckdb::duckdb_register(con,"date",fpaR::date,overwrite = TRUE)
  duckdb::duckdb_register(con,"fx",fpaR::fx,overwrite = TRUE)
  duckdb::duckdb_register(con,"store",fpaR::store,overwrite = TRUE)


  sales <- dplyr::tbl(con,dplyr::sql("select * from sales"))
  product <- dplyr::tbl(con,dplyr::sql("select * from product"))
  customer <- dplyr::tbl(con,dplyr::sql("select * from customer"))
  store <- dplyr::tbl(con,dplyr::sql("select * from store"))
  fx <- dplyr::tbl(con,dplyr::sql("select * from fx"))
  date <- dplyr::tbl(con,dplyr::sql("select * from date"))


  out <- base::list(
    sales=sales
    ,product=product
    ,customer=customer
    ,store=store
    ,fx=fx
    ,date=date
  )


  return(out)
}


