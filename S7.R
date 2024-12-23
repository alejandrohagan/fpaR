library(tidyverse)
library(assertthat)
library(S7)
devtools::load_all()

drv <- duckdb::duckdb(dbdir="/home/hagan/database.duckdb")

con <- DBI::dbConnect(duckdb::duckdb())

duckdb::duckdb_register(con,"sales",fpaR::sales)


sales_db <- dplyr::tbl(con,sql("select * from sales"))

sales <- fpaR::sales

  sales |>
  calendar(date_column,type="standard") |> # returns structured object clarifies calendar table
  totalyd(value) |>  # clarifies the action (shift, aggregate or compare)
  calculate()  # returns either a tbl or dbi object



data_tbl <-
  n

data_tbl()






## sub class


method <- new_class(
  name="method"
  ,properties=list(
    type=new_property(
    class=class_character
    ,default="standard"
    )
  )
)


calendar_tbl <- new_class(
  name="calendar_tbl"
  ,properties = list(
          data=new_property(
            class=class_data.frame
            ,default = NULL
          )
          ,date=new_property(
            class=class_any
            ,default = NULL
          )
          ,method=new_property(
            class=method
            ,default = method()
          )

          ,date_vec=new_property(
            class=class_character
            ,getter = \(self){
              as.character(self@date)
            }
          )

          ,nrow=new_property(
            class=class_numeric
            ,default=0
            ,getter = \(self){
              nrow(self@data)
            }
          )
          ,ncol=new_property(
            class=class_numeric
            ,default=0
            ,getter= \(self){
              ncol(self@data)
            }
          )
          ,group_indicator=new_property(
            class=class_logical
            ,getter=\(self){

              dplyr::if_else(!purrr::is_empty(groups(self@data)),TRUE,FALSE)

            }
          )
          ,group_quo=new_property(
            class=class_any
            ,getter = \(self){

              dplyr::groups(self@data)
            }
          )
          ,group_vec=new_property(
            class=class_any
            ,getter = \(self){

              as.character(unlist(dplyr::groups(self@data)))

            }
          )

    ,min_date=new_property(
      class=class_numeric
      ,default = NULL
      ,getter=\(self){

        self@data |> pull(!!self@date) |> min(na.rm=TRUE)

      }
    )
    ,max_date=new_property(
      class=class_numeric
      ,default=NULL
      ,getter=\(self){

        self@data |> pull(!!self@date) |> max(na.rm=TRUE)

      }
    )
  )
  ,constructor = function(data,date,method){
    new_object(
      S7_object()
      ,data=data,date=rlang::enquo(date),method=method(type = method)
    )
  }
)


totalytd_tbl <- new_class(
  "totalytd_tbl"
  ,properties = list(
    value=new_property(
      class=class_any
    )
    ,value_vec=new_property(
      class=class_character
      ,getter = \(self){
      as.character(self@value)
      }
    )
    ,aggregation=new_property(
      class=class_logical
      ,default=TRUE
      # ,setter=\(self,value){
      #
      #   value <- TRUE
      #   self@aggregation <- value
      #   self
      #
      # }
    )
    ,shift=new_property(
      class=class_logical
      # ,setter=\(self,value){
      #   value <- TRUE
      #
      #   self@shift <- value
      #   slef
      #
      # }
    )
    ,compare=new_property(
      class=class_logical
      # ,setter=\(self,value){
      #
      #   value <- TRUE
      #
      #   self@compare <- value
      #   self
      #
      # }
    )
    ,fun=new_property(
      class=class_function
      # ,setter=\(self,value){
      #
      #   value <- \(x) cumsum(x)
      #
      #   self@fun <- value
      #   self
      #
      # }
    )
    ,new_col=new_property(
      class=class_character
      # ,setter=\(self,value){
      #
      #   value <- "ytd"
      #   self@new_col <- value
      #
      # }
    )
  )
  ,constructor = function(value,aggregation){
    new_object(
      S7_object()
      ,value=rlang::enquo(value)
      ,aggregation=aggregation
    )
  }
)


,constructor = function(value,aggregation,new_col,fun,compare,shift){
  new_object(

    ,calendar=calendar_tbl(data=calendar@data,method=calendar@method@type,date = !!calendar@date )
    ,value=rlang::enquo(value)
    ,aggregation=aggegration
    ,new_col="ytd"
    ,fun=\(x) cumsum(x)
    ,compare=FALSE
    ,shift=FALSE
  )
}

totalytd_tbl(vs)

mtcars |>
  group_by(vs) |>
  calendar_tbl(date = am,method = "standard") |>
  totalytd_tbl(value="vs")


dtest@calendar@data |>
  group_by(pick(all_of(test@calendar@group_vec))) |>
  mutate(
    !!test@new_col:=test@new_col
  )


full_tbl |>
  dplyr::group_by(year,...) |>
  dplyr::arrange(date,.by_group = TRUE) |>
  dplyr::mutate(
    ytd=base::cumsum({{value_var}})
  ) |>
  dplyr::ungroup()













assertthat::assert_that(base::is.data.frame(.data), msg = "Data must be a data frame.")
assertthat::assert_that(base::is.character(time_unit), msg = "Time unit must be a character string.")
assertthat::assert_that(time_unit %in% base::c("day", "week","quarter","semester","month", "year"), msg = "Time frame must be one of 'day', 'week','semester', 'month', or 'year'.")
assertthat::assert_that(lubridate::is.Date(.data |> pull({{date_var}})), msg = "The date column is not in Date format.")


ti <- new_generic("ti","data",function(data,date,value,time_unit){

  S7_dispatch()

})


method(ti,class_data.frame) <- function(data,date,value,time_unit){


  ti_tbl <- time_intelligence_tbl(data=data,date={{date}},value={{value}},time_unit=time_unit)


  # Floor the date to the specified time frame
#
  summary_tbl <- ti_tbl@data@data |>

    dplyr::mutate(
      date = lubridate::floor_date(!!ti_tbl@date,unit= ti_tbl@time_unit@time_unit)
      ,time_unit=ti_tbl@time_unit@time_unit
    ) |>
    dplyr::group_by(date,dplyr::pick(ti_tbl@data@group_vec)) |>
    dplyr::summarise(
      "{rlang::as_label(ti_tbl@value)}":=sum(!!ti_tbl@value,na.rm=TRUE)
      ,.groups = "drop"
    )


  # Create a calendar table with all the dates in the specified time frame

  calendar_tbl <- tibble::tibble(
    date = base::seq.Date(from = base::min(summary_tbl$date,na.rm=TRUE), to = base::max(summary_tbl$date,na.rm = TRUE), by = ti_tbl@time_unit@time_unit)
  )

  # create crossing table of groups
  if(!purrr::is_empty(ti_tbl@data@group_quo)){

    calendar_tbl <- dplyr::left_join(
      summary_tbl |> dplyr::distinct(pick(ti_tbl@data@group_vec)) |> dplyr::mutate(id="id")
      ,calendar_tbl |> dplyr::mutate(id="id")
      ,by=dplyr::join_by(id)
      ,relationship = "many-to-many"
    ) |>
      dplyr::select(-id)

  }

  # Perform a full join to ensure all time frames are represented

full_tbl <- dplyr::full_join(
  calendar_tbl
  ,summary_tbl
  ,by = dplyr::join_by(date,!!!ti_tbl@data@group_quo)
) |>
  dplyr::mutate(
    "{rlang::as_label(ti_tbl@value)}":= dplyr::coalesce(!!ti_tbl@value, 0)
  )


  return(full_tbl)
}

sales |>
  group_by(store_key) |> # tibble
  mom(date=order_date,value=unit_price) |> # returns ti_tbl class with printed instructions, kinda like what recipe does
  calculuate() ## returns results in tibble
