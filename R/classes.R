
.onLoad <- function(...) {
  S7::methods_register()
}


time_unit <- S7::new_class(

  ,name="time_unit"
  ,package = "fpaR"
  ,properties = list(
    value=S7::new_property(
      class=S7::class_character
      ,default = "day"
      ,setter=\(self,value){
        value <- stringr::str_to_lower(value)
        self@value <- value
        self
      }
      ,validator = \(value){
        if(length(value)!=1) cli::format_error("Please enter only one time unit")
      }
    )
  )
  ,validator = \(self){
    valid_units <- c("day","week","month","quarter","year")
    if(!any(self@value %in% valid_units))  return(cli::format_error("Please only enter {valid_units}"))
  }
)

# action class

action <- S7::new_class(
  name="action"
  ,package = "fpaR"
  ,properties=list(
    value=S7::new_property(
      class=S7::class_any
      ,setter = \(self,value){
        value <- stringr::str_to_lower(value)
        self@value <- fpaR:::make_action_cli(value)
        self
      }
    )
  )
)

## calendar class

calendar_tbl <- S7::new_class(
  name="calendar_tbl"
  ,package = "fpaR"
  ,properties =
    list(
      data=S7::new_property(
        class=S7::class_data.frame
      )
      ,calendar_type=S7::new_property(
        class=S7::class_character
        ,validator = \(value){
          if(!any(value %in% c("standard","554"))) return(cli::format_error("Please return either 'standard' or '554'"))
        }
        ,setter=\(self,value){
          value <- stringr::str_to_lower(value)
          self@calendar_type <- value
          self
        }
      )
      ,date_vec=S7::new_property(
        class=class_any
      )
      ,date_quo=S7::new_property(
        class=S7::class_any
        ,getter=\(self){
          x <- rlang::parse_expr(self@date_vec)
          x
        }
      )
      ,min_date=S7::new_property(
        class=S7::class_numeric
        ,getter=\(self){
          x <-  self@data |>
            dplyr::pull(dplyr::any_of(self@date_vec)) |>
            min(na.rm=TRUE)
          x
        }
      )
      ,max_date=S7::new_property(
        class=S7::class_numeric
        ,getter=\(self){
          x <-  self@data |>
            dplyr::pull(dplyr::any_of(self@date_vec)) |>
            max(na.rm=TRUE)
          x
        }
      )
      ,date_range=S7::new_property(
        class=S7::class_numeric
        ,getter =\(self){
          x <- as.numeric(self@max_date-self@min_date)
          x
        }
      )
      ,date_count=S7::new_property(
        class=S7::class_numeric
        ,getter=\(self){
          x <- self@data |>
            dplyr::pull(self@date_quo) |>
            unique() |>
            length()
          x
        }
      )
      ,date_missing=S7::new_property(
        class=S7::class_numeric
        ,getter=\(self){
          x <- as.numeric(self@date_range-self@date_count)
          x
        }
      )
      ,group_indicator=S7::new_property(
        class=S7::class_logical
        ,getter=\(self){
          x <- dplyr::if_else(!purrr::is_empty(dplyr::groups(self@data)),TRUE,FALSE)
          x
        }
      )
      ,group_quo=S7::new_property(
        class=S7::class_any
        ,getter = \(self){
          x <- dplyr::groups(self@data)
          x
        }
      )
      ,group_vec=S7::new_property(
        class=S7::class_any
        ,getter = \(self){
          x <-  as.character(unlist(dplyr::groups(self@data)))
          x
        }
      )
      ,group_count=S7::new_property(
        class=S7::class_numeric
        ,getter=\(self){
          if(!self@group_indicator){

            x <- 0
            x

          }else{
            x <-   length(self@group_vec)
            x
          }
        }
      )
    )
)

### ti table

ti_tbl <- S7::new_class(
  name="ti_tbl"
  ,package = "fpaR"
  ,properties = list(
    calendar_tbl=calendar_tbl
    ,time_unit=time_unit
    ,value_vec=S7::new_property(class=class_character)
    ,value_quo=S7::new_property(
      class=S7::class_any
      ,getter=\(self){
        x <- rlang::parse_expr(self@value_vec)
        x
      }
    )
    ,new_column_name_prefix=S7::new_property(class=S7::class_character)
    ,new_column_name=S7::new_property(
      class=class_character
      ,getter = \(self){
        x <- paste0(self@new_column_name_prefix,"_",self@value_vec)
        x
      }
    )
    ,new_date_column_name=S7::new_property(class=S7::class_character)
    ,lag_n=new_property(
      class=class_numeric
      ,default = 0
    )
    ,sort_logic=S7::new_property(class=S7::class_logical)

    #custom classes
    ,action=S7::new_property(class=action)
    ,fn=class_function
  )
  ,validator = \(self){

    if(!any(self@calendar_tbl@data |>  dplyr::pull(self@calendar_tbl@date_vec) |> class() %in% c("Date"))){

      return(cli::format_error("'{self@date_vec}' is not in Date format"))
    }
  }
)


# ytd_tbl <- S7::new_class(
#   "ytd_tbl"
#   ,parent = ti_tbl
#   )

#
# qtd_tbl <- S7::new_class(
#   "qtd_tbl"
#   ,parent = ti_tbl
#   ,package = "fpaR"
# )
#
# mtd_tbl <- S7::new_class(
#   "mtd_tbl"
#   ,parent = ti_tbl
#   ,package = "fpaR"
# )
#
# wtd_tbl <- S7::new_class(
#   "totalwtd_tbl"
#   ,parent = ti_tbl
#   ,package = "fpaR"
# )
#
#
# atd_tbl <- S7::new_class(
#   "totalatd_tbl"
#   ,parent = ti_tbl
#   ,package = "fpaR"
# )
#
#
# dod_tbl <- S7::new_class(
#   "dod_tbl"
#   ,parent = ti_tbl
#   ,package = "fpaR"
# )
#
# wow_tbl <- S7::new_class(
#   "wow_tbl"
#   ,parent = ti_tbl
#   ,package = "fpaR"
# )
#
# mom_tbl <- S7::new_class(
#   "mom_tbl"
#   ,parent = ti_tbl
#   ,package = "fpaR"
# )
#
#
# yoy_tbl<- S7::new_class(
#   "yoy_tbl"
#   ,parent = ti_tbl
#   ,package = "fpaR"
# )
#
