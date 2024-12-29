

## S7 class

# time unit class

time_unit <- S7::new_class(
  ,name="time_unit"
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
  ,properties=list(
    value=S7::new_property(
      class=S7::class_character
      ,setter = \(self,value){
        value <- stringr::str_to_lower(value)
        self@value <- value
        self
      }
    )
  )
  ,validator = \(self){
    if(!any(self@value %in% c("shift","compare","aggregate"))) return('Action must return "shift","compare" or "aggregate"')
  }
)


## calendar class

ti_tbl <- S7::new_class(
  name="ti_tbl"
  ,properties = list(

    data=S7::new_property(
      class=class_data.frame
    )
    ,type=S7::new_property(
      class=S7::class_character
      ,validator = \(value){
        if(!any(value %in% c("standard","554"))) return(cli::format_error("Please return either 'standard' or '554'"))
      }
      ,setter=\(self,value){
        value <- stringr::str_to_lower(value)
        self@type <- value
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
      ,default = NULL
      ,getter=\(self){
        x <-  self@data |> pull(dplyr::any_of(self@date_vec)) |> min(na.rm=TRUE)
        x
      }
    )
    ,max_date=S7::new_property(
      class=S7::class_numeric
      ,default=NULL
      ,getter=\(self){
        x <-  self@data |> pull(dplyr::any_of(self@date_vec)) |> max(na.rm=TRUE)
        x
      }
    )
    ,time_unit=S7::new_property(class=time_unit)
    ,group_indicator=S7::new_property(
      class=S7::class_logical
      ,getter=\(self){
        x <- dplyr::if_else(!purrr::is_empty(groups(self@data)),TRUE,FALSE)
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
  ,value_vec=S7::new_property(class=class_character)
  ,value_quo=S7::new_property(
    class=S7::class_any
    ,getter=\(self){
      x <- rlang::parse_expr(self@value_vec)
      x
    }
  )
  ,new_column_name=S7::new_property(class=S7::class_character)
  ,sort_logic=S7::new_property(class=S7::class_logical)
  ,fn=S7::new_property(class=S7::class_function)
  #custom classes
  ,action=S7::new_property(class=action)
)
  ,validator = \(self){

    if(!any(self@data |>  dplyr::pull(self@date_vec) |> class() %in% c("Date"))){

      return(cli::format_error("'{self@date_vec}' is not in Date format"))
    }
  }
)






