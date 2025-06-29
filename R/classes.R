
# to register methods upon packag loading--------------
.onLoad <- function(...) {
  S7::methods_register()
}


# time unit class when aggregating a date column-----------

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

# action class to help with printing methods--------------

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
    ,method=S7::new_property(
      class=S7::class_any
      ,default = NA_character_
    )
  )
)


# calendar class to create a calendar compliant table---------------

calendar <- S7::new_class(
  name="calendar"
  ,package = "fpaR"
  ,properties =
    list(
      data=S7::new_property(
        class=S7::class_any
        ,setter = \(self,value){

          self@data <- make_db_tbl(value)
          self
        }
      )
      ,class_name=S7::new_property(
        class=S7::class_any
        ,getter = \(self){
          class <- class(self@data)
          x <- dplyr::first(if_else(str_detect(class,"tbl_dbi"),"dbi","tbl"))
          x
        }
        ,validator = \(value){
          if(!any(class(value) %in% c("tbl_dbi","data.frame"))) return(cli::format_error("Please pass a 'dbi' or 'data.frame' object"))
        }
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
        class=S7::class_any
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

# value tbl class

value <- S7::new_class(
  "value"
  ,properties = list(

    value_vec=S7::new_property(
      class=S7::class_character
      ,default = NA_character_
    )

    ,value_quo=S7::new_property(
      class=S7::class_any
      ,getter=\(self){
        x <- rlang::parse_expr(self@value_vec)
        x
      }
    )

    ,new_column_name_vec=S7::new_property(
      class=S7::class_character
      ,setter = \(self,value){
        self@new_column_name_vec <- base::paste0(value,"_",self@value_vec)
        self
      }
    )
    ,new_column_name_quo=S7::new_property(
      class=S7::class_any
      ,getter= \(self){
       x <- rlang::parse_expr(self@new_column_name_vec)
       x
      }
    )
  )
)


# function tbl class-----------------------


fn <- S7::new_class(
  "fn"
  ,properties = list(
    fn_exec=S7::class_function
    ,fn_name=S7::new_property(
      class=S7::class_character
      ,default=NA_character_
    )
    ,fn_long_name=S7::new_property(
      class=S7::class_character
      ,default = NA_character_
    )
    ,shift=S7::new_property(
      class=S7::class_character
      ,default = NA_character_
    )
    ,compare=S7::new_property(
      class=S7::class_character
      ,default = NA_character_
    )
    ,new_date_column_name=S7::new_property(
      class=S7::class_any
      ,default = NA_character_
    )
    ,lag_n=S7::new_property(
      class=S7::class_numeric
      ,default = 0
    )
  )
)

# ti class to bring everything together ----------------

ti <- S7::new_class(
  name="ti"

  ,package = "fpaR"

  #properties
  ,properties = list(

  #see calendar class
    calendar=calendar
  # see time unit class
    ,time_unit=time_unit

  # properties on the target variable
    ,value=value

  # properties of function
    ,fn=fn
  # properties to help with printing method
    ,action=action

  )
  # validator to check if date column is in date format
    ,validator = \(self){

    if(!any(self@calendar@data |>  dplyr::pull(self@calendar@date_vec) |> class() %in% c("Date"))){

      return(cli::format_error("'{self@date_vec}' is not in Date format"))
    }
  }
)


## abc class---------



segment <- S7::new_class(

  ,name="segment"
  ,package = "fpaR"
  ,properties = list(
    data=S7::new_property(
      class=S7::class_any
      ,setter = \(self,value){

        self@data <- make_db_tbl(value)
        self
      }
    )
    ,value_vec=S7::new_property(
      class=S7::class_character
    )
    ,value_quo=S7::new_property(
      class=S7::class_any
      ,getter=\(self){
        x <- rlang::parse_expr(self@value_vec)
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
    ,category_values=S7::new_property(
      class=S7:::class_numeric
      ,default=c(.7,.96,1)
      ,validator = \(value){

        if(assertthat::assert_that(all(value<=1),msg = cli::format_error("Please ensure the category_values are less than or equal to 1"))){}

      }
    )
    ,category_names=S7::new_property(
      class=S7::class_any
      ,setter = \(self,value){
        self@category_names <- letters[1:length(self@category_values)]
        self
      }
    )
    ,fn=fn
    ,action=action
  )
)
