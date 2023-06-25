
library(tidyverse)
show_in_excel <- function(.data){
.data=diamonds
# supporting arguments
rows<- base::dim(.data)[1]
cols <- base::dim(.data)[2]
obj_class <- base::class(.data)



# validation checks
  purrr::map(.x=obj_class, function(.x)
    assertthat::assert_that(
      dplyr::if_else(
        .x %in% c("tbl_df", "tbl","data.frame")
        ,TRUE
        ,FALSE)
      ,msg = "function only supports tibbles or data.frame objects"
      )
    )


  if(rows>1e6){
    cli::cli_alert_danger("Row exceeds excel limits -- will not print more than 1M rows")
  } else{
  cli::cli_alert("data contains {rows} rows and {cols} columns")
}
  temp <- base::paste0(base::tempfile(),".csv")

  utils::write.csv(.data,file = temp)

  fs::file_show(path=temp)

cli::cli_alert_success("success: temp file @{temp}")
}

read
diamonds %>% show_in_excel()
