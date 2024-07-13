
#' Converts dataframe to csv file and opens up in excel
#'
#' @param .data dataframe in tibble or dataframe format
#'
#' @return a csv file
#' @export
#'
#' @examples
#' ggplot2::diamonds |>  show_in_excel()
show_in_excel <- function(.data){

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
  cli::cli_alert("data contains {rows} row(s) and {cols} column(s)")
  }

# create temp file path to be used later
  temp <- base::paste0(base::tempfile(),".csv")

  #sve teh doc df as a csv file
utils::write.csv(.data,file = temp)

 #open up the csv file
  fs::file_show(path=temp)

  #alert that successful
cli::cli_alert_success("success: temp file @{temp}")
}

