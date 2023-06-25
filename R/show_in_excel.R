

show_in_excel <- function(.data){

  rows<- dim(.data)[1]
  cols <- dim(.data)[1]

  assertthat:class(.data) %in% c("tbl_df", "tbl","data.frame"),msg="data obj must be tibble or dataframe")

  if(rows>1e6){
    cli::cli_alert_danger("Row exceeds excel limits -- will not print more than 1M rows")
  }

  temp <- base::paste0(base::tempfile(),".csv")

  utils::write.csv(.data,file = temp)

  fs::file_show(path=temp)
}


diamonds %>% show_in_excel()
