

#' Title
#'
#' @param .data
#' @param dim
#' @param date
#' @param fn
#' @param date_aggregation
#' @param start_date
#' @param end_date
#'
#' @return
#' @export
#'
#' @examples
totalytd <- function(.data,dim,date,fn,time_unit){




  if(missing(start_date)&missing(end_date)){

    .data <- .data

    cli::cli_h1("Info:")

    cli::cli_alert_info("No filtering of data")

  } else if(missing(start_date)){


  end_date_name <-as.character(end_date)

  .data <-   filter(.data,{{date}}<{{end_date}})

  cli::cli_h1("Info:")
  cli::cli_alert_info("Filtering date to be less than {end_date_name}")

  } else if(missing(end_date)) {

  start_date_name <-as.character(start_date)


  .data <-   filter(.data,{{date}}>{{start_date}})

  cli::cli_h1("Info:")
  cli::cli_alert_info("Filtering date to be greater than {start_date_name}")

  } else {

    start_date_name <-as.character(start_date)
    end_date_name <-as.character(end_date)

    cli::cli_h1("Info:")
    cli::cli_alert_info("Filtering date to be greater than {start_date_name} and less than {end_date_name}")
    .data <- filter(.data,{{date}}<{{end_date}},{{date}}>{{start_date}})

  }
?timetk::summarise_by_time()
  lubridate::week
.data %>%
    group_by({{date}}) %>%
    summarise(sum=sum({{dim}})) %>%
    group_by(year=year(date)) %>%
    mutate(totalytd_=cumsum(sum)) %>%
    ungroup() %>%
    select(-c(year,sum))

  cli::cli_h1("Info:")
  cli::cli_alert_info("Returned  rows by ")
}




