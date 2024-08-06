#
# totalytd <- function(.data,dim,date,fn,time_unit){
#
#
#
#
#   if(missing(start_date)&missing(end_date)){
#
#     .data <- .data
#
#     cli::cli_h1("Info:")
#
#     cli::cli_alert_info("No filtering of data")
#
#   } else if(missing(start_date)){
#
#
#   end_date_name <-as.character(end_date)
#
#   .data <-   filter(.data,{{date}}<{{end_date}})
#
#   cli::cli_h1("Info:")
#   cli::cli_alert_info("Filtering date to be less than {end_date_name}")
#
#   } else if(missing(end_date)) {
#
#   start_date_name <-as.character(start_date)
#
#
#   .data <-   filter(.data,{{date}}>{{start_date}})
#
#   cli::cli_h1("Info:")
#   cli::cli_alert_info("Filtering date to be greater than {start_date_name}")
#
#   } else {
#
#     start_date_name <-as.character(start_date)
#     end_date_name <-as.character(end_date)
#
#     cli::cli_h1("Info:")
#     cli::cli_alert_info("Filtering date to be greater than {start_date_name} and less than {end_date_name}")
#     .data <- filter(.data,{{date}}<{{end_date}},{{date}}>{{start_date}})
#
#   }
# ?timetk::summarise_by_time()
#   lubridate::week
# .data %>%
#     group_by({{date}}) %>%
#     summarise(sum=sum({{dim}})) %>%
#     group_by(year=year(date)) %>%
#     mutate(totalytd_=cumsum(sum)) %>%
#     ungroup() %>%
#     select(-c(year,sum))
#
#   cli::cli_h1("Info:")
#   cli::cli_alert_info("Returned  rows by ")
# }















#
# # Example data
# test_data <- data.frame(
#   date = as.Date(c("2023-01-01", "2023-01-08", "2023-01-15")),
#   gross_income = c(100, 150, 200)
# )
#
# # Perform tests
# test_that("Function works correctly for weekly aggregation", {
#   result <- mypackage$calculate_period_changes(test_data, date, gross_income, "week")
#
#   expected_dates <- as.Date(c("2023-01-01", "2023-01-08", "2023-01-15"))
#   expected_incs <- c(100, 150, 200)
#   expected_changes <- c(NA, 50, 50)
#
#   assert_that(all(result$floor_date == expected_dates))
#   assert_that(all(result$inc == expected_incs))
#   assert_that(all(result$poop_change == expected_changes))
# })
#
# test_that("Function handles missing dates correctly", {
#   incomplete_data <- data.frame(
#     date = as.Date(c("2023-01-01", "2023-01-15")),
#     gross_income = c(100, 200)
#   )
#   result <- mypackage$calculate_period_changes(incomplete_data, date, gross_income, "week")
#
#   expected_dates <- as.Date(c("2023-01-01", "2023-01-08", "2023-01-15"))
#   expected_incs <- c(100, 0, 200)
#   expected_changes <- c(NA, -100, 200)
#
#   assert_that(all(result$floor_date == expected_dates))
#   assert_that(all(result$inc == expected_incs))
#   assert_that(all(result$poop_change == expected_changes))
# })
#
# # Print results for manual inspection
# print(mypackage$calculate_period_changes(test_data, date, gross_income, "week"))
#
#
# library(glue)
# library(DBI)
# library(assertthat)
#
# # Function to calculate period changes using SQL
# calculate_period_changes_sql <- function(conn, table_name, date_col, value_col, time_frame) {
#   assert_that(is.character(table_name), msg = "Table name must be a character string.")
#   assert_that(is.character(date_col), msg = "Date column must be a character string.")
#   assert_that(is.character(value_col), msg = "Value column must be a character string.")
#   assert_that(is.character(time_frame), msg = "Time frame must be a character string.")
#   assert_that(time_frame %in% c("day", "week", "month", "year"), msg = "Time frame must be one of 'day', 'week', 'month', or 'year'.")
#
#   # Define the appropriate SQL date truncation function
#   time_frame_sql <- switch(
#     time_frame,
#     "day" = "DATE",
#     "week" = "WEEK",
#     "month" = "MONTH",
#     "year" = "YEAR"
#   )
#
#   # SQL query to summarize the data
#   summary_sql <- glue_sql("
#     SELECT
#       DATE_TRUNC({time_frame_sql}, {`date_col`}) AS floor_date,
#       SUM({`value_col`}) AS inc
#     FROM {`table_name`}
#     GROUP BY DATE_TRUNC({time_frame_sql}, {`date_col`})
#   ", .con = conn)
#
#   # SQL query to generate the full table with period-over-period changes
#   final_sql <- glue_sql("
#     WITH summary_tbl AS (
#       {summary_sql}
#     ),
#     calendar_tbl AS (
#       SELECT
#         GENERATE_SERIES(
#           MIN(floor_date),
#           MAX(floor_date),
#           INTERVAL '1 {time_frame}'
#         ) AS floor_date
#       FROM summary_tbl
#     ),
#     full_tbl AS (
#       SELECT
#         c.floor_date,
#         COALESCE(s.inc, 0) AS inc
#       FROM
#         calendar_tbl c
#         LEFT JOIN summary_tbl s ON c.floor_date = s.floor_date
#     )
#     SELECT
#       floor_date,
#       inc,
#       inc - LAG(inc, 1) OVER (ORDER BY floor_date) AS poop_change
#     FROM full_tbl
#   ", .con = conn)
#
#   # Execute the SQL query and fetch the results
#   result <- dbGetQuery(conn, final_sql)
#   return(result)
# }

# Example usage (assuming you have a DB connection 'conn' and a table 'test_table')
# conn <- dbConnect(RPostgres::Postgres(), dbname = "your_database")
# result <- calculate_period_changes_sql(conn, "test_table", "date", "gross_income", "week")
# print(result)
