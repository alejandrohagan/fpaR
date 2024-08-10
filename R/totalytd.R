
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
