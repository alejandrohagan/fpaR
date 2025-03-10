

# find sql logic for 554, 445, 13 month calendars
# put indicator for imbalanced periods
# put print method for imbalanced period for comparison functions
# Put description in print method of calendar description
# update readme examples


library(tinytable)
library(tidyverse)

x <- mtcars[1:5, 1:5]

tt(x,theme = "bootstrap")






"WITH date_with_quarters AS (
  SELECT
  date_column,
  EXTRACT(YEAR FROM date_column) AS year,
  EXTRACT(MONTH FROM date_column) AS month,
  EXTRACT(DAY FROM date_column) AS day,
  EXTRACT(WEEK FROM date_column) AS week,
  ROW_NUMBER() OVER (ORDER BY date_column) AS row_num
  FROM dates_table
  WHERE date_column >= '2025-01-05'  -- Use your flexible start date here
),
calendar_with_quarters AS (
  SELECT
  date_column,
  year,
  month,
  week,
  -- Determine the quarter by calculating which group of 3 months the date belongs to
  CASE
  WHEN month BETWEEN 1 AND 3 THEN 1
  WHEN month BETWEEN 4 AND 6 THEN 2
    WHEN month BETWEEN 7 AND 9 THEN 3
  WHEN month BETWEEN 10 AND 12 THEN 4
  END AS quarter,
  -- Assign months based on the 5-5-4 calendar logic
  CASE
  WHEN EXTRACT(WEEK FROM date_column) <= 5 THEN 1  -- First 5 weeks are month 1
  WHEN EXTRACT(WEEK FROM date_column) <= 10 THEN 2  -- Next 5 weeks are month 2
  ELSE 3  -- Remaining weeks are month 3
  END AS month_in_quarter
  FROM date_with_quarters
)
SELECT
date_column,
year,
quarter,
month_in_quarter,
CASE
WHEN month_in_quarter = 1 THEN 'Month 1 (5 weeks)'
WHEN month_in_quarter = 2 THEN 'Month 2 (5 weeks)'
ELSE 'Month 3 (4 weeks)'
END AS month_description
FROM calendar_with_quarters
ORDER BY date_column;
"


"
WITH date_data AS (
  -- Select sequential dates and assign a row number to them.
  SELECT
  date,
  ROW_NUMBER() OVER (ORDER BY date) AS row_num
  FROM
  dates_table
),
quarter_data AS (
  -- Assign each row_num to a quarter and month based on the 91-day quarters (13 weeks).
  SELECT
  date,
  row_num,
  -- Calculate the quarter
  (row_num - 1) / 91 + 1 AS quarter,
  -- Calculate the relative day within the quarter (1 to 91)
  ((row_num - 1) % 91) + 1 AS day_in_quarter
  FROM
  date_data
),
month_data AS (
  -- Assign months based on the 5-5-4 structure (5 weeks, 5 weeks, 4 weeks)
  SELECT
  date,
  quarter,
  day_in_quarter,
  -- Define the month based on day_in_quarter
  CASE
  -- 5 weeks = 35 days
  WHEN day_in_quarter BETWEEN 1 AND 35 THEN 1
  WHEN day_in_quarter BETWEEN 36 AND 70 THEN 2
  ELSE 3
  END AS month_in_quarter
  FROM
  quarter_data
)
-- Final result: Join all the data and display date, quarter, and month in quarter
SELECT
date,
quarter,
month_in_quarter
FROM
month_data
ORDER BY
date;
"



