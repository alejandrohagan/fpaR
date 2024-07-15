library(tidyverse)


test <- read_csv("https://huggingface.co/datasets/AbhayBhan/SalesData/raw/main/s1.csv") |>
  janitor::clean_names() |>
  mutate(
    date=mdy(date)
  )

## time intelligence


ytd
ptd
mtd
qtd
pmtd
pqtd
yoy
mom
wow
pop

##after summary need to join to expand and have full calendar table

## ytd

summary_tbl |>
  mutate(
    year=year(date)
    ,quarter=quarter(date)
    ,month=month(date)
    ,week=week(date)
  ) |>
  group_by(year,city)
  mutate(
    ytd=cumsum(n)
  )

  ## mtd

  summary_tbl |>
    mutate(
      year=year(date)
      ,quarter=quarter(date)
      ,month=month(date)
      ,week=week(date)
    ) |>
    group_by(year,month,city) |>
  mutate(
    mtd=cumsum(n)
  )

  ## wtd

  summary_tbl |>
    mutate(
      year=year(date)
      ,quarter=quarter(date)
      ,month=month(date)
      ,week=week(date)
    ) |>
    group_by(year,month,week,city) |>
    mutate(
      wtd=cumsum(n)
    )

  ## wtd

  summary_tbl |>
    mutate(
      year=year(date)
      ,quarter=quarter(date)
      ,month=month(date)
      ,week=week(date)
    ) |>
    group_by(year,month,week,city) |>
    mutate(
      wtd=cumsum(n)
    )



summary_tbl <- test |>
  group_by(
    city
    ,date
    ) |>
  summarise(
    n=n()
    ,.groups="drop_last"
  ) |>
  arrange(city,date)

calendar_tbl <- tibble(
  calendar_date=seq.Date(from=min(summary_tbl$date),to=max(summary_tbl$date),by="day")
)

date_vec <- ymd("2021-01-01")


date_vec+months(5)

calendar_tbl |>
  # rowwise() |>
  mutate(
    comparison_period=calendar_date%m+%months(1)
      ) |>
  left_join(
    summary_tbl
    ,by=join_by(calendar_date==date)
  ) |>
  left_join(
    summary_tbl
    ,by=join_by(comparison_period==date,city)
  ) |>
  mutate(
   delta= n.x-n.y
  ) |>
  drop_na()






devtools::document()
devtools::load_all()
devtools::test()
# load table
rm(list = c("make_segmentation"))
drv <- duckdb::duckdb(dbdir="/home/hagan/database.duckdb")

con <- DBI::dbConnect(drv)
DBI::dbListTables(con)

diamonds_db <- tbl(con,"mtcars_dbi")

mtcars_kmeans <- mtcars |>
  tibble::rownames_to_column(var="column1")

make_segmentation(
  .data =mtcars_kmeans ,
id_col=column1,
kmeans_nstart = 10,
kmeans_centers_init = 5,
kmeans_iter.max = 100,max.overlaps = 100,
centers_grid_range = 1:15)

devtools::test()
# sql practice

y  <- "price"
x1 <- "x"
x2 <- "carat"
.data <- "diamonds.db"

var_1 <- list(c(y="price",x1="x",x2="carat",.data="diamonds"))
library(glue)
sql <-"
WITH means AS (
  SELECT
  AVG({`y`}) AS mean_y,
  AVG({`x1`}) AS mean_x1,
  AVG({`x2`}) AS mean_x2
  FROM {`.data`}

)"

glue_sql("
WITH means AS (
  SELECT
  AVG({`y`}) AS mean_y,
  AVG({`x1`}) AS mean_x1,
  AVG({`x2`}) AS mean_x2
  FROM {`.data`})"
,y="price"
,x1="x"
,x2="carat"
,.data="diamonds"
,.con=con)

map2(
  .x=sql
  ,.y=var_1
  ,\(x,y) glue::glue_sql(x,y,.con=con)
)

tibble(
  x=
)


rm(list=ls())

DBI::dbListTables(con)
## lm

  y <- iris$Sepal.Length - mean(iris$Sepal.Length)
x1 <- iris$Petal.Length - mean(iris$Petal.Length)
x2 <- iris$Sepal.Width - mean(iris$Sepal.Width)


resx2x1 <- resid(lm(x2 ~0+ x1))
resyx1 <- resid(lm(y ~0+ x1))
coef_x2 <- coef(lm(resyx1 ~0+ resx2x1))[["resx2x1"]]

resx1x2 <- resid(lm(x1 ~0+ x2))
resyx2 <- resid(lm(y ~0+ x2))
coef_x1 <- coef(lm(resyx2 ~ 0+resx1x2))[["resx1x2"]]

all.equal(c(coef_x1, coef_x2),
          coef(lm(y ~0+ x1 + x2))[c("x1", "x2")],
          check.attributes = FALSE)


y  <- "price"
x1 <- "x"
x2 <- "carat"
.data <- "diamonds.db"

test_lm <- glue::glue_sql("
-- Step 1: Create a table to hold the iris dataset

-- Step 2: Calculate the mean of each column
WITH means AS (
    SELECT
        AVG({`y`}) AS mean_y,
        AVG({`x1`}) AS mean_x1,
        AVG({`x2`}) AS mean_x2
    FROM {`.data`}

)

-- Step 3: Compute the centered values for Sepal.Length, Petal.Length, and Sepal.Width
, centered AS (
    SELECT
        {`y`} - means.mean_y AS y,
        {`x1`} - means.mean_x1 AS x1,
        {`x2`} - means.mean_x2 AS x2
    FROM
        {`.data`}, means
)

-- Step 4: Compute the residuals of the linear models using REGR_SLOPE
, res_x2_x1 AS (
    SELECT
        x2 - (x1 * REGR_SLOPE(x2, x1) OVER()) AS resx2x1
,ROW_NUMBER() OVER () AS row_num
    FROM centered
)
, res_y_x1 AS (
    SELECT
        y - (x1 * REGR_SLOPE(y, x1) OVER()) AS resyx1
,ROW_NUMBER() OVER () AS row_num
    FROM centered
)
, res_x1_x2 AS (
    SELECT
        x1 - (x2 * REGR_SLOPE(x1, x2) OVER()) AS resx1x2
,ROW_NUMBER() OVER () AS row_num
    FROM centered
)
, res_y_x2 AS (
    SELECT
        y - (x2 * REGR_SLOPE(y, x2) OVER()) AS resyx2
,ROW_NUMBER() OVER () AS row_num
    FROM centered
)

-- Step 5: Compute the coefficients from the residuals
SELECT
    REGR_SLOPE(resyx1, resx2x1) AS coef_x2,
    REGR_SLOPE(resyx2, resx1x2) AS coef_x1
FROM
    res_y_x1, res_x2_x1, res_y_x2, res_x1_x2;
",
.con=con)



test_lm <- glue::glue_sql("


-- Step 2: Calculate the mean of each column
WITH means AS (
    SELECT
        AVG({`y`}) AS mean_y,
        AVG({`x1`}) AS mean_x1,
        AVG({`x2`}) AS mean_x2
    FROM {`.data`}

)

-- Step 3: Compute the centered values for Sepal.Length, Petal.Length, and Sepal.Width
, centered AS (
    SELECT
        {`y`} - means.mean_y AS y,
        {`x1`} - means.mean_x1 AS x1,
        {`x2`} - means.mean_x2 AS x2
    FROM
        {`.data`}, means
)
, res_x2_x1 AS (
    SELECT
        x2 - (x1 * REGR_SLOPE(x2, x1) OVER()) AS resx2x1
,ROW_NUMBER() OVER () AS row_num
    FROM centered
)
, res_y_x1 AS (
    SELECT
        y - (x1 * REGR_SLOPE(y, x1) OVER()) AS resyx1
,ROW_NUMBER() OVER () AS row_num
    FROM centered
)
, res_x1_x2 AS (
    SELECT
        x1 - (x2 * REGR_SLOPE(x1, x2) OVER()) AS resx1x2
,ROW_NUMBER() OVER () AS row_num
    FROM centered
)
, res_y_x2 AS (
    SELECT
        y - (x2 * REGR_SLOPE(y, x2) OVER()) AS resyx2
,ROW_NUMBER() OVER () AS row_num
    FROM centered
),

full_tbl as (

SELECT
    res_x2_x1.row_num
    ,res_x2_x1.resx2x1
    ,res_y_x1.resyx1
    ,res_x1_x2.resx1x2
    ,res_y_x2.resyx2

FROM
    res_x2_x1

JOIN
    res_y_x1 ON res_x2_x1.row_num = res_y_x1.row_num
JOIN
    res_x1_x2 ON res_x2_x1.row_num = res_x1_x2.row_num
JOIN
    res_y_x2 ON res_x2_x1.row_num = res_y_x2.row_num
)

select
    REGR_SLOPE(resyx1, resx2x1) AS coef_x2
    ,REGR_SLOPE(resyx2, resx1x2) AS coef_x1

from full_tbl

",
.con=con)

DBI::dbGetQuery(con,test_lm) |> as_tibble()

lm(price~0+x+carat,data=diamonds)

## practice sql functions
dbplyr::remote_con(contoso_fact_sales_db)
dbplyr::remote_table(contoso_fact_sales_db)

dbplyr::remote_query(contoso_fact_sales_db)

## practice fpaR unctions

out <- create_date_db(contoso_fact_sales_db,start_date = "2021-01-01",end_date = "2023-03-01")


DBI::dbGetQuery(conn = con,statement = out)


contoso_fact_sales_db |>
  group_by(SalesKey) |>
  mutate(
   id=(SalesKey)
  )


contoso_fact_sales_db |> pluck("src") |> pluck("con")


usethis::use_package(c("cli"))

contoso_fact_sales_db |>
  mutate(
    test=SalesKey/2
  ) |>
  relocate(test) |> show_query()
  make_cohort_tbl(id_var="StoreKey",date = "DateKey",time_unit = "month",period_label = FALSE)


vec <- c("one.md","two.md","three.md")

dir.create("test")
fpaR::clean_file_names("teset")
online_cohorts %>%
  cohort_table_month(CustomerID, InvoiceDate)


time_unit <- "month"
.data <- "contoso_fact_sales"
id_var <- "StoreKey"
date <- "DateKey"


test_sql <- glue::glue_sql(

"WITH date_var_calculated AS (
    SELECT
        {id_var}
        ,DATE_TRUNC({time_unit}, {date}) AS date_var
        ,MIN(DATE_TRUNC({time_unit}, {date})) OVER (PARTITION BY {id_var}) AS cohort

    FROM  {.data}

),

summary_calculated AS (
    SELECT
        cohort
        ,date_var
        ,COUNT(DISTINCT {id_var}) AS users

    FROM
        date_var_calculated

    GROUP BY
        cohort
        ,date_var)

SELECT
    *,
    DENSE_RANK() OVER (ORDER BY cohort, date_var) AS group_id
FROM
    summary_calculated;"
,.con=con
)





DBI::dbGetInfo(contoso_fact_sales_db)
DBI::dbGetQuery(con,test)

DBI::dbGetInfo(con)
 |> summary()




postgres_agg <- sql_translator(.parent = base_agg,
                               cor = sql_aggregate_2("CORR"),
                               cov = sql_aggregate_2("COVAR_SAMP"),
                               sd =  sql_aggregate("STDDEV_SAMP", "sd"),
                               var = sql_aggregate("VAR_SAMP", "var")
)

# Next we have to simulate a connection that uses this variant
con <- simulate_dbi("TestCon")
sql_translation.TestCon <- function(x) {
  sql_variant(
    base_scalar,
    postgres_agg,
    base_no_win
  )
}

translate_sql(cor(x, y), con = con, window = FALSE)
translate_sql(sd(income / years), con = con, window = FALSE)

# Any functions not explicitly listed in the converter will be translated
# to sql as is, so you don't need to convert all functions.
translate_sql(??regr_intercept(y, x), con = con)


lm(price~0+carat+cut,data=diamonds) |>
  summary()

usethis::use_r("create_date_tbl")
test <- sql("


 WITH generate_date AS (
        SELECT CAST(RANGE AS DATE) AS date_key
          FROM RANGE(DATE '2009-01-01', DATE '2013-12-31', INTERVAL 1 DAY)
          )
   SELECT date_key AS date_key,
          DAYOFYEAR(date_key) AS day_of_year,
          YEARWEEK(date_key) AS week_key,
          WEEKOFYEAR(date_key) AS week_of_year,
          DAYOFWEEK(date_key) AS day_of_week,
          ISODOW(date_key) AS iso_day_of_week,
          DAYNAME(date_key) AS day_name,
          DATE_TRUNC('week', date_key) AS first_day_of_week,
          DATE_TRUNC('week', date_key) + 6 AS last_day_of_week,
          YEAR(date_key) || RIGHT('0' || MONTH(date_key), 2) AS month_key,
          MONTH(date_key) AS month_of_year,
          DAYOFMONTH(date_key) AS day_of_month,
          LEFT(MONTHNAME(date_key), 3) AS month_name_short,
          MONTHNAME(date_key) AS month_name,
          DATE_TRUNC('month', date_key) AS first_day_of_month,
          LAST_DAY(date_key) AS last_day_of_month,
          CAST(YEAR(date_key) || QUARTER(date_key) AS INT) AS quarter_key,
          QUARTER(date_key) AS quarter_of_year,
          CAST(date_key - DATE_TRUNC('Quarter', date_key) + 1 AS INT) AS day_of_quarter,
          ('Q' || QUARTER(date_key)) AS quarter_desc_short,
          ('Quarter ' || QUARTER(date_key)) AS quarter_desc,
          DATE_TRUNC('quarter', date_key) AS first_day_of_quarter,
          LAST_DAY(DATE_TRUNC('quarter', date_key) + INTERVAL 2 MONTH) as last_day_of_quarter,
          CAST(YEAR(date_key) AS INT) AS year_key,
          DATE_TRUNC('Year', date_key) AS first_day_of_year,
          DATE_TRUNC('Year', date_key) - 1 + INTERVAL 1 YEAR AS last_day_of_year,
          ROW_NUMBER() OVER (PARTITION BY YEAR(date_key), MONTH(date_key), DAYOFWEEK(date_key) ORDER BY date_key) AS ordinal_weekday_of_month
     FROM generate_date

    ")

out
DBI::dbGetQuery(conn = con,test)

sql1 <- sql("SELECT * FROM table1")
sql2 <- sql("SELECT * FROM table2 WHERE column1 > 100")
sql3 <- sql("SELECT column2, column3 FROM table3")


combined_sql <- sql(paste(sql1, sql2, sql3, sep = "; "))

mtcars %>% glue::glue_data("{rownames(.)} has {hp} hp")


x <- mtcars$hp
inherits(x = ymd("2021-13-30"),what = "Date",)
y <- rownames(mtcars)

glue::glue("{y} has {x} hp")


generate_445_calendar <- function(start_date, year) {
  # Convert start_date to Date format if it's not already
  if (!inherits(start_date, "Date")) {
    start_date <- as.Date(start_date)
  }

  # Initialize vectors to hold month and week information
  months <- c("January", "February", "March", "April", "May", "June",
              "July", "August", "September", "October", "November", "December")
  weeks <- c(4, 4, 5, 4, 4, 5, 4, 4, 5, 4, 4, 5)  # 4-4-5 pattern

  # Create a data frame to store the calendar
  calendar <- data.frame(
    Month = character(length(months)),
    Week = integer(length(months)),
    Days = integer(length(months)),
    Start_Date = as.Date(character(length(months)))
  )

  # Initialize the start date
  current_date <- start_date

  # Fill in the calendar data frame
  for (i in seq_along(months)) {
    calendar$Month[i] <- months[i]
    calendar$Week[i] <- weeks[i]
    calendar$Days[i] <- weeks[i] * 7  # Calculate total days based on weeks
    calendar$Start_Date[i] <- current_date
    current_date <- current_date + calendar$Days[i]  # Move to the next month's start date
  }

  return(calendar)
}
start_date <- "2024-01-01"
year <- 2024
calendar_2024 <- generate_445_calendar(start_date, year)
print(calendar_2024)


generate_445_calendar <- function(start_date, year) {
  # Convert start_date to Date format if it's not already
  if (!inherits(start_date, "Date")) {
    start_date <- as.Date(start_date)
  }

  # Initialize vectors to hold month and week information
  months <- c("January", "February", "March", "April", "May", "June",
              "July", "August", "September", "October", "November", "December")
  weeks <- c(4, 4, 5, 4, 4, 5, 4, 4, 5, 4, 4, 5)  # 4-4-5 pattern

  # Create a data frame to store the calendar
  calendar <- data.frame(
    Date = as.Date(character(0)),
    Month = character(0),
    Week = integer(0),
    Start_Date = as.Date(character(0))
  )

  # Initialize the start date
  current_date <- start_date

  # Fill in the calendar data frame
  for (i in seq_along(months)) {
    month_days <- weeks[i] * 7  # Calculate total days based on weeks

    # Generate dates for the month
    month_dates <- seq(current_date, by = "day", length.out = month_days)

    # Append to the calendar data frame
    calendar <- rbind(calendar, data.frame(
      Date = month_dates,
      Month = rep(months[i], month_days),
      Week = rep(1:weeks[i], each = 7)[1:month_days],  # Assign week numbers
      Start_Date = rep(current_date, month_days)
    ))

    current_date <- current_date + month_days  # Move to the next month's start date
  }

  return(calendar)
}

"-- Step 1: Create a table to hold the iris dataset
-- Assuming the dataset is already available in a table called `iris_data`

-- Step 2: Calculate the mean of each column
WITH means AS (
    SELECT
        AVG(y) AS mean_y,
        AVG(x1) AS mean_x1,
        AVG(x2) AS mean_x2
    FROM iris_data
),

-- Step 3: Compute the centered values for y, x1, and x2
centered AS (
    SELECT
        y - means.mean_y AS y_centered,
        x1 - means.mean_x1 AS x1_centered,
        x2 - means.mean_x2 AS x2_centered
    FROM
        iris_data, means
),

-- Step 4: Compute the residuals of the linear models using REGR_SLOPE
residuals AS (
    SELECT
        x2_centered - (x1_centered * REGR_SLOPE(x2_centered, x1_centered) OVER()) AS res_x2_x1,
        y_centered - (x1_centered * REGR_SLOPE(y_centered, x1_centered) OVER()) AS res_y_x1,
        x1_centered - (x2_centered * REGR_SLOPE(x1_centered, x2_centered) OVER()) AS res_x1_x2,
        y_centered - (x2_centered * REGR_SLOPE(y_centered, x2_centered) OVER()) AS res_y_x2,
        ROW_NUMBER() OVER () AS row_num
    FROM centered
)

-- Step 5: Compute the coefficients from the residuals
SELECT
    REGR_SLOPE(res_y_x1, res_x2_x1) AS coef_x2,
    REGR_SLOPE(res_y_x2, res_x1_x2) AS coef_x1
FROM residuals;
"
library(DBI)

dbExecute(con, ".help;") |> as_tibble() |> print(n=100)

dbGetQuery(con
                      ,
           "
           SELECT generate_series(2, 5);
           "
                      )
  as_tibble() |>
  print(n=100)

DBI::dbListTables(con)

ext_tbl |> pull(install_path)



x <- mtcars


accumulate(.x=1:10,.f=\(prev,.next) mtcars |> mutate(test=prev+1) |> as_tibble() |> relocate(last_col()),.init=1)

mf <- memdb_frame(g = c(1, 1, 2, 2, 2), x = 1:5, y = 5:1)
mf

mf <-  ?memdb_frame(g = diamonds$price,c=diamonds$carat)

library(dbplyr)

mf |>
  summarise(
    test=dplyr::sql("regr_intercept(g,c)")
  )


lazy_frame(x = diamonds$price,y=diamonds$cut, con = simulate_snowflake()) |>
  summarise(x=mean(x))

translate_sql(?str_pad(x,"left",10), con = simulate_oracle())
translate_sql(substr(x, 1, 2), con = simulate_sqlite())


xs = range(1,11)
xs == [1,2,3,4,5,6,7,8,9,10]

library(duckplyr)
mtcars |>
  duckplyr::as_duckplyr_df() |>
  duckplyr::add_tally(vs) |> explain()



library(tidyverse)


dates_vec <- seq.Date(from=ymd("2023-01-01"),to=ymd("2023-12=31"),by = "day")




calendar_tbl <- tibble(
  dates=dates_vec
) |>
  mutate(
    quarter=lubridate::quarter(dates)
    ,quarter_start_date=floor_date(dates,unit="quarter")
    ,quarter_end_date=ceiling_date(dates,unit="quarter")
  )

sales_tbl <- tibble(
  row_id=1:100
) |>
  rowwise() |>
  mutate(
    posting_date=sample(dates_vec,1,replace = TRUE)
    ,days_clear=sample(c(1:30),1,replace=TRUE)
) |>
  ungroup() |>
  mutate(
    clearing_date=posting_date+days_clear
  )

decrease <- calendar_tbl |>
  left_join(
    sales_tbl
    ,by=
      join_by(
        quarter_start_date>posting_date
        ,quarter_end_date >clearing_date
      )
  ) |>
  drop_na() |>
  group_by(
    quarter
  ) |>
  summarise(
    decrease=n()
  )


### ABC cateogry

var_vec=="sum"

if(var_vec=="sum"){
 temp1 <- diamonds_db |>  #passes the dataframe through

  dplyr::group_by(cut,clarity) |>  #group's the dataframe by the input group column

  #creates a bunch of columns
  dplyr::summarize(
    var=sum(price)
    ,.groups="drop"
  ) |>
    dbplyr::window_order(desc(var),.by_group = FALSE)

} else{

  temp1 <- diamonds |>  #passes the dataframe through

    dplyr::group_by(cut,clarity) |>  #group's the dataframe by the input group column

    #creates a bunch of columns
    dplyr::summarize(
      var=dplyr::n()
      ,.groups="drop"
    ) |>
    arrange(desc(var),.by_group = FALSE)



}



a <- .7
b <- .25
c <- .05

order_by <- "sum"

order_by_value <- base::match.arg(arg = order_by,choices = c("sum","n"))

diamonds_db |>

  dplyr::group_by(cut,clarity) |>
  dplyr::summarize(
    var=sum(price)
    ,.groups="drop"
  ) |>
  arrange(var) |>
  dplyr::mutate(
    cum_sum=base::cumsum(var)
    ,prop_total=var/max(cum_sum)
    ,cum_prop_total=base::cumsum(prop_total)
    ,row_id=dplyr::row_number()
    ,max_row_id=max(row_id)
    ,id_prop_total=row_id/max_row_id
    ,group_classification_by_dim=
      dplyr::case_when(
        cum_prop_total  <=a      ~  "A"
        ,cum_prop_total <=(a+b)  ~  "B"
        ,.default                = "C"
        ),
    dim_threshold=
      dplyr::case_when(
        group_classification_by_dim=="A"~a
        ,group_classification_by_dim=="B"~(a+b)
        ,.default                        = c
        )
    ) |>
  dplyr::select(-c(prop_total,cum_sum,max_row_id,))

usethis::use_github_action()

quare <- function(n) {
  k <- 1
  while(TRUE) {
    if (k == n*n) {
      print(k)
      return(TRUE)
    } else {
      k <- k + 1
    }
  }
}
square(-20)


square <- function(n) {
  k <- 1
  while (k <= n * n) {
    if (k == n * n) {
      return(k)
    }
    k <- k + 1
  }
}



