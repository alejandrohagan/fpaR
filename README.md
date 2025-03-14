# README


# fpaR: A Business Intelligence Toolkit for Financial Planning & Analysis (FP&A)

## Introduction

`fpaR` is a collection of business intelligence tools designed to
simplify common **financial planning and analysis (FP&A)** tasks such as
time intelligence calculations, customer, vendor or product
segmentation, and factor/variance analysis.

The package is inspired by best practices from a collection of blogs,
books, industry research, and hands-on work experience, consolidating
frequently performed business analyses into a fast, efficient, and
reusable framework.

In particular, the time intelligence functions are heavily inspired by
[PowerBI DAX](https://www.sqlbi.com/) functions

Under the hood, these functions are built upon the great foundations
of: - [dbplyr](https://dbplyr.tidyverse.org/), -
[duckdb](https://github.com/duckdb/duckdb-r) -
[lubridate](https://lubridate.tidyverse.org/)

`fpaR` is designed to seamlessly work with either tibbles or modern
database (DuckDB, Snowflake, SQLite, etc) with a common syntax.

Even if you are working with tibbles, most functions are optimized to
leverage [DuckDB](https://github.com/duckdb/duckdb-r) for superior speed
and performance.[^1]

By default most `fpaR` function returns a lazy DBI object which you can
return as a tibble with `dplyr::collect()`

## Key features & benefits

- **Flexible support** common syntax for both local tibbles and
  databases
- **Optimized for speed and scale** tibbles rewritten to take advantage
  of [duckdb]() database
- **Every transformation step is explained and summarized** instant
  clarity to what the underlying functions transformations are doing so
  you can validate and understand your outputs

## Installation

Install the development version of `fpaR` from GitHub:

``` r
# Install using pak or install.package()

pak::pak("alejandrohagan/fpaR")
```

## What is in fpaR?

**Time intelligence**

`fpaR` provides readily available functions for most time intelligence
analysis such as **Year-over-Year (YoY)**, **Month-to-Date (MTD)**, and
**Current Year-to-Date over Previous Year-to-Date (YoYTD)**.

These functions are designed to quickly answer most common time
intelligence related analysis in a consistent, fast and transparent way.

**These features offer key benefits: **

- **Auto-fill missing dates**: Ensures no missing periods in your
  calculations to ensure there are no missing dates or periods

- **Flexible calendar options**: Handle comparisons based on a
  **standard** or **5-5-4** fiscal calendar to accommodate different
  reporting frameworks

- **Period imbalance indicator**: When comparing periods with dates
  imbalance, the time intelligence functions will alert you to the type
  and number of period imbalances to ensure you are aware of misleading
  likewise comparisons

Below is the full list of time intelligence functions:

| short_name | description                                    | shift | aggregate | compare |
|------------|------------------------------------------------|-------|-----------|---------|
| YoY        | Full Year over Year                            |       |           | X       |
| YTD        | Year-to-Date                                   |       | X         |         |
| PYTD       | Prior Year-to-Date amount                      | X     | X         |         |
| YoYTD      | Current Year-to-Date over Prior Year-to-Date   | X     | X         | X       |
| YTDOPY     | Year-to-Date over Full Previous Year           | X     | X         | X       |
| QoQ        | Full Quarter over Quarter                      |       |           | X       |
| QTD        | Quarter-to-Date                                |       | X         |         |
| PQTD       | Prior Quarter-to-Date                          | X     | X         |         |
| QOQTD      | Quarter-over-Quarter-to-Date                   | X     | X         | X       |
| QTDOPQ     | Quarter-to-Date over Full Previous Quarter     | X     | X         | X       |
| MTD        | Month-to-Date                                  |       | X         |         |
| MoM        | Full Month over Full Month                     |       |           | X       |
| MoMTD      | Current Month-to-Date over Prior Month-to-Date | X     | X         | X       |
| PMTD       | Prior Month’s MTD amount                       | X     | X         |         |
| MTDOPM     | Month-to-Date over Full Previous Month         | X     | X         | X       |
| WTD        | Week-to-Date                                   |       | X         |         |
| WoW        | Full Week over Full Week                       |       |           | X       |
| WoWTD      | Current Week-to-Date over Prior Week-to-Date   | X     | X         | X       |
| PWTD       | Prior Week-to-Date                             | X     | X         |         |
| ATD        | cumlaitve total from inception to date         |       | x         |         |
| DoD        | Full Day over Full Day                         |       |           | X       |

## How to use fpaR?

### Time Intelligence

When you execute a time intelligence function, it will return a `ti`
class object with a custom print method that explains what the function
is doing and a summary of transformation steps and the calendar
attributes

You will see a print method that explains the function’s actions,
details the calendar’s attributes, summarizes the main transformation
steps and lists out possible next actions

To return a tibble of results, pass the ti object through to
`calculate()`

``` r
sales |> 
   mtd(.date=order_date,.value = margin,calendar_type = "standard") |> 
   calculate()
```

    # Source:     SQL [?? x 5]
    # Database:   DuckDB v1.1.3 [hagan@Linux 6.9.3-76060903-generic:R 4.4.2//tmp/Rtmp5z4vIG/file1c6961d6cdb0e]
    # Ordered by: date
        year month date       margin mtd_margin
       <dbl> <dbl> <date>      <dbl>      <dbl>
     1  2021     8 2021-08-01    0           0 
     2  2021     8 2021-08-02    0           0 
     3  2021     8 2021-08-03    0           0 
     4  2021     8 2021-08-04 2091.       2091.
     5  2021     8 2021-08-05  112.       2203.
     6  2021     8 2021-08-06   71.2      2274.
     7  2021     8 2021-08-07 4406.       6681.
     8  2021     8 2021-08-08    0        6681.
     9  2021     8 2021-08-09 2044.       8724.
    10  2021     8 2021-08-10 6847.      15572.
    # ℹ more rows

If you using a tibble data, under the hood, `fpaR` is converting that
tibble to a [duckdb]() database and using [dbplyr]() to execute all the
calculations. Use `dplyr::collect()` to return it as a regular tibble

``` r
sales |> 
   mtd(.date=order_date,.value = margin,calendar_type = "standard") |> 
   calculate() |> 
   dplyr::arrange(date) |> 
   dplyr::collect() |> 
   head(10)
```

    # A tibble: 10 × 5
        year month date        margin mtd_margin
       <dbl> <dbl> <date>       <dbl>      <dbl>
     1  2021     5 2021-05-18   407.        407.
     2  2021     5 2021-05-19   711.       1118.
     3  2021     5 2021-05-20  1424.       2542.
     4  2021     5 2021-05-21 11339.      13881.
     5  2021     5 2021-05-22  5359.      19240.
     6  2021     5 2021-05-23     0       19240.
     7  2021     5 2021-05-24     0       19240.
     8  2021     5 2021-05-25   793.      20033.
     9  2021     5 2021-05-26    74.6     20107.
    10  2021     5 2021-05-27  1433.      21540.

what if you need the analysis at the group level?

Simply pass through the groups that you want with `dplyr::group_by()`
and time intelligence function will create a custom calendar for each
group level.

``` r
sales |> 
   dplyr::group_by(customer_key,store_key) |> 
   yoy(.date=order_date,.value = margin,calendar_type = "standard")
```

    • The calendar was aggregated to the year time unit
    • A standard calendar is created with 2 groups
    • Calendar ranges from 2021-05-18 to 2024-04-20
    • 222 days were missing and replaced with 0
    • New date column date and year was created

------------------------------------------------------------------------

## Why do we need this package when we have lubridate?

Time-based comparisons, such as Year-over-Year (YoY),
Quarter-over-Quarter (QoQ), and Month-to-Date (MTD), are common for
tracking business performance. However, they come with challenges:

- Many datasets **do not have continuous dates**, especially if data is
  recorded only on business days or for active transactions

- Period imbalances between periods (Eg. the different number of days
  between February vs. January) can create misleading analysis or trends

- Your organization may use a non-standard calendar such as a 5-5-4,
  4-4-5, or 13 month calendar and you need the ability to do time
  intelligence functions

- Your data may be in excel sheets, csv or databases and you need to
  inter-operable framework to switch between all your data types

**Example Issue:**

| order_date | margin |
|------------|--------|
| 2024-01-01 | 1200   |
| 2024-01-03 | 1100   |
| 2024-01-06 | 1300   |

If we use `dplyr::lag()` to compare **Day-over-Day (DoD)** revenue, we
would be missing `2024-01-02` and `2024-01-04` which will lead to
incorrect answers or trends

To correct this, `fpaR` automatically **fills in missing dates** for
each group of your data to ensure there are no missing periods when
comparing periods

## Issue 2

    # A tibble: 10 × 6
       date        year month pmtd_margin previous_period_short previous_period_long
       <date>     <dbl> <dbl>       <dbl>                 <dbl>                <dbl>
     1 2022-02-28  2022     2     142668.                     0                    4
     2 2022-02-27  2022     2     132202.                     0                    0
     3 2022-02-26  2022     2     129436.                     0                    0
     4 2022-02-25  2022     2     123449.                     0                    0
     5 2022-02-24  2022     2     112772.                     0                    0
     6 2022-02-23  2022     2     112085.                     0                    0
     7 2022-02-22  2022     2     112085.                     0                    0
     8 2022-02-21  2022     2     101241.                     0                    0
     9 2022-02-20  2022     2      97958.                     0                    0
    10 2022-02-19  2022     2      95431.                     0                    0

## Issue 3

**Practice Datasets**

- This package leverages the `contoso` package for its analysis. The
  contoso datasets are fictional datasets of sales transaction which are
  helpful for business intelligence related analysis (or general
  database management practices)

![Contoso](fig/contoso_schema.svg)

This is an **active work-in-progress**, and feedback, testing, and
contributions are welcome!

------------------------------------------------------------------------

## **Core Functionalities**

The package is centered around **three main areas of analysis**:

### **Time Intelligence Functions**

A comprehensive set of time-based analytical functions to track trends
over different periods.

*Support for non-standard calendars (e.g., 5-5-4 fiscal calendar) is
under development.*

------------------------------------------------------------------------

## Future capabilities

### **Segmentation Strategies**

Provides functions to segment and categorize your data into meaningful
business categories.

#### **Example Segmentation Methods:**

- **ABC Classification** – Categorizing products/customers based on
  revenue contribution.  
- **New vs. Returning** – Distinguishing first-time buyers from repeat
  customers.  
- **K-means Clustering** – Grouping data points based on patterns.  
- **UMAP (Uniform Manifold Approximation and Projection)** –
  Dimensionality reduction for clustering.

------------------------------------------------------------------------

### **Factor / Variation Analysis**

Breaks down revenue or cost changes into **price, volume, and mix
effects**.

#### **Use Cases:**

- Analyzing revenue growth **due to price increases vs. increased sales
  volume**.  
- Measuring the impact of **product mix changes on profitability**.

------------------------------------------------------------------------

[^1]: I plan use [duckplyr](https://duckplyr.tidyverse.org/index.html)
    once it expands support for lubridate functions
