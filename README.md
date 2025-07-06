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
[PowerBI DAX](https://www.sqlbi.com/) functions.

Under the hood, these functions are built upon the great foundations of:

- [dbplyr](https://dbplyr.tidyverse.org/)  
- [duckdb](https://github.com/duckdb/duckdb-r)
- [lubridate](https://lubridate.tidyverse.org/)

`fpaR` is designed to seamlessly work with either tibbles or modern
databases (DuckDB, Snowflake, SQLite, etc) with a unified syntax.

Even if you are working with tibbles, most functions are optimized to
leverage [DuckDB](https://github.com/duckdb/duckdb-r) for superior speed
and performance.[^1]

By default most `fpaR` function returns a lazy DBI object which you can
return as a tibble with `dplyr::collect()`

## Key features & benefits

- **Unified syntax** regardless if your data is in a tibble or a
  database
- **Optimized for speed and scale** your data is converted to a local
  [duckdb](https://github.com/duckdb/duckdb-r) database to scale your
  calculations
- **Instant clarity** as every function summarizes its transformation
  actions so that you can understand and validate the functions results

## Installation

Install the development version of `fpaR` from GitHub:

``` r
# Install using pak or install.package()

pak::pak("alejandrohagan/fpaR")
```

## What is in fpaR?

There are 3 main categories of functions: - Time intelligence (Ready for
testing and feedback) - Segmentation strategies (work in progress) -
Factor Analysis (work in progress)

### Time intelligence

`fpaR` provides readily available functions for most time intelligence
analysis such as **Year-over-Year (YoY)**, **Month-to-Date (MTD)**, and
**Current Year-to-Date over Previous Year-to-Date (YoYTD)** analysis.

These functions are designed to quickly answer most common time
intelligence related analysis in a consistent, fast and transparent way.

**Key benefits:**

- **Auto-fill missing dates**: Ensures no missing periods in your
  datasets so that right period comparisons are performed

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

``` r
sales |> 
   mtd(.date=order_date,.value = margin,calendar_type = "standard") 
```

    ── Month-to-date ───────────────────────────────────────────────────────────────

    Function: `mtd` was executed

    ── Description: ──

    This creates a daily `cumsum()` of the current month margin from the start of
    the standard calendar month to the end of the month

    ── Calendar: ──

    • The calendar aggregated order_date to the day time unit
    • A standard calendar is created with 0 groups
    • Calendar ranges from 2021-05-18 to 2024-04-20
    • 222 days were missing and replaced with 0
    • New date column date, year and month was created from order_date

    ── Actions: ──

    ✔Aggregate

    ✖Shift

    ✖Compare

    ✖Proportion Of Total

    ✖Count Distinct

    ── Next Steps: ──

    • Use `calculate()` to return the results

    ────────────────────────────────────────────────────────────────────────────────

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

    # Source:     SQL [?? x 7]
    # Database:   DuckDB v1.1.3 [hagan@Linux 6.12.10-76061203-generic:R 4.4.3//tmp/RtmpkvGhnP/file372c865e1f58a]
    # Ordered by: date
        year month date       margin missing_date_indicator mtd_margin
       <dbl> <dbl> <date>      <dbl>                  <dbl>      <dbl>
     1  2021     8 2021-08-01    0                        1         0 
     2  2021     8 2021-08-02    0                        1         0 
     3  2021     8 2021-08-03    0                        1         0 
     4  2021     8 2021-08-04 2091.                       0      2091.
     5  2021     8 2021-08-05  112.                       0      2203.
     6  2021     8 2021-08-06   71.2                      0      2274.
     7  2021     8 2021-08-07 4406.                       0      6681.
     8  2021     8 2021-08-08    0                        1      6681.
     9  2021     8 2021-08-09 2044.                       0      8724.
    10  2021     8 2021-08-10 6847.                       0     15572.
    # ℹ more rows
    # ℹ 1 more variable: days_in_current_period <dbl>

If you using a tibble, under the hood, `fpaR` is converting your data to
a [duckdb](https://github.com/duckdb/duckdb-r) database.

If your data is a datable, the package will leverage
[dbplyr](https://dbplyr.tidyverse.org/) to execute all the calculations.

Either case use `dplyr::collect()` to return your results to a local
tibble.

``` r
sales |>                                                                
   mtd(.date=order_date,.value = margin,calendar_type = "standard") |>  
   calculate() |>                                                       
   dplyr::collect() |>                                               
   head(10)
```

    # A tibble: 10 × 7
        year month date       margin missing_date_indicator mtd_margin
       <dbl> <dbl> <date>      <dbl>                  <dbl>      <dbl>
     1  2021     8 2021-08-01    0                        1         0 
     2  2021     8 2021-08-02    0                        1         0 
     3  2021     8 2021-08-03    0                        1         0 
     4  2021     8 2021-08-04 2091.                       0      2091.
     5  2021     8 2021-08-05  112.                       0      2203.
     6  2021     8 2021-08-06   71.2                      0      2274.
     7  2021     8 2021-08-07 4406.                       0      6681.
     8  2021     8 2021-08-08    0                        1      6681.
     9  2021     8 2021-08-09 2044.                       0      8724.
    10  2021     8 2021-08-10 6847.                       0     15572.
    # ℹ 1 more variable: days_in_current_period <dbl>

### what if you need the analysis at the group level?

Simply pass through the groups that you want with `dplyr::group_by()`
and time intelligence function will create a custom calendar for each
group level.

``` r
sales |>   
   dplyr::group_by(customer_key,store_key) |>  
   yoy(.date=order_date,.value = margin,calendar_type = "standard") 
```

    • The calendar aggregated order_date to the year time unit
    • A standard calendar is created with 2 groups
    • Calendar ranges from 2021-05-18 to 2024-04-20
    • 222 days were missing and replaced with 0
    • New date column date and year was created from order_date

1.  Take your dataset
2.  Select the groups you want with `group_by()`
3.  Prints the sumamrized instructions the actions the functions
    performs to calculate the year over year analysis

## Why do we need this package when we have lubridate?

[Lubridate](https://lubridate.tidyverse.org/) is an excellent package
and is heavily used by the package. The issue isn’t lubridate but rather
issues you may not be aware of in your package.

Time-based comparisons, such as Year-over-Year (YoY),
Quarter-over-Quarter (QoQ), and Month-to-Date (MTD), are common for
tracking business performance. However, they come with challenges:

- Many datasets **do not have continuous dates**, especially if data is
  recorded only on business days or for active transactions

- Period imbalances between periods (Eg. the different number of days
  between February vs. January) can create misleading analysis or trends

- Your analysis may need to reference a non-standard calendar such as a
  5-5-4, 4-4-5, or 13 month calendar

- Your data may be in excel sheets, csv or databases and you need to
  inter-operable framework to switch between all your data types

### Continuous Dates

| order_date | margin |
|------------|--------|
| 2024-01-01 | 1200   |
| 2024-01-03 | 1100   |
| 2024-01-06 | 1300   |
| 2024-01-07 | 900    |
| 2024-01-08 | 1200   |
| 2024-01-09 | 850    |
| 2024-01-11 | 1450   |

If we use `dplyr::lag()` to compare **Day-over-Day (DoD)** revenue, we
would be missing `2024-01-02`, `2024-01-04`, and `2024-01-05` which will
lead to incorrect answers or trends.

To correct this, `fpaR` will complete your calendar for each group for
the missing periods to ensure there are no missing periods when
calculating trends.

## Issue 2

    # A tibble: 10 × 5
       date        year month pmtd_margin days_in_comparison_period
       <date>     <dbl> <dbl>       <dbl>                     <dbl>
     1 2022-02-28  2022     2     142668.                        31
     2 2022-02-27  2022     2     132202.                        27
     3 2022-02-26  2022     2     129436.                        26
     4 2022-02-25  2022     2     123449.                        25
     5 2022-02-24  2022     2     112772.                        24
     6 2022-02-23  2022     2     112085.                        23
     7 2022-02-22  2022     2     112085.                        22
     8 2022-02-21  2022     2     101241.                        21
     9 2022-02-20  2022     2      97958.                        20
    10 2022-02-19  2022     2      95431.                        19

- TBD

## Issue 3

- TBD

## Datasets

- This package leverages the
  [Contoso](https://github.com/alejandrohagan/contoso) package for its
  analysis. The contoso datasets are fictional business transaction of
  Contoso toy company which are helpful for business intelligence
  related analysis

This is an **active work-in-progress**, and feedback, testing, and
contributions are welcome!

## Future capabilities

### **Segmentation Strategies**

Provides functions to segment and categorize your data into meaningful
business categories.

#### **Example Segmentation Methods:**

- **ABC Classification** – Categorizing products/customers based on
  revenue contribution
- **Cohort Analysis** - Categorize your data by a shared time based
  attribute to track over time
- **New vs. Returning** – Distinguishing first-time buyers from repeat
  customers
- **K-means Clustering** – Grouping data points based on patterns
- **UMAP (Uniform Manifold Approximation and Projection)** –
  Dimensionaltiy reduction for clustering

------------------------------------------------------------------------

### **Factor / Variation Analysis**

Breaks down revenue or cost changes into **price, volume, and mix
effects**.

#### **Use Cases:**

- Analyzing revenue growth **due to price increases vs. increased sales
  volume**
- Measuring the impact of **product mix changes on profitability**.

------------------------------------------------------------------------

## Additional references and inspirations

- [PeerChristensen’s Cohort
  Package](https://github.com/PeerChristensen/cohorts)

[^1]: I plan use [duckplyr](https://duckplyr.tidyverse.org/index.html)
    once it expands support for lubricate functions
