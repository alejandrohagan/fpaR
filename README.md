# README


``` r
devtools::load_all()
```

    ℹ Loading fpaR

``` r
devtools::document()
```

    ℹ Updating fpaR documentation
    ℹ Loading fpaR

# **fpaR: A Business Intelligence Toolkit for Financial Planning & Analysis (FP&A)**

## **Introduction**

`fpaR` is a collection of business intelligence tools designed to
simplify common **financial planning and analysis (FP&A)** tasks such as
time intelligence calculations, customer segmentation, and factor
analysis.

The package is inspired by best practices from a collection of **blogs,
books, industry research, and hands-on work experience**, consolidating
frequently performed business analyses into a **fast, efficient, and
reusable framework**.

In particular, the time intelligence functions are heavily inspired by
[PowerBI DAX](https://www.sqlbi.com/) functions

Under the hood, these functions are built upon the great foundations of
[dbplyr](https://dbplyr.tidyverse.org/),
[duckdb](https://github.com/duckdb/duckdb-r), and
[lubridate](https://lubridate.tidyverse.org/)

## What is in fpaR?

**Time intelligence**

`fpaR` provides readily available functions for most time intelligence
anlaysis such as **Year-over-Year (YoY)**, **Month-to-Date (MTD)**, and
**Current Year-to-Date over Previous Year-to-Date (YoYTD)** efficiently.

**These features offer key benefits: **

- **Auto-fill missing dates**: Ensures no missing periods in your
  calculations, even if your dataset doesn’t include every single day.

- **Flexible calendar options**: Handle comparisons based on a
  **standard calendar** or a **5-5-4 fiscal calendar** to accommodate
  different reporting frameworks

Time intelligence can be summarized if they aggregate a value, shift a
value or compare against another value.

Below is the full list of time intelligence functions

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

`fpaR` simplifies these workflows by providing **ready-to-use
functions** that work seamlessly with both **tibble data frames** and
all modern database clients (DuckDB, Snowflake, SQLite, etc).

Even if you are working with tibbles, most functions are optimized to
leverage **[DuckDB](https://github.com/duckdb/duckdb-r) for superior
speed and performance**.

### **Key Features at a Glance:**

- **Flexible support** for both local tibbles and databases  
- **Optimized for speed** using DuckDB  
- **Reusable business intelligence functions** for FP&A, eliminating
  redundant SQL queries
- **Built-in segmentation** and factor analysis techniques to uncover
  business insights quickly
- **Growing and evolving package** with planned enhancements, including
  support for non-standard fiscal calendars

------------------------------------------------------------------------

## Why do we need custom time intelligence functions?

Time-based comparisons, such as **Year-over-Year (YoY),
Quarter-over-Quarter (QoQ), and Month-to-Date (MTD)**, are common for
tracking business performance. However, they come with challenges:

### **Missing Dates**

- Many datasets **do not have continuous dates**, especially if data is
  recorded only on business days or for active transactions

- Missing weekends or holidays can distort **rolling period
  calculations**

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

## how to use fpaR?

When you execute a time intelligence function, it will return a ti
object with a custom print method that explains what the function is
doing and a summary of transformation steps and the calendar attributes

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

    • The calendar was aggregated to the day time unit
    • A standard calendar is created with 0 groups
    • Calendar ranges from 2021-05-18 to 2024-04-20
    • 222 days were missing and replaced with 0
    • New date column date, year and month was created

    ── Actions: ──

    ✔ Aggregate margin

    ✖ Shift

    ✖ Compare

    ── Next Steps: ──

    • Use `calculate()` to return the results

    ────────────────────────────────────────────────────────────────────────────────

to return a tibble of results, pass the ti object through to
`calculated()`

``` r
sales |> 
   mtd(.date=order_date,.value = margin,calendar_type = "standard") |> 
   calculate()
```

    # Source:     SQL [?? x 5]
    # Database:   DuckDB v1.1.3 [hagan@Linux 6.9.3-76060903-generic:R 4.4.2//tmp/RtmpeNY6FZ/file1399f114f84fd]
    # Ordered by: date
        year month date       margin mtd_margin
       <dbl> <dbl> <date>      <dbl>      <dbl>
     1  2023     9 2023-09-11   796.     36443.
     2  2023     9 2023-09-12 10441.     46884.
     3  2023     9 2023-09-13  1558.     48442.
     4  2023     9 2023-09-14  2599.     51041.
     5  2023     9 2023-09-15  1642.     52684.
     6  2023     9 2023-09-16  3885.     56568.
     7  2023     9 2023-09-17     0      56568.
     8  2023     9 2023-09-18   447.     57015.
     9  2023     9 2023-09-19  5164.     62180.
    10  2023     9 2023-09-20  3211.     65390.
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
group.

``` r
sales |> 
   dplyr::group_by(customer_key,store_key) |> 
   yoy(.date=order_date,.value = margin,calendar_type = "standard")
```

    ── Year over year ──────────────────────────────────────────────────────────────

    Function: `yoy` was executed

    ── Description: ──

    This creates a full year `sum()` of the previous year margin and compares it
    with the full year `sum()` current year margin from the start of the standard
    calendar year to the end of the year

    ── Calendar: ──

    • The calendar was aggregated to the year time unit
    • A standard calendar is created with 2 groups
    • Calendar ranges from 2021-05-18 to 2024-04-20
    • 222 days were missing and replaced with 0
    • New date column date and year was created

    ── Actions: ──

    ✔ Aggregate margin

    ✔ Shift 1 year

    ✔ Compare previous year

    customer_key and store_key groups are in the table

    ── Next Steps: ──

    • Use `calculate()` to return the results

    ────────────────────────────────────────────────────────────────────────────────

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

### \*\* Segmentation Strategies \*\*

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

## **Installation**

Install the development version of `fpaR` from GitHub:

``` r
# Install using pak (recommended)
pak::pak("alejandrohagan/fpaR")
```
