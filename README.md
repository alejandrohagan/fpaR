# README

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

| Short Name | Description                                              | Shift | Aggregate | Compare |
|------------|----------------------------------------------------------|-------|-----------|---------|
| YoY        | Full Year over Year                                      |       |           | X       |
| YTD        | Year-to-Date                                             | X     |           |         |
| PYTD       | Prior Year-to-Date amount                                | X     | X         |         |
| YoYTD      | Current Year-to-Date over Prior Year-to-Date             | X     | X         | X       |
| YTDOPY     | Year-to-Date over Full Previous Year                     | X     | X         | X       |
| QoQ        | Full Quarter over Quarter                                |       |           | X       |
| QTD        | Quarter-to-Date                                          | X     |           |         |
| PQTD       | Prior Quarter-to-Date                                    | X     | X         |         |
| QOQTD      | Quarter-over-Quarter-to-Date                             | X     | X         | X       |
| QTDOPQ     | Quarter-to-Date over Full Previous Quarter               | X     | X         | X       |
| MTD        | Month-to-Date                                            | X     |           |         |
| MoM        | Full Month over Full Month                               |       |           | X       |
| MoMTD      | Current Month-to-Date over Prior Month-to-Date           | X     | X         | X       |
| PMTD       | Prior Month's MTD amount                                 | X     | X         |         |
| MTDOPM     | Month-to-Date over Full Previous Month                   | X     | X         | X       |
| WTD        | Week-to-Date                                             | X     |           |         |
| WoW        | Full Week over Full Week                                 |       |           | X       |
| WoWTD      | Current Week-to-Date over Prior Week-to-Date             | X     | X         | X       |
| PWTD       | Prior Week-to-Date                                       | X     | X         |         |
| ATD        | Cumulative total from inception to date                  | X     |           |         |
| DoD        | Full Day over Full Day                                   |       |           | X       |

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


| Date       | Revenue |
|------------|---------|
| 2024-01-01 | 1,200   |
| 2024-01-03 | 1,100   |
| 2024-01-06 | 1,300   |



If we use `dplyr::lag()` to compare **Day-over-Day (DoD)** revenue, we
would be missing `2024-01-02` and `2024-01-04` which will lead to
incorrect answers or trends

To correct this, `fpaR` automatically **fills in missing dates** for
each group of your data to ensure there are no missing periods when
comparing periods

**Practice Datasets**

- Microsoft’s Contoso Dataset to help with practice with transaction
  data

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
