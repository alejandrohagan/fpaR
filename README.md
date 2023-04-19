README
================

# fpaR

This is a collection of business intelligence tools and patterns to help
with common data queries and insights. This is based on numerous blogs,
books, post and work experience in what I have seen as most frequent
analysis /work effort

The package functions can be split between three focus areas:

1)  Segmentation strategies

    - UMAP
    - Kmeans
    - ABC

2)  Time intelligence functions

    - List of standard time intelligence functions such as
      month-to-date, year over year for standard and non-standard
      calendars
    - See table before complete list

3)  Variance analysis strategies

    - Applied statsitical techniques to underestand variance analysis
      including graphing techniques
      - quantile regression
      - anova
    - Like for like comparison

## Installation

You can install the development version of fpaR from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("alejandrohagan/fpaR")

library(tidyverse)
```

This is correctly under development and is in early stages!

This package is heavily inspired by the DAX Patterns for the time
intelligence functions

Segmentation helpers:

- **make_segmentation()** will use kmeans and umap to create a
  segmentation
- **abc_segmentation()**
- new vs. returning customers

Calculation Helpers

- **count_plus()** will replicate the handy dplyr::count() function

- **show_excel()** will show interim tranformation steps in excell

- **divide** will give an alternative results to dividing by zero that
  isn’t `Inf`

- standard time intelligence functions

- non-standard time intelligence functions

- Transition metrics

- variance / factor analysis

- Practice datasets

``` r
tibble::tribble(
  ~"Short Name",~"Description",
  "YTD",    "Year-to-date",  
"QTD",  "Quarter-to-date",           
"MAT",  "Moving annual total",           
"PY",   "Previous year",             
"PQ",   "Previous quarter",              
"PM",   "Previous month",            
"PYC",  "Previous year complete",            
"PQC",  "Previous quarter complete",             
"PMC",  "Previous month complete",           
"PP",   "Previous period; automatically selects year, quarter, or month" ,   
"PYMAT",    "Previous year moving annual total" ,            
"YOY",  "Year-over-year",           
"QOQ",  "Quarter-over-quarter",             
"MOM",  "Month-over-month",         
"MATG",     "Moving annual total growth",           
"POP",  "Period-over-period; automatically selects year, quarter, or month",            
"PYTD",     "Previous year-to-date",             
"PQTD", "Previous quarter-to-date",              
"YOYTD",    "Year-over-year-to-date" ,          
"QOQTD", "  Quarter-over-quarter-to-date",          
"YTDOPY",   "Year-to-date-over-previous-year",          
"QTDOPQ",   "Quarter-to-date-over-previous-quarter",    
  
)
```

    # A tibble: 22 × 2
       `Short Name` Description                                                   
       <chr>        <chr>                                                         
     1 YTD          Year-to-date                                                  
     2 QTD          Quarter-to-date                                               
     3 MAT          Moving annual total                                           
     4 PY           Previous year                                                 
     5 PQ           Previous quarter                                              
     6 PM           Previous month                                                
     7 PYC          Previous year complete                                        
     8 PQC          Previous quarter complete                                     
     9 PMC          Previous month complete                                       
    10 PP           Previous period; automatically selects year, quarter, or month
    # ℹ 12 more rows
