README
================

# Introduction to fpaR

This is a collection of business intelligence tools and patterns to help
with common data queries and insights. This is based on numerous blogs,
books, post and work experience in what I have seen as most frequent
analysis /work effort

The package functions can be split between three focus areas:

1)  Segmentation strategies

    - UMAP
    - Kmeans
    - ABC
    - New vs. Returning Customers/ Vendors

2)  Time intelligence functions

    - List of standard time intelligence functions such as
      month-to-date, year over year for standard and non-standard
      calendars
    - See table before complete list
    - Non-standard calendar such a 5-5-4 calendar

3)  Variance analysis strategies

    - Applied statsitical techniques to underestand variance analysis
      including graphing techniques
      - quantile regression
      - anova
    - Like for like comparison
    - Something like a mix between marginalmeans, anova, lm and rq
      - You calculate certain components such as price, customer
        turnover, high margin vs. low margin segmentations
      - Then do a quantile regression on the parts to see how there is
        difference between high margin and low margin indicators
      - ideally we then have our factors and we can simply take the
        coefficients and see directly the components that is driving
        changes
        - so we start at customer level, store level and product level
          attributes and other componets that when muliplied and added
          get what you are calcualted -price X vol =rev -avg.price X
          product mix -vol =product mix X quantity

## Installation

You can install the development version of fpaR from
[GitHub](https://github.com/alejandrohagan/fpaR) with:

``` r
# install.packages("devtools")
devtools::install_github("alejandrohagan/fpaR")
```


    ── R CMD build ─────────────────────────────────────────────────────────────────
    * checking for file ‘/tmp/Rtmp0ft9Zl/remotes54122fb95d/alejandrohagan-fpaR-1b32a6f/DESCRIPTION’ ... OK
    * preparing ‘fpaR’:
    * checking DESCRIPTION meta-information ... OK
    * checking for LF line-endings in source and make files and shell scripts
    * checking for empty or unneeded directories
      NB: this package now depends on R (>= 3.5.0)
      WARNING: Added dependency on R >= 3.5.0 because serialized objects in
      serialize/load version 3 cannot be read in older versions of R.
      File(s) containing such objects:
        ‘fpaR/data/contoso_dim_channel.RData’
        ‘fpaR/data/contoso_dim_date.RData’
        ‘fpaR/data/contoso_dim_product.RData’
        ‘fpaR/data/contoso_dim_product_subcategory.RData’
        ‘fpaR/data/contoso_dim_promotion.RData’
        ‘fpaR/data/contoso_fact_sales.RData’
    * building ‘fpaR_0.0.0.9000.tar.gz’

## Package Components

This is correctly under development and is in early stages!

This package is heavily inspired by the DAX Patterns for the time
intelligence functions

**Segmentation helpers:**

- **make_segmentation()** will use kmeans and umap to create a
  segmentation
- **abc_segmentation()**
- Turnover customers

**Calculation Helpers**

- **count_plus()** will replicate the handy dplyr::count() function but
  augments with portions and cumulative sum of proportions

- **show_excel()** will show your tibbles or data frames steps in excel

- **divide** will give an default NA or alternative to dividing by zero
  that isn’t `Inf` (model after [DAX
  divide](https://learn.microsoft.com/en-us/dax/divide-function-dax))

- Calculate() model after DAX calculate, basically a supercharged sumif
  that allows you to filter data with or with any filter context

**Time Intelligence Functions**

- Standard time intelligence functions[^1]

  - These are time intelligence fucntions that are consistent with a
    standard calendar (eg. the calendar on your computer or your phone)

- Non-standard time intelligence functions

  - These are calendars that maybe more common in the retail business
    (Eg. 5-4-5 quarter calcualtion)
  - These calendars aim to control for weekends as that may be larger
    driver of sales

**Variation Analysis**

- variance / factor analysis

**Practice Datasets**

- Microsoft’s Contoso Dataset to help with practice with transaction
  data

<!-- -->

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

[^1]: See table below
