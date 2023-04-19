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
```

    Error in readRDS(file) : error reading from connection

    ── R CMD build ─────────────────────────────────────────────────────────────────
    * checking for file ‘/tmp/RtmpBHFLDL/remotes4860968d7cf1f/alejandrohagan-fpaR-ae8fe05/DESCRIPTION’ ... OK
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

``` r
library(tidyverse)
```

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
  augments with cumsum

- **show_excel()** will show interim transformation steps in excel

- **divide** will give an alternative results to dividing by zero that
  isn’t `Inf`

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

<div id="topqovtkxm" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#topqovtkxm .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#topqovtkxm .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#topqovtkxm .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#topqovtkxm .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#topqovtkxm .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#topqovtkxm .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#topqovtkxm .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#topqovtkxm .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#topqovtkxm .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#topqovtkxm .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#topqovtkxm .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#topqovtkxm .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#topqovtkxm .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#topqovtkxm .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#topqovtkxm .gt_from_md > :first-child {
  margin-top: 0;
}

#topqovtkxm .gt_from_md > :last-child {
  margin-bottom: 0;
}

#topqovtkxm .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#topqovtkxm .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#topqovtkxm .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#topqovtkxm .gt_row_group_first td {
  border-top-width: 2px;
}

#topqovtkxm .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#topqovtkxm .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#topqovtkxm .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#topqovtkxm .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#topqovtkxm .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#topqovtkxm .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#topqovtkxm .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#topqovtkxm .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#topqovtkxm .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#topqovtkxm .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-left: 4px;
  padding-right: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#topqovtkxm .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#topqovtkxm .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#topqovtkxm .gt_left {
  text-align: left;
}

#topqovtkxm .gt_center {
  text-align: center;
}

#topqovtkxm .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#topqovtkxm .gt_font_normal {
  font-weight: normal;
}

#topqovtkxm .gt_font_bold {
  font-weight: bold;
}

#topqovtkxm .gt_font_italic {
  font-style: italic;
}

#topqovtkxm .gt_super {
  font-size: 65%;
}

#topqovtkxm .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 75%;
  vertical-align: 0.4em;
}

#topqovtkxm .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#topqovtkxm .gt_indent_1 {
  text-indent: 5px;
}

#topqovtkxm .gt_indent_2 {
  text-indent: 10px;
}

#topqovtkxm .gt_indent_3 {
  text-indent: 15px;
}

#topqovtkxm .gt_indent_4 {
  text-indent: 20px;
}

#topqovtkxm .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="Short Name">Short Name</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="Description">Description</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="Short Name" class="gt_row gt_left">YTD</td>
<td headers="Description" class="gt_row gt_left">Year-to-date</td></tr>
    <tr><td headers="Short Name" class="gt_row gt_left">QTD</td>
<td headers="Description" class="gt_row gt_left">Quarter-to-date</td></tr>
    <tr><td headers="Short Name" class="gt_row gt_left">MAT</td>
<td headers="Description" class="gt_row gt_left">Moving annual total</td></tr>
    <tr><td headers="Short Name" class="gt_row gt_left">PY</td>
<td headers="Description" class="gt_row gt_left">Previous year</td></tr>
    <tr><td headers="Short Name" class="gt_row gt_left">PQ</td>
<td headers="Description" class="gt_row gt_left">Previous quarter</td></tr>
    <tr><td headers="Short Name" class="gt_row gt_left">PM</td>
<td headers="Description" class="gt_row gt_left">Previous month</td></tr>
    <tr><td headers="Short Name" class="gt_row gt_left">PYC</td>
<td headers="Description" class="gt_row gt_left">Previous year complete</td></tr>
    <tr><td headers="Short Name" class="gt_row gt_left">PQC</td>
<td headers="Description" class="gt_row gt_left">Previous quarter complete</td></tr>
    <tr><td headers="Short Name" class="gt_row gt_left">PMC</td>
<td headers="Description" class="gt_row gt_left">Previous month complete</td></tr>
    <tr><td headers="Short Name" class="gt_row gt_left">PP</td>
<td headers="Description" class="gt_row gt_left">Previous period; automatically selects year, quarter, or month</td></tr>
    <tr><td headers="Short Name" class="gt_row gt_left">PYMAT</td>
<td headers="Description" class="gt_row gt_left">Previous year moving annual total</td></tr>
    <tr><td headers="Short Name" class="gt_row gt_left">YOY</td>
<td headers="Description" class="gt_row gt_left">Year-over-year</td></tr>
    <tr><td headers="Short Name" class="gt_row gt_left">QOQ</td>
<td headers="Description" class="gt_row gt_left">Quarter-over-quarter</td></tr>
    <tr><td headers="Short Name" class="gt_row gt_left">MOM</td>
<td headers="Description" class="gt_row gt_left">Month-over-month</td></tr>
    <tr><td headers="Short Name" class="gt_row gt_left">MATG</td>
<td headers="Description" class="gt_row gt_left">Moving annual total growth</td></tr>
    <tr><td headers="Short Name" class="gt_row gt_left">POP</td>
<td headers="Description" class="gt_row gt_left">Period-over-period; automatically selects year, quarter, or month</td></tr>
    <tr><td headers="Short Name" class="gt_row gt_left">PYTD</td>
<td headers="Description" class="gt_row gt_left">Previous year-to-date</td></tr>
    <tr><td headers="Short Name" class="gt_row gt_left">PQTD</td>
<td headers="Description" class="gt_row gt_left">Previous quarter-to-date</td></tr>
    <tr><td headers="Short Name" class="gt_row gt_left">YOYTD</td>
<td headers="Description" class="gt_row gt_left">Year-over-year-to-date</td></tr>
    <tr><td headers="Short Name" class="gt_row gt_left">QOQTD</td>
<td headers="Description" class="gt_row gt_left">   Quarter-over-quarter-to-date</td></tr>
    <tr><td headers="Short Name" class="gt_row gt_left">YTDOPY</td>
<td headers="Description" class="gt_row gt_left">Year-to-date-over-previous-year</td></tr>
    <tr><td headers="Short Name" class="gt_row gt_left">QTDOPQ</td>
<td headers="Description" class="gt_row gt_left">Quarter-to-date-over-previous-quarter</td></tr>
  </tbody>
  
  
</table>
</div>

[^1]: See table below
