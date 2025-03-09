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

<div>

<div id="nnkyrxqikd" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#nnkyrxqikd table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#nnkyrxqikd thead, #nnkyrxqikd tbody, #nnkyrxqikd tfoot, #nnkyrxqikd tr, #nnkyrxqikd td, #nnkyrxqikd th {
  border-style: none;
}
&#10;#nnkyrxqikd p {
  margin: 0;
  padding: 0;
}
&#10;#nnkyrxqikd .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
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
&#10;#nnkyrxqikd .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#nnkyrxqikd .gt_title {
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
&#10;#nnkyrxqikd .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#nnkyrxqikd .gt_heading {
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
&#10;#nnkyrxqikd .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#nnkyrxqikd .gt_col_headings {
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
&#10;#nnkyrxqikd .gt_col_heading {
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
&#10;#nnkyrxqikd .gt_column_spanner_outer {
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
&#10;#nnkyrxqikd .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#nnkyrxqikd .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#nnkyrxqikd .gt_column_spanner {
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
&#10;#nnkyrxqikd .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#nnkyrxqikd .gt_group_heading {
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
&#10;#nnkyrxqikd .gt_empty_group_heading {
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
&#10;#nnkyrxqikd .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#nnkyrxqikd .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#nnkyrxqikd .gt_row {
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
&#10;#nnkyrxqikd .gt_stub {
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
&#10;#nnkyrxqikd .gt_stub_row_group {
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
&#10;#nnkyrxqikd .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#nnkyrxqikd .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#nnkyrxqikd .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#nnkyrxqikd .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#nnkyrxqikd .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#nnkyrxqikd .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#nnkyrxqikd .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#nnkyrxqikd .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#nnkyrxqikd .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#nnkyrxqikd .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#nnkyrxqikd .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#nnkyrxqikd .gt_footnotes {
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
&#10;#nnkyrxqikd .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#nnkyrxqikd .gt_sourcenotes {
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
&#10;#nnkyrxqikd .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#nnkyrxqikd .gt_left {
  text-align: left;
}
&#10;#nnkyrxqikd .gt_center {
  text-align: center;
}
&#10;#nnkyrxqikd .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#nnkyrxqikd .gt_font_normal {
  font-weight: normal;
}
&#10;#nnkyrxqikd .gt_font_bold {
  font-weight: bold;
}
&#10;#nnkyrxqikd .gt_font_italic {
  font-style: italic;
}
&#10;#nnkyrxqikd .gt_super {
  font-size: 65%;
}
&#10;#nnkyrxqikd .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#nnkyrxqikd .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#nnkyrxqikd .gt_indent_1 {
  text-indent: 5px;
}
&#10;#nnkyrxqikd .gt_indent_2 {
  text-indent: 10px;
}
&#10;#nnkyrxqikd .gt_indent_3 {
  text-indent: 15px;
}
&#10;#nnkyrxqikd .gt_indent_4 {
  text-indent: 20px;
}
&#10;#nnkyrxqikd .gt_indent_5 {
  text-indent: 25px;
}
&#10;#nnkyrxqikd .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#nnkyrxqikd div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>

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
| PMTD       | Prior Month's MTD amount                       | X     | X         |         |
| MTDOPM     | Month-to-Date over Full Previous Month         | X     | X         | X       |
| WTD        | Week-to-Date                                   |       | X         |         |
| WoW        | Full Week over Full Week                       |       |           | X       |
| WoWTD      | Current Week-to-Date over Prior Week-to-Date   | X     | X         | X       |
| PWTD       | Prior Week-to-Date                             | X     | X         |         |
| ATD        | cumlaitve total from inception to date         |       | x         |         |
| DoD        | Full Day over Full Day                         |       |           | X       |

</div>

</div>

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

<div>

<div id="hwduskxanv" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#hwduskxanv table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#hwduskxanv thead, #hwduskxanv tbody, #hwduskxanv tfoot, #hwduskxanv tr, #hwduskxanv td, #hwduskxanv th {
  border-style: none;
}
&#10;#hwduskxanv p {
  margin: 0;
  padding: 0;
}
&#10;#hwduskxanv .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
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
&#10;#hwduskxanv .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#hwduskxanv .gt_title {
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
&#10;#hwduskxanv .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#hwduskxanv .gt_heading {
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
&#10;#hwduskxanv .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#hwduskxanv .gt_col_headings {
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
&#10;#hwduskxanv .gt_col_heading {
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
&#10;#hwduskxanv .gt_column_spanner_outer {
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
&#10;#hwduskxanv .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#hwduskxanv .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#hwduskxanv .gt_column_spanner {
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
&#10;#hwduskxanv .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#hwduskxanv .gt_group_heading {
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
&#10;#hwduskxanv .gt_empty_group_heading {
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
&#10;#hwduskxanv .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#hwduskxanv .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#hwduskxanv .gt_row {
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
&#10;#hwduskxanv .gt_stub {
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
&#10;#hwduskxanv .gt_stub_row_group {
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
&#10;#hwduskxanv .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#hwduskxanv .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#hwduskxanv .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#hwduskxanv .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#hwduskxanv .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#hwduskxanv .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#hwduskxanv .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#hwduskxanv .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#hwduskxanv .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#hwduskxanv .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#hwduskxanv .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#hwduskxanv .gt_footnotes {
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
&#10;#hwduskxanv .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#hwduskxanv .gt_sourcenotes {
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
&#10;#hwduskxanv .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#hwduskxanv .gt_left {
  text-align: left;
}
&#10;#hwduskxanv .gt_center {
  text-align: center;
}
&#10;#hwduskxanv .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#hwduskxanv .gt_font_normal {
  font-weight: normal;
}
&#10;#hwduskxanv .gt_font_bold {
  font-weight: bold;
}
&#10;#hwduskxanv .gt_font_italic {
  font-style: italic;
}
&#10;#hwduskxanv .gt_super {
  font-size: 65%;
}
&#10;#hwduskxanv .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#hwduskxanv .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#hwduskxanv .gt_indent_1 {
  text-indent: 5px;
}
&#10;#hwduskxanv .gt_indent_2 {
  text-indent: 10px;
}
&#10;#hwduskxanv .gt_indent_3 {
  text-indent: 15px;
}
&#10;#hwduskxanv .gt_indent_4 {
  text-indent: 20px;
}
&#10;#hwduskxanv .gt_indent_5 {
  text-indent: 25px;
}
&#10;#hwduskxanv .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#hwduskxanv div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>

| Date       | Revenue |
|------------|---------|
| 2024-01-01 | 1,200   |
| 2024-01-03 | 1,100   |
| 2024-01-06 | 1,300   |

</div>

</div>

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
