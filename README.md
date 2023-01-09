f the business to these metrics.
The $50M value estimate is associated with nominal working capital, which we can clarify in a future meeting. The main factor affecting the range is the responsiveness of the business to value capture opportunities, as well as product prices. We will have initial results from the F&L pilot by the end of the month, which will serve as a roadmap for WCI. If operations are located in the hub, WCI's ability to capture value can be accelerated. This can be achieved by harmonizing data elements across receivables and payables, integrating WCI objectives throughout operational KPIs and management reports, prioritizing and incentivizing staff and management towards WCI objectives, and leveraging existing integration and interfaces with procurement, sales, and the supply chain to present and execute value-capturing workflows. Without these advantages, I estimate that it will delay the full value capture by 2 years, as other priorities such as MSP work migrations will consume management attention and capacity. Key enablers of value capture, regardless of operations, include the successful implementation of Hiradius (which harmonizes the receivables data set and provides visibility into invoice status) and the Snowflake implementation of the chemicals and procurement dataset. The piece that is missing without operations is the interface and interaction with value chains, which should be achieved through EPOs and their existing stewardship points, GBS KPIs, and the Business Enable Control Tower concept that brings visibility to value capture opportunities and the action or inaction of the business on these metrics.

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

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


SELECT
    CASE
        WHEN COLUMN_NAME LIKE '%DATE%' AND value = '1900-01-01' THEN NULL
        ELSE value
    END AS value,
    ...
FROM yourTable










<!-- README.md is generated from README.Rmd. Please edit that file -->

# fpaR

<!-- badges: start -->
<!-- badges: end -->

The goal of fpaR is to …

## Installation

You can install the development version of fpaR from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("alejandrohagan/fpaR")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(fpaR)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/v1/examples>.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
