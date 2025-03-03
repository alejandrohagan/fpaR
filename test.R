library(tidyverse)
library(assertthat)
library(S7)
library(rlang)
library(dbplyr)
library(testthat)
devtools::document()



x <- pqtd(sales,order_date,margin,calendar_type="standard",lag_n = 1)



