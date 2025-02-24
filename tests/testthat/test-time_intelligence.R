library(tidyverse)
library(S7)
library(testthat)
# load arguments / data

.data <- sales |> group_by(customer_key)

### functions correctly run --------------

test_that("ti functions correctly run",{

  x <- testthat::expect_error(fpaR::ytd(.data = .data,.date = order_date,.value = margin,calendar_type = "standard"),NA)
  x <- testthat::expect_error(fpaR::pytd(.data =.data ,.date = order_date,.value = margin,calendar_type = "standard",lag_n = 1),NA)
  x <- testthat::expect_error(fpaR::yoytd(.data,.date = order_date,.value = margin,calendar_type = "standard",lag_n = 1),NA)
  x <- testthat::expect_error(fpaR::yoy(.data,.date = order_date,.value = margin,calendar_type = "standard",lag_n = 1),NA)
  x <- testthat::expect_error(fpaR::ytdopy(.data,.date = order_date,.value = margin,calendar_type = "standard",lag_n = 1),NA)
  x <- testthat::expect_error(fpaR::qtd(.data,.date = order_date,.value = margin,calendar_type = "standard"),NA)
  x <- testthat::expect_error(fpaR::pqtd(.data,.date = order_date,.value = margin,calendar_type = "standard",lag_n = 1),NA)
  x <- testthat::expect_error(fpaR::qtdopq(.data,.date = order_date,.value = margin,calendar_type = "standard",lag_n = 1),NA)
  x <- testthat::expect_error(fpaR::qoq(.data,.date = order_date,.value = margin,calendar_type = "standard",lag_n = 1),NA)
  x <- testthat::expect_error(fpaR::qoqtd(.data,.date = order_date,.value = margin,calendar_type = "standard",lag_n = 1),NA)
  x <- testthat::expect_error(fpaR::mtd(.data,.date = order_date,.value = margin,calendar_type = "standard"),NA)
  x <- testthat::expect_error(fpaR::pmtd(.data,.date = order_date,.value = margin,calendar_type = "standard",lag_n = 1),NA)
  x <- testthat::expect_error(fpaR::momtd(.data,.date = order_date,.value = margin,calendar_type = "standard",lag_n = 1),NA)
  x <- testthat::expect_error(fpaR::mom(.data,.date = order_date,.value = margin,calendar_type = "standard",lag_n = 1),NA)
  x <- testthat::expect_error(fpaR::mtdopm(.data,.date = order_date,.value = margin,calendar_type = "standard",lag_n = 1),NA)
  x <- testthat::expect_error(fpaR::wtd(.data,.date = order_date,.value = margin,calendar_type = "standard"),NA)
  x <- testthat::expect_error(fpaR::pwtd(.data,.date = order_date,.value = margin,calendar_type = "standard",lag_n = 1),NA)
  x <- testthat::expect_error(fpaR::wowtd(.data,.date = order_date,.value = margin,calendar_type = "standard",lag_n = 1),NA)
  x <- testthat::expect_error(fpaR::wow(.data,.date = order_date,.value = margin,calendar_type = "standard",lag_n = 1),NA)
  x <- testthat::expect_error(fpaR::wtdopw(.data,.date = order_date,.value = margin,calendar_type = "standard",lag_n = 1),NA)
  x <- testthat::expect_error(fpaR::atd(.data,.date = order_date,.value = margin,calendar_type = "standard"),NA)
  x <- testthat::expect_error(fpaR::dod(.data,.date = order_date,.value = margin,calendar_type = "standard",lag_n = 1),NA)
  }
)


## sum total validation checks



test_that(
  "ytd - sum validation",{
    testthat::expect_true(
      all(
        sales |>
          ytd(order_date,quantity,"standard") |>
          calculate() |>
          collect() |>
          arrange(date) |>
          dplyr::group_by(
            year
          ) |>
          dplyr::summarise(
            sum_value=sum(quantity,na.rm=TRUE)
            ,max_ytd=last(ytd_quantity),
            ,.groups="drop"
          ) |>
          dplyr::mutate(
            delta=sum_value-max_ytd
          ) |>
          dplyr::pull(delta)==0
      )
    )
  }
)


test_that(
  "mtd - sum validation",{
    testthat::expect_true(
      all(
        sales |>
          mtd(order_date,quantity,"standard") |>
          calculate() |>
          collect() |>
          arrange(date) |>
          dplyr::group_by(
            year,month
          ) |>
          dplyr::summarise(
            sum_value=sum(quantity,na.rm=TRUE)
            ,max_mtd=last(mtd_quantity),
            ,.groups="drop"
          ) |>
          dplyr::mutate(
            delta=sum_value-max_mtd
          ) |>
          dplyr::pull(delta)==0
      )
    )
  }
)



test_that(
  "yoy - sum validation",{
    testthat::expect_true(
      all(
        sales |>
          yoy(order_date,quantity,"standard") |>
          calculate() |>
          collect() |>
          arrange(year) |>
          mutate(
            lag_value=lead(yoy_quantity,1)
            ,delta=quantity-lag_value
          ) |>
          filter(
            !is.na(delta)
          ) |>
          dplyr::pull(delta)==0
      )
    )
  }
)
