library(tidyverse)
library(S7)
library(testthat)
devtools::load_all()
# load arguments / data


### functions correctly run --------------

test_that("ti functions correctly run",{
  .data <- sales |> group_by(currency_code)

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
  "ytd - window_order logic",{

    # prep data
    .data <- sales |>
      group_by(currency_code)

    out <-  .data |>
      ytd(order_date,quantity,"standard") |>
      calculate() |>
      collect() |>
      arrange(date) |>
      dplyr::group_by(
        year,currency_code
      ) |>
      dplyr::summarise(
        sum_value=sum(quantity,na.rm=TRUE)
        ,max_value=last(ytd_quantity),
        ,.groups="drop"
      ) |>
      dplyr::mutate(
        delta=sum_value-max_value
      )

    # start test
    testthat::expect_true(
      all(out |> dplyr::pull(delta)==0)
    )
  }
)


test_that(
  "pytd - window_order logic",{
    pytd_tbl <- sales |> group_by(currency_code) |>
      pytd(order_date,quantity,"standard",lag_n = 1) |>
      calculate() |>
      group_by(
        year,currency_code
      ) |>
      summarise(
        max_pytd=max(pytd_quantity,na.rm=TRUE)
        ,.groups="drop"
      ) |>
      collect() |>
      filter(
        year %in% c(2022,2023)
      )

    ytd_tbl <-  sales |> group_by(currency_code)  |>
      ytd(order_date,quantity,"standard") |>
      calculate() |>
      group_by(
        year,currency_code
      ) |>
      summarise(
        max_ytd=max(ytd_quantity,na.rm=TRUE)
        ,.groups="drop"
      ) |>
      collect() |>
      filter(
        year %in% c(2022,2023)
      )

    pytd_validation <- ytd_tbl |>
      full_join(
        pytd_tbl
        ,by=join_by(year,currency_code)
      ) |>
      arrange(currency_code,year) |>
      group_by(
        currency_code
      ) |>
      mutate(
        lead=lead(max_pytd,1)
      ) |>
      drop_na() |>
      mutate(
        validation=max_ytd==lead
      ) |>
      pull(validation)


    testthat::expect_true(
      all(pytd_validation)
    )
  }
)




test_that(
  "qtd - window_order logic",{

    qtd_validation <- sales |>
      group_by(currency_code) |>
      qtd(order_date,quantity,"standard") |>
      calculate() |>
      collect() |>
      arrange(date) |>
      dplyr::group_by(
        year,quarter,currency_code
      ) |>
      dplyr::summarise(
        sum_value=sum(quantity,na.rm=TRUE)
        ,max_value=last(qtd_quantity),
        ,.groups="drop"
      ) |>
      dplyr::mutate(
        delta=sum_value-max_value
      ) |>
      dplyr::pull(delta)==0


    testthat::expect_true(
      all(qtd_validation)
    )
  }
)




test_that(
  "mtd - window_order logic",{

    mtd_validation <- sales |>
      group_by(currency_code) |>
      mtd(order_date,quantity,"standard") |>
      calculate() |>
      collect() |>
      arrange(date) |>
      dplyr::group_by(
        year,month,currency_code
      ) |>
      dplyr::summarise(
        sum_value=sum(quantity,na.rm=TRUE)
        ,max_value=last(mtd_quantity),
        ,.groups="drop"
      ) |>
      dplyr::mutate(
        delta=sum_value-max_value
      ) |>
      dplyr::pull(delta)==0
    testthat::expect_true(
      all(mtd_validation)
    )
  }
)




test_that(
  "wtd - window_order logic",{

    wtd_validation <- sales |>
      group_by(currency_code) |>
      wtd(order_date,quantity,"standard") |>
      calculate() |>
      collect() |>
      arrange(date) |>
      dplyr::group_by(
        year,month,,week,currency_code
      ) |>
      dplyr::summarise(
        sum_value=sum(quantity,na.rm=TRUE)
        ,max_value=last(wtd_quantity),
        ,.groups="drop"
      ) |>
      dplyr::mutate(
        delta=sum_value-max_value
      ) |>
      dplyr::pull(delta)==0


    testthat::expect_true(
      all(wtd_validation)
    )
  }
)



test_that(
  "yoy - sum validation",{
   yoy_validation <-  sales |>
      group_by(currency_code) |>
      yoy(order_date,quantity,"standard") |>
      calculate() |>
      collect() |>
      arrange(year,currency_code) |>
      group_by(currency_code) |>
      mutate(
        lag_value=lead(yoy_quantity,1)
        ,delta=quantity-lag_value
      ) |>
      filter(
        !is.na(delta)
      ) |>
      dplyr::pull(delta)==0

    testthat::expect_true(
      all(yoy_validation)
    )
  }
)
