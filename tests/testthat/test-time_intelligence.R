# load arguments / data

library(dplyr)
library(rlang)
library(lubridate)
library(tidyr)



## create calendar fills missing

test_that("create calendar fills in missing periods",{

 new_tbl <-  sales |>
   group_by(currency_code) |>
    mutate(
      year=year(order_date)
    ) |>
    filter(
      TRUE
      ,!year %in% c(2022,2023)
    ) |>
    yoy(order_date,margin,"standard",1) |>
    calculate() |>
    arrange(date) |>
   collect()

 validation_vec <- new_tbl |> pull(year) %in% c(2022,2023) |> sum()

  testthat::expect_true(
   validation_vec==10
  )}

 )


## correct aggregation when unequal periods

test_that("period imabalance validation -- month",{

  pm_mtd_tbl <- sales |>
    group_by(currency_code) |>
    mtd(order_date,margin,"standard") |>
    calculate() |>
    filter(
      TRUE
      ,month(date) %in% c(1)
      ,year(date) %in% c(2022)
    ) |>
    arrange(date)


  cm_pmtd_tbl <- sales |>
    group_by(currency_code) |>
    pmtd(order_date,margin,"standard",1) |>
    calculate() |>
    filter(
      TRUE
      ,month(date) %in% c(2)
      ,year(date) %in% c(2022)
    ) |>
    arrange(date)


pm_mtd_vec <-   pm_mtd_tbl |>
    filter(
      date=="2022-01-27"
    ) |>
    pull(mtd_margin) |> sum()


cm_pmtd_vec <-   cm_pmtd_tbl |>
  filter(
    date=="2022-02-27"
  ) |>
  pull(pmtd_margin) |> sum()

pm_mtd_rem_vec <-   pm_mtd_tbl |>
  filter(
    month==1
    ,year==2022
    ,day(date)==31
  ) |>
  pull(mtd_margin) |> sum()

cm_pmtd_rem_vec <-   cm_pmtd_tbl |>
    filter(
      month==2
      ,year==2022
      ,day(date)==28
    ) |>
    pull(pmtd_margin) |> sum()

testthat::expect_equal(
  cm_pmtd_vec,pm_mtd_vec
)

testthat::expect_equal(
  pm_mtd_rem_vec,cm_pmtd_rem_vec
)

})



test_that("period imabalance validation -- year",{

  pm_ytd_tbl <- sales |>
    group_by(currency_code) |>
    ytd(order_date,margin,"standard") |>
    calculate() |>
    filter(
      TRUE
      ,month(date) %in% c(2)
      ,year(date) %in% c(2022)
    ) |>
    arrange(date)


  cm_pytd_tbl <- sales |>
    group_by(currency_code) |>
    pytd(order_date,margin,"standard",1) |>
    calculate() |>
    filter(
      TRUE
      ,month(date) %in% c(2)
      ,year(date) %in% c(2023)
    ) |>
    arrange(date)


  pm_ytd_vec <-   pm_ytd_tbl |>
    filter(
      date=="2022-02-27"
    ) |>
    pull(ytd_margin) |> sum()


  cm_pytd_vec <-   cm_pytd_tbl |>
    filter(
      date=="2023-02-27"
    ) |>
    pull(pytd_margin) |> sum()

  pm_ytd_rem_vec <-   pm_ytd_tbl |>
    filter(
      date>"2022-02-27"
    ) |>

    pull(ytd_margin) |> sum(... = _,na.rm=TRUE)

  cm_pytd_rem_vec <-   cm_pytd_tbl |>
    filter(
      date>"2023-02-27"
    ) |>
    pull(pytd_margin) |> sum(...=_,na.rm=TRUE)

  testthat::expect_true(
    cm_pytd_vec==pm_ytd_vec
  )

  testthat::expect_true(
    pm_ytd_rem_vec==cm_pytd_rem_vec
  )

})

test_that("period imabalance validation -- quarter",{

  pm_qtd_tbl <- sales |>
    # group_by(currency_code) |>
    qtd(order_date,margin,"standard") |>
    calculate() |>
    filter(
      TRUE
      ,month(date) %in% c(1)
      ,year(date) %in% c(2022)
    ) |>
    arrange(date)


  cm_pqtd_tbl <- sales |>
    # group_by(currency_code) |>
    pqtd(order_date,margin,"standard",1) |>
    calculate() |>
    filter(
      TRUE
      ,month(date) %in% c(4)
      ,year(date) %in% c(2022)
    ) |>
    arrange(date)


  pm_qtd_vec <-   pm_qtd_tbl |>
    filter(
      date=="2022-01-27"
    ) |>
    pull(qtd_margin) |> sum()


  cm_pqtd_vec <-   cm_pqtd_tbl |>
    filter(
      date=="2022-04-27"
    ) |>
    pull(pqtd_margin) |> sum()

  pm_qtd_rem_vec <-   pm_qtd_tbl |>
    filter(
      date>"2022-01-27"
    ) |>

    pull(qtd_margin) |> sum(... = _,na.rm=TRUE)

  cm_pqtd_rem_vec <-   cm_pqtd_tbl |>
    filter(
      date>"2022-04-27"
    ) |>
    pull(pqtd_margin) |> sum(...=_,na.rm=TRUE)

  testthat::expect_true(
    cm_pqtd_vec==pm_qtd_vec
  )

  testthat::expect_true(
    pm_qtd_rem_vec==cm_pqtd_rem_vec
  )

})



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

    ytd_tbl <-  sales |>
      group_by(currency_code)  |>
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
