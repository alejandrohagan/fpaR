


test_that(
  "ytd - sum validation",{
    testthat::expect_true(
      all(
        sales |>
          ytd(order_date,quantity,"standard") |>
          calculate() |>
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
        sales |> ytd(order_date,quantity,"standard") |>
          calculate() |>
          dplyr::group_by(
            year
          ) |>
          dplyr::summarise(
            sum_value=sum(quantity,na.rm=TRUE)
            ,max_ytd=max(ytd_quantity),
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



## validated tests are above




test_that(
  "mtd - sum validation",{
    testthat::expect_true(
      all(
        sales |>
          mtd(order_date,quantity,"standard") |>
          calculate() |>
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
  "wtd - sum validation",{
    testthat::expect_true(
      all(
        sales |>
          wtd(order_date,quantity,"standard") |>
          calculate() |>
          dplyr::group_by(
            year,month,week
          ) |>
          dplyr::summarise(
            sum_value=sum(quantity,na.rm=TRUE)
            ,max_wtd=last(wtd_quantity),
            ,.groups="drop"
          ) |>
          dplyr::mutate(
            delta=sum_value-max_wtd
          ) |>
          dplyr::pull(delta)==0
      )
    )
  }
)




test_that(
  "atd - sum validation",{
    testthat::expect_true(
      all(
        sales |>
          atd(order_date,quantity,"standard") |>
          calculate() |>

          dplyr::summarise(
            sum_value=sum(quantity,na.rm=TRUE)
            ,max_atd=last(atd_quantity),
            ,.groups="drop"
          ) |>
          dplyr::mutate(
            delta=sum_value-max_atd
          ) |>
          dplyr::pull(delta)==0
      )
    )
  }
)



test_that("yoy - month success", {

  testthat::expect_no_error(
    fpaR::sales |> fpaR::yoy(date= order_date,value=quantity,lag_n=1,calendar_type="standard")
  )
})


test_that("yoy - S7 class", {

  testthat::expect_true(
    fpaR::sales |> fpaR::yoy(date = order_date,value=quantity,lag_n=1,calendar_type="standard") |>
      S7::S7_inherits()
    )
  }
)

test_that("yoy -- expand missing dates",{

  testthat::expect_true(
    all(
    x <- fpaR::sales |>
      filter(
        !year(order_date) %in% c("2016","2018")
      ) |>
      fpaR::yoy(date = order_date,value=quantity,lag_n=1,calendar_type="standard") |>
      calculate() |>
      pull(date)==c("2015-01-01", "2016-01-01", "2017-01-01", "2018-01-01" ,"2019-01-01", "2020-01-01", "2021-01-01", "2022-01-01" ,"2023-01-01","2024-01-01")
    )
  )
  }
)



test_that("yoy - calender_type fail ", {

  testthat::expect_error(
    fpaR::sales |> fpaR::yoy(date= order_date,value=quantity,lag_n=1,calendar_type="normal")
  )
})

