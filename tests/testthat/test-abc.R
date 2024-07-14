test_that("abc func n works", {

  testthat::expect_success(
    testthat::expect_type(ggplot2::diamonds |>abc(cut,dim = price,a = .7,b=.2,c=.1,func = "n") ,"list")
    )

  })


test_that("abc func sum works", {

  testthat::expect_success(
    testthat::expect_type(ggplot2::diamonds |>abc(cut,dim = price,a = .7,b=.2,c=.1,func = "sum") ,"list")
  )

})


test_that("assert checks work", {

  testthat::expect_error(
    testthat::expect_type(ggplot2::diamonds |>abc(cut,dim = price,a = .7,b=.3,c=.1,func = "sum") ,"list")
  )

})


test_that("assert checks work", {

  testthat::expect_error(

    ggplot2::diamonds |>abc(cut,dim = price,a = .7,b=.3,c=.1,func = "sum")

  )

})


