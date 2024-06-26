library(tidyverse)
devtools::load_all()
devtools::document()


dat |>
  group_by(
    date_key
  )

dat <- fpaR::contoso_fact_sales |>
  mutate(
    DateKey=mdy(DateKey)
  ) |>
  janitor::clean_names()

fpaR::calculate(dat,.fn = "sum",rows = "store_key",cols ="sales_quantity")

fpaR::calculate(ggplot2::diamonds,.fn="min(price)",rows=cut,cols=color,filter='color=="D"&cut=="Fair"')


compare <- function(.groups){

  tbl1 <- head(diamonds,10)
  tbl2 <- head(diamonds,30) %>% rename(cut2=cut,color2=color)


  con <- DBI::dbConnect(duckdb::duckdb())

  duckdb::duckdb_register(con,"t1",tbl1)
  duckdb::duckdb_register(con,"t2",tbl2)

  DBI::dbListTables(con)
  tbl1_db <- tbl(con,"t1")
  tbl2_db <- tbl(con,"t2")

  lookup_vec <- tibble(id=c("cut_id","color_id")
                       ,tbl1_col=c("cut","color")
                       ,tbl2_col=c("cut2","color2")
  )
  selected_vec <- filter(lookup_vec,id %in% .groups)




  temp_1 <- tbl1_db %>%
    group_by(pick(selected_vec$tbl1_col)) %>%
    summarize(mean3=mean(price)
              ,.groups = "drop")

  temp_2 <- tbl2_db %>%
    group_by(pick(selected_vec$tbl2_col)) %>%
    summarize(mean_2=mean(price),
              .groups = "drop")

  join_args <-   map2(
    .x=selected_vec$tbl1_col
    ,.y=selected_vec$tbl2_col
    ,~glue::glue("{.x}=={.y}")
  ) %>% str_flatten_comma()

  join_form <- paste0("join_by(",join_args,")")


  join_tbl <-  temp_1 %>%
    left_join(
      temp_2
      ,by = eval(parse_expr(join_form))
    )
  return(join_tbl)

}


compare(c("cut_id","color_id"))

price <- 10

jana <- 100

diamonds %>%
  mutate(
    test=cumsum(.data[["price"]])
    ,test3=cumsum(.data$price)
    ,test2=cumsum(.env$jana)
    ,test4=cumsum(.env[["price"]])

  )

con <- DBI::dbConnect(duckdb::duckdb())

duckdb::duckdb_register(con,"diamonds",diamonds)

diamonds_db <-
  dplyr::tbl(con,"diamonds")

diamonds_db |>
  count_plus(cut,wt=price)

diamonds |>
  count_plus(cut,wt=price)

