library(tidyverse)
devtools::document()
devtools::load_all()

library(glue)


db <- fpaR::create_contonso_duckdb()

sales_db <- db$sales

con <- dbplyr::remote_con(sales_db)



make_aggregation_dbi(sales_db,customer_key,date_var=order_date,value_var=quantity,time_unit="month")




totalytd_sql(.data=sales_db,store_key,date_var=order_date,value_var=quantity)




dplyr::tbl(con,test_sql)

con <- dbplyr::remote_con(sales_db)

capture_original_query(sales_db)



    # Create two SQL objects
    sql1 <- sql("with t as (select * from")
    sql2 <- sql(paste("(",sql(remote_query(sales_db)),")"))
    sql3 <- sql(")")
    sql4 <- sql("select * from t limit 5")

    # Combine the SQL objects using paste0 or paste
    combined_sql <- paste(sql1, sql2,sql3,sql4)

    dplyr::tbl(con,dplyr::sql(combined_sql))

    DBI::dbGetQuery(con,dplyr::sql(combined_sql))

    tst_sql <- glue_sql("

  select *

  from
  {sql2}

  where
  true


    ",.con=con)


tst_sql <- sales_db %>%
  glue::glue_data_sql("
  select
  *

  from
  ({`dbplyr::ident_q((remote_query(.))`})

  where
  true


    ",.con=con
  )


out <- dbplyr::remote_query(sales_db)



tbl <- dbplyr::remote_query(sales_db)



tbl <- "sales"

dbplyr::ident(
  glue_sql("
  {sql({`tbl`})}
  ", .con = con
  ,tbl=tbl
  ,.literal = FALSE
  )
)


tbl <- "sales"

sub_query <- glue_sql("
  SELECT *
  FROM {`tbl`}
  ", .con = con)

glue_sql("
  SELECT s.{`value_var`}
  FROM ({sub_query}) AS s
  ", .con = con)




tbl(con,tst_sql)


# returns results

DBI::dbGetQuery(conn = con,tbl_sql)

test <- dplyr::tbl(con,sql(tbl_sql))

dplyr::tbl(con,sql(no_group_sql))

##
dbplyr::remote_query(sales_db)

dbplyr::sql_render(query = dbplyr::remote_query(sales_db),con=con)


test <- select_query(from=dbplyr::remote_query(sales_db),select=sql(tbl_sql))

# send statements

DBI::dbSendQuery(conn = con,group_sql)

# returns zero
DBI::dbExecute(conn = con,group_sql)


# similiar to dbsend query
DBI::dbSendStatement(conn = con,group_sql)

DBI::dbGetQuery(conn = con,group_sql)

DBI::Id()

dbplyr::build_sql(con=con,group_sql)

dbplyr::db_connection_describe(con)


## test tbl with cte

tbl(con,no_group_sql)

## maybe a way to pass a table path

dbplyr::as_table_path(group_sql,con)

dbplyr::sql_render(
  query = dbplyr::build_sql("select * from sales",con = con)
  ,con = con,sql_options =sql_options(cte = TRUE)
)




convert_dots_to_string(hello,how,are,you)


paste0("hello.",test_fn(hello,how,are,you))


library(rlang)

# Define some variables
x <- 10
y <- 20

# Capture an expression using expr()
my_expr <- expr(10+10+x+as.numeric("1"))

# Evaluate the expression using eval_tidy() in the current environment
eval(my_expr)

# Output the result
print(result)



my_call <- expr(mean(x = z, na.rm = FALSE))

# Modify the function call: change na.rm to TRUE
modified_call <- call_modify(my_call,x=x)

# Print the modified call
print(modified_call)

# You can then evaluate the modified call
z <- c(1, 2, 3, NA)
result <- eval(modified_call)
print(result)


value <- 10
# Capture an expression with a placeholder
my_expr <- expr(value + 5)

# Interpolate the value into the expression
interpolated_expr <- expr_interp(my_expr)

# Print the interpolated expression
print(interpolated_expr)

# Evaluate the interpolated expression
result <- eval(interpolated_expr)


library(rlang)
my_quo <- quo(x + y)
env <- new_environment(list(x = 3, y = 4))
result <- eval_tidy(my_quo, env)
print(result)


nested_quo <- quo(quo(x + y))
flat_quo <- quo_squash(nested_quo)
print(flat_quo)

text |> str()
my_quo <- quo(mean(x))
name <- quo_name(my_quo)
text <- quo_text(my_quo)
print(name)
print(text)


rlang::parse_quo("my_quo",env)
my_quo <- quo(mean(x, na.rm = TRUE))
expr <- quo_get_expr(my_quo)
env <- quo_get_env(my_quo)
print(expr)
print(env)


