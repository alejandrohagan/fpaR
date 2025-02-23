library(tidyverse)
library(dbplyr)
library(glue)

con <- DBI::dbConnect(duckdb::duckdb("/home/hagan/database.duckdb"))

duckdb::duckdb_register(con,"diamonds",diamonds)

DBI::dbListTables(con)
DBI::dbDisconnect(con)
## linear modeling

y  <- "price"
x1 <- "x"
x2 <- "carat"
.data <- "diamonds"

var_1 <- list(c(y="price",x1="x",x2="carat",.data="diamonds"))
sql <-"
WITH means AS (
  SELECT
  AVG({`y`}) AS mean_y,
  AVG({`x1`}) AS mean_x1,
  AVG({`x2`}) AS mean_x2
  FROM {`.data`}

)"

glue_sql("
WITH means AS (
  SELECT
  AVG({`y`}) AS mean_y,
  AVG({`x1`}) AS mean_x1,
  AVG({`x2`}) AS mean_x2
  FROM {`.data`})"
,y="price"
,x1="x"
,x2="carat"
,.data="diamonds"
,.con=con)

map2(
  .x=sql
  ,.y=var_1
  ,\(x,y) glue::glue_sql(x,y,.con=con)
)

# lm

y <- iris$Sepal.Length - mean(iris$Sepal.Length)
x1 <- iris$Petal.Length - mean(iris$Petal.Length)
x2 <- iris$Sepal.Width - mean(iris$Sepal.Width)


resx2x1 <- resid(lm(x2 ~0+ x1))
resyx1 <- resid(lm(y ~0+ x1))
coef_x2 <- coef(lm(resyx1 ~0+ resx2x1))[["resx2x1"]]

resx1x2 <- resid(lm(x1 ~0+ x2))
resyx2 <- resid(lm(y ~0+ x2))
coef_x1 <- coef(lm(resyx2 ~ 0+resx1x2))[["resx1x2"]]

all.equal(c(coef_x1, coef_x2),
          coef(lm(y ~0+ x1 + x2))[c("x1", "x2")],
          check.attributes = FALSE)


y  <- "price"
x1 <- "x"
x2 <- "carat"
.data <- "diamonds"

test_lm <- glue::glue_sql("
-- Step 1: Create a table to hold the iris dataset

-- Step 2: Calculate the mean of each column
WITH means AS (
    SELECT
        AVG({`y`}) AS mean_y,
        AVG({`x1`}) AS mean_x1,
        AVG({`x2`}) AS mean_x2
    FROM {`.data`}

)

-- Step 3: Compute the centered values for Sepal.Length, Petal.Length, and Sepal.Width
, centered AS (
    SELECT
        {`y`} - means.mean_y AS y,
        {`x1`} - means.mean_x1 AS x1,
        {`x2`} - means.mean_x2 AS x2
    FROM
        {`.data`}, means
)

-- Step 4: Compute the residuals of the linear models using REGR_SLOPE
, res_x2_x1 AS (
    SELECT
        x2 - (x1 * REGR_SLOPE(x2, x1) OVER()) AS resx2x1
,ROW_NUMBER() OVER () AS row_num
    FROM centered
)
, res_y_x1 AS (
    SELECT
        y - (x1 * REGR_SLOPE(y, x1) OVER()) AS resyx1
,ROW_NUMBER() OVER () AS row_num
    FROM centered
)
, res_x1_x2 AS (
    SELECT
        x1 - (x2 * REGR_SLOPE(x1, x2) OVER()) AS resx1x2
,ROW_NUMBER() OVER () AS row_num
    FROM centered
)
, res_y_x2 AS (
    SELECT
        y - (x2 * REGR_SLOPE(y, x2) OVER()) AS resyx2
,ROW_NUMBER() OVER () AS row_num
    FROM centered
)

-- Step 5: Compute the coefficients from the residuals
SELECT
    REGR_SLOPE(resyx1, resx2x1) AS coef_x2,
    REGR_SLOPE(resyx2, resx1x2) AS coef_x1
FROM
    res_y_x1, res_x2_x1, res_y_x2, res_x1_x2
",
.con=con)



test_lm <- glue::glue_sql("


-- Step 2: Calculate the mean of each column
WITH means AS (
    SELECT
        AVG({`y`}) AS mean_y,
        AVG({`x1`}) AS mean_x1,
        AVG({`x2`}) AS mean_x2
    FROM {`.data`}

)

-- Step 3: Compute the centered values for Sepal.Length, Petal.Length, and Sepal.Width
, centered AS (
    SELECT
        {`y`} - means.mean_y AS y,
        {`x1`} - means.mean_x1 AS x1,
        {`x2`} - means.mean_x2 AS x2
    FROM
        {`.data`}, means
)
, res_x2_x1 AS (
    SELECT
        x2 - (x1 * REGR_SLOPE(x2, x1) OVER()) AS resx2x1
,ROW_NUMBER() OVER () AS row_num
    FROM centered
)
, res_y_x1 AS (
    SELECT
        y - (x1 * REGR_SLOPE(y, x1) OVER()) AS resyx1
,ROW_NUMBER() OVER () AS row_num
    FROM centered
)
, res_x1_x2 AS (
    SELECT
        x1 - (x2 * REGR_SLOPE(x1, x2) OVER()) AS resx1x2
,ROW_NUMBER() OVER () AS row_num
    FROM centered
)
, res_y_x2 AS (
    SELECT
        y - (x2 * REGR_SLOPE(y, x2) OVER()) AS resyx2
,ROW_NUMBER() OVER () AS row_num
    FROM centered
),

full_tbl as (

SELECT
    res_x2_x1.row_num
    ,res_x2_x1.resx2x1
    ,res_y_x1.resyx1
    ,res_x1_x2.resx1x2
    ,res_y_x2.resyx2

FROM
    res_x2_x1

JOIN
    res_y_x1 ON res_x2_x1.row_num = res_y_x1.row_num
JOIN
    res_x1_x2 ON res_x2_x1.row_num = res_x1_x2.row_num
JOIN
    res_y_x2 ON res_x2_x1.row_num = res_y_x2.row_num
)

select
    REGR_SLOPE(resyx1, resx2x1) AS coef_x2
    ,REGR_SLOPE(resyx2, resx1x2) AS coef_x1

from full_tbl

",
.con=con)

DBI::dbGetQuery(con,test_lm) |> as_tibble()

tbl(con,sql(test_lm)) |> as_tibble()

DBI::dbListTables(con)


test3_sql <- sql("

WITH means AS (
    SELECT
        AVG(price) AS mean_price,
        AVG(x) AS mean_x,
        AVG(carat) AS mean_carat
    FROM diamonds
),

sums AS (
    SELECT
        SUM((price - m.mean_price) * (x - m.mean_x)) AS S_yx,
        SUM((price - m.mean_price) * (carat - m.mean_carat)) AS S_ycarat,
        SUM((x - m.mean_x) * (x - m.mean_x)) AS S_xx,
        SUM((carat - m.mean_carat) * (carat - m.mean_carat)) AS S_caratcarat,
        SUM((x - m.mean_x) * (carat - m.mean_carat)) AS S_xcarat
    FROM diamonds, means m
),
coefs AS (
    SELECT
        (S_yx * S_caratcarat - S_ycarat * S_xcarat) / (S_xx * S_caratcarat - S_xcarat * S_xcarat) AS coef_x,
        (S_ycarat * S_xx - S_yx * S_xcarat) / (S_xx * S_caratcarat - S_xcarat * S_xcarat) AS coef_carat
    FROM sums
)
SELECT
    m.mean_price - c.coef_x * m.mean_x - c.coef_carat * m.mean_carat AS intercept,
    c.coef_x,
    c.coef_carat
FROM means m, coefs c

")




tbl(con,sql(test3_sql))



lm(price~x+carat,data=diamonds)


generate_means_sql <- function(table, dep_var, indep_vars) {
  # Combine the dependent and independent variable names into a single vector
  vars <- c(dep_var, indep_vars)

  # Create a SQL fragment for each variable using purrr::map_chr
  cols_sql <- map_chr(vars, ~ glue("AVG({.x}) AS mean_{.x}"))

  # Collapse the fragments into one comma-separated string
  cols_sql <- paste(cols_sql, collapse = ",\n    ")

  # Construct the full SQL query using glue
  query <- glue("SELECT
    {cols_sql}
FROM {table}")

  # Return the query wrapped in sql_ql() (assuming sql_ql is defined in your workflow)
  return(query)
}

generate_means_sql("diamonds", "price", c("x", "carat","y","z"))


library(glue)
library(purrr)

generate_sums_sql <- function(table, dep_var, indep_vars) {
  # Create expressions for the dependent variable with each independent variable
  dep_indep_exprs <- map_chr(indep_vars, ~ glue(
    "SUM(({dep_var} - m.mean_{dep_var}) * ({.x} - m.mean_{.x})) AS S_{dep_var}{.x}"
  ))

  # Create expressions for each independent variable's variance
  indep_var_exprs <- map_chr(indep_vars, ~ glue(
    "SUM(({.x} - m.mean_{.x}) * ({.x} - m.mean_{.x})) AS S_{.x}{.x}"
  ))

  # For every unique pair of independent variables, compute their covariance
  comb_exprs <- character(0)
  if (length(indep_vars) > 1) {
    comb_exprs <- combn(indep_vars, 2, simplify = FALSE, FUN = function(pair) {
      glue("SUM(({pair[1]} - m.mean_{pair[1]}) * ({pair[2]} - m.mean_{pair[2]})) AS S_{pair[1]}{pair[2]}")
    }) %>% unlist()
  }

  # Combine all expressions into one SQL fragment (comma-separated)
  all_exprs <- c(dep_indep_exprs, indep_var_exprs, comb_exprs)
  exprs_sql <- paste(all_exprs, collapse = ",\n    ")

  # Construct the final query; note that we join the 'means' CTE/table
  query <- glue("SELECT
    {exprs_sql}
FROM {table}, means m")

  # Wrap it in sql_ql() (assuming that's your function for executing SQL)
  return(query)
}

# Example usage:
# This call generates SQL that computes sums-of-products for:
#   Dependent: price
#   Independent: x and carat
sql_query <- generate_sums_sql("diamonds", "price", c("x", "carat","z","y"))

library(glue)
library(purrr)
library(glue)
library(purrr)

# Helper: for a given vector of independent variables (in order),
# construct the symbolic (X′X) matrix.
construct_matrix <- function(vars) {
  n <- length(vars)
  mat <- matrix("", n, n)
  for(i in seq_len(n)) {
    for(j in seq_len(n)) {
      if(i <= j) {
        mat[i, j] <- glue("S_{vars[i]}{vars[j]}")
      } else {
        mat[i, j] <- glue("S_{vars[j]}{vars[i]}")
      }
    }
  }
  return(mat)
}

# Recursive function to compute the determinant of a symbolic matrix.
determinant_expr_matrix <- function(mat) {
  n <- nrow(mat)
  if(n == 1) {
    return(mat[1,1])
  } else {
    terms <- map_chr(seq_len(n), function(j) {
      sign <- ifelse((1 + j) %% 2 == 0, "", "-")
      # Remove first row and j-th column to form the submatrix.
      submat <- mat[-1, -j, drop = FALSE]
      term <- glue("({mat[1, j]} * ({determinant_expr_matrix(submat)}))")
      paste0(sign, term)
    })
    # Join the terms with " + "
    paste(terms, collapse = " + ")
  }
}

# Helper: construct a matrix like M but with column 'col_replace'
# replaced by the vector of dependent covariances.
construct_replaced_matrix <- function(vars, dep, col_replace) {
  n <- length(vars)
  mat <- matrix("", n, n)
  for(i in seq_len(n)) {
    for(j in seq_len(n)) {
      if(j == col_replace) {
        # replaced column: use the covariance between dep and the regressor in row i.
        mat[i, j] <- glue("S_{dep}{vars[i]}")
      } else {
        if(i <= j) {
          mat[i, j] <- glue("S_{vars[i]}{vars[j]}")
        } else {
          mat[i, j] <- glue("S_{vars[j]}{vars[i]}")
        }
      }
    }
  }
  return(mat)
}

# Main function to generate the SQL snippet for coefficients
generate_coefs_sql_generic <- function(dep_var, indep_vars) {
  # Build the main (X′X) matrix.
  M <- construct_matrix(indep_vars)
  det_M <- determinant_expr_matrix(M)

  # For each independent variable, build the column-replaced matrix and compute its determinant.
  coefs_exprs <- map2_chr(seq_along(indep_vars), indep_vars, function(idx, var) {
    M_replace <- construct_replaced_matrix(indep_vars, dep_var, idx)
    det_replace <- determinant_expr_matrix(M_replace)
    glue("({det_replace})/({det_M}) AS coef_{var}")
  })

  # Combine into a SQL snippet (wrapped in a CTE named "coefs")
  coefs_sql <- glue("coefs AS (
  SELECT
    {paste(coefs_exprs, collapse = ",\n    ")}
  FROM sums
)")

  # Wrap with your SQL function (assumed to be sql_ql)
  return(coefs_sql)
}

# Example usage:
# For a regression of 'price' on independent variables 'x', 'carat', and 'depth'
sql_query <- generate_coefs_sql_generic("price", c("x", "carat", "depth"))
cat(sql_query)


library(glue)
library(purrr)

# Helper: Build a nested residualization expression.
# Given an initial expression and a vector of variables to partial out,
# it returns an expression like:
#   ((initial_expr - var1 * regr_slope(initial_expr, var1) OVER ())
#      - var2 * regr_slope(..., var2) OVER ()) ...
build_residual_expr <- function(initial_expr, others) {
  reduce(others, .init = initial_expr, .f = function(acc, other) {
    glue("({acc} - {other} * regr_slope({acc}, {other}) OVER ())")
  })
}

# Main function: Generate SQL code that computes the regression coefficients
# via nested residualization using regr_slope() for an arbitrary number of regressors.
# It assumes that a CTE (or table) named "centered" exists with the centered values of:
#   - The dependent variable (dep_var)
#   - Each independent variable (as provided in indep_vars)
generate_regr_slope_coefs_sql_multi <- function(dep_var, indep_vars) {
  # For each independent variable v, the "other" regressors are those in indep_vars excluding v.
  coef_exprs <- map(indep_vars, function(v) {
    others <- setdiff(indep_vars, v)
    # Generate a nested residual expression for the dependent variable
    # after partialling out all variables in 'others'.
    res_y_expr <- build_residual_expr(dep_var, others)
    # Similarly, generate a residual expression for the regressor v.
    res_v_expr <- build_residual_expr(v, others)
    # The coefficient for v is then the slope from regressing the residualized y on residualized v.
    glue("regr_slope({res_y_expr}, {res_v_expr}) AS coef_{v}")
  })

  # Combine the coefficient expressions into a SELECT statement from the "centered" data.
  coefs_sql <- glue("coefs AS (
    SELECT
      {paste(coef_exprs, collapse = ',\n      ')}
    FROM centered
)")

  # Wrap with your SQL execution function (assumed here to be sql_ql)
  return(coefs_sql)
}

# Example usage:
# For a regression of 'price' on independent variables 'x', 'carat', and 'depth',
# this generates SQL that sequentially residualizes out the effects of the other regressors.
sql_query <- generate_regr_slope_coefs_sql_multi("price", c("x", "carat", "depth"))
cat(sql_query)


## stuck here
coefs AS (
  SELECT
  -- Coefficient for x: regress the residualized y on the residualized x,
  -- where the residuals are computed by partialing out carat.
  regr_slope(
    y - carat * regr_slope(y, carat) OVER (),
    x - carat * regr_slope(x, carat) OVER ()
  ) AS coef_x,

  -- Coefficient for carat: regress the residualized y on the residualized carat,
  -- where the residuals are computed by partialing out x.
  regr_slope(
    y - x * regr_slope(y, x) OVER (),
    carat - x * regr_slope(carat, x) OVER ()
  ) AS coef_carat
  FROM centered
