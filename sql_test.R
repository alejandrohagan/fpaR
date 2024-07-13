
#' Get formula elements and assign to respective lists
#'
#' @param ... formula object
#'
#' @return list
#'
#' @examples
#' make_formula_vars(y~x)
make_formula_vars <- function(...){

  # test input is formula
assertthat::assert_that(is_formula(...),msg = "Please insert a formula eg. y~x")

  # vars <- c("~","y","x+x1+x2")

  # capture as character

  vars <- as.character(...)

  # Assign formula elements to componets

  dependent_var <- vars[[2]]
  independent_var <-  stringr::str_split(string=vars[3],pattern = "\\+|\\*") |> unlist() |> str_squish()
  all_vars <- c(dependent_var,independent_var,recursive=TRUE)

  # create list of outputs

out_lst <-  list(
   independent_var=independent_var
   ,dependent_var=dependent_var
   ,all_vars=all_vars
 )

  # return lists

return(out_lst)

}




## make means tbl------------------

#' Make CTE of means of all variables
#'
#' @param all_vars character vector of all variables
#'
#' @return sql object
#'
#' @examples
#' make_means_sql(c("price","carat"))
make_means_cte <- function(all_vars){

  # opening CTE
  sql_1 <- dplyr::sql(
    "WITH means AS (

    SELECT \n"
  )

  # CTE main body

  sql_2 <- paste0("AVG(",all_var,") AS mean_",all_var,",\n ",collapse = "")

  # closing CTE
  sql_3 <- "
  from {.data}
  )"
  # combine it

  out_sql <- sql(paste0(sql_1,sql_2,sql_3))

  return(out_sql)

}


#' Make CTE of centered values
#'
#' @param all_vars character vector of all variables
#'
#' @return sql object
#'
#' @examples
make_centered_cte <- function(all_vars){

  # opening CTE
  sql_1 <- dplyr::sql(
    "WITH centered AS (

    SELECT \n"
  )

  # CTE main body

  sql_2 <- paste0(all_var," - means.mean_",all_var," AS y_",all_var,",\n ",collapse = "")

  # closing CTE
  sql_3 <- "
  from {.data}
  )"
  # combine it

  out_sql <- sql(paste0(sql_1,sql_2,sql_3))

  return(out_sql)

}



vars_lst <- make_formula_vars(price~carat+x+y+z)

### mean of each var
y <- "price"
x1 <- "carat"
x2 <- "x"
x3 <- "y"

make_means_cte(vars_lst$all_vars)
make_centered_cte(vars_lst$all_vars)


independent_var <- c(x1,x2,x3)
dependent_var <- c(y)
all_var <- c(y,x1,x2,x3)
con <- diamonds_db$src[[1]]
.data <- "diamonds_db"




crossing(x=vars_lst$all_vars,y=vars_lst$all_vars) |>
  filter(x!=y)

objects <- c("A", "B", "C", "D")

# Generate all possible pairs
pairs <- combn(vars_lst$all_vars, 2, simplify = FALSE)


cte_names_lst <- map2(pairs,1,\(x=.x,y=.y) paste0("res_",unlist(x)[[y]],"_",unlist(x)[y+1]))

first_arg_lst <- map2(pairs,1,\(x=.x,y=.y) paste0(unlist(x)[[y]]))

second_arg_lst <- map2(pairs,2,\(x=.x,y=.y) paste0(unlist(x)[[y]]))


sql_1 <- dplyr::sql(
  "WITH centered AS (

    SELECT \n"
)

sql_1 <- paste0("WITH ",cte_names_lst," AS ( \n SELECT \n",collapse= "")


# CTE main body --- turn this pmap() function

sql_2 <- paste0(first_arg_lst," - (",second_arg_lst,"* REGR_SLOPE(",first_arg_lst,",",second_arg_lst,") OVER()) AS ",cte_names_lst,",\n","ROW_NUMBER() OVER () AS row_num\n",collapse = "")

# closing CTE
sql_3 <- "
  from centered
  ),"


out_sql <- sql(paste0(sql_1,sql_2,sql_3))


"res_x3_x1 AS (
    SELECT
        x3 - (x1 * REGR_SLOPE(x3, x1) OVER()) AS resx3x1,
        ROW_NUMBER() OVER () AS row_num
    FROM centered
),"




## full sql

glue::glue_sql("

-- Step 2: Calculate the mean of each column
WITH means AS (
    SELECT
        AVG({`y`}) AS mean_y,
        AVG({`x1`}) AS mean_x1,
        AVG({`x2`}) AS mean_x2,
        AVG({`x3`}) AS mean_x3
    FROM {`.data`}
),

-- Step 3: Compute the centered values for y, x1, x2, and x3
centered AS (
    SELECT
        {`y`} - means.mean_y AS y,
        {`x1`} - means.mean_x1 AS x1,
        {`x2`} - means.mean_x2 AS x2,
        {`x3`} - means.mean_x3 AS x3
    FROM
        {`.data`}, means
),

res_x2_x1 AS (
    SELECT
        x2 - (x1 * REGR_SLOPE(x2, x1) OVER()) AS resx2x1,
        ROW_NUMBER() OVER () AS row_num
    FROM centered
),

res_y_x1 AS (
    SELECT
        y - (x1 * REGR_SLOPE(y, x1) OVER()) AS resyx1,
        ROW_NUMBER() OVER () AS row_num
    FROM centered
),

res_x3_x1 AS (
    SELECT
        x3 - (x1 * REGR_SLOPE(x3, x1) OVER()) AS resx3x1,
        ROW_NUMBER() OVER () AS row_num
    FROM centered
),

res_x1_x2 AS (
    SELECT
        x1 - (x2 * REGR_SLOPE(x1, x2) OVER()) AS resx1x2,
        ROW_NUMBER() OVER () AS row_num
    FROM centered
),

res_y_x2 AS (
    SELECT
        y - (x2 * REGR_SLOPE(y, x2) OVER()) AS resyx2,
        ROW_NUMBER() OVER () AS row_num
    FROM centered
),

res_x3_x2 AS (
    SELECT
        x3 - (x2 * REGR_SLOPE(x3, x2) OVER()) AS resx3x2,
        ROW_NUMBER() OVER () AS row_num
    FROM centered
),

res_x1_x3 AS (
    SELECT
        x1 - (x3 * REGR_SLOPE(x1, x3) OVER()) AS resx1x3,
        ROW_NUMBER() OVER () AS row_num
    FROM centered
),

res_y_x3 AS (
    SELECT
        y - (x3 * REGR_SLOPE(y, x3) OVER()) AS resyx3,
        ROW_NUMBER() OVER () AS row_num
    FROM centered
),

res_x2_x3 AS (
    SELECT
        x2 - (x3 * REGR_SLOPE(x2, x3) OVER()) AS resx2x3,
        ROW_NUMBER() OVER () AS row_num
    FROM centered
),

full_tbl AS (
    SELECT
        res_x2_x1.row_num,
        res_x2_x1.resx2x1,
        res_y_x1.resyx1,
        res_x3_x1.resx3x1,
        res_x1_x2.resx1x2,
        res_y_x2.resyx2,
        res_x3_x2.resx3x2,
        res_x1_x3.resx1x3,
        res_y_x3.resyx3,
        res_x2_x3.resx2x3
    FROM
        res_x2_x1
    JOIN
        res_y_x1 ON res_x2_x1.row_num = res_y_x1.row_num
    JOIN
        res_x3_x1 ON res_x2_x1.row_num = res_x3_x1.row_num
    JOIN
        res_x1_x2 ON res_x2_x1.row_num = res_x1_x2.row_num
    JOIN
        res_y_x2 ON res_x2_x1.row_num = res_y_x2.row_num
    JOIN
        res_x3_x2 ON res_x2_x1.row_num = res_x3_x2.row_num
    JOIN
        res_x1_x3 ON res_x2_x1.row_num = res_x1_x3.row_num
    JOIN
        res_y_x3 ON res_x2_x1.row_num = res_y_x3.row_num
    JOIN
        res_x2_x3 ON res_x2_x1.row_num = res_x2_x3.row_num
)

SELECT
    REGR_SLOPE(resyx1, resx2x1) AS coef_x2,
    REGR_SLOPE(resyx1, resx3x1) AS coef_x3,
    REGR_SLOPE(resyx2, resx1x2) AS coef_x1,
    REGR_INTERCEPT(resyx1, resx2x1) AS intercept
FROM full_tbl
")
