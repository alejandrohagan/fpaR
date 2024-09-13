
library(tidyverse)

devtools::load_all()
Maybe <- function(value = NULL) {
  list(
    value = value,
    is_nothing = is.null(value) || is.na(value),
    bind = function(func) {
      if (is.null(value) || is.na(value)) {
        return(Maybe(NULL))
      } else {
        return(func(value))
      }
    },
    get_value = function() {
      if (is.null(value) || is.na(value)) {
        return(NULL)
      } else {
        return(value)
      }
    }
  )
}

# Example functions to use with Maybe
add_one <- function(x) Maybe(x + 1)
square <- function(x) Maybe(x * x)



factor(quantity)$aggregate(sum())$period(current_period())


factor(target=revenue,v=quantity,p=net_price)

factor(revenue)$pp(quantity)$multiply()$d(price_realization)


factor(
  revenue~
      vol(vol     ~ pp(quantity)          * d(price_realization) )+
    price(price   ~ d(quantity)           * cp(price_realization) )+
     mix1(mix1    ~ cp(quantity,sum))     * pp(mix)-cp(quantity))+
     mix2(mix2    ~ pp(price_realization) -  pp(price_realization,mean))+
      mix(mix     ~ mix1                  * mix2)
,time_unit="month"
)

sales_tbl |>
  fpaR::mom(date_var = order_date,value_var = quantity) |> tail()

str(formula_vec)
attr(formula_vec,"term.label")[[1]]
attr(formula_vec,"variables")
attr(formula_vec,"factors")

formula_vec[[2]][3]

price=pr_delta*jan_quantity
,vol=quantity_delta*feb_price_realization
,mix_vol=(sum(feb_quantity)*jan_mix)-feb_quantity
,mix_price=jan_price_realization-mean(jan_price_realization)
,mix=mix_vol*mix_price

# Creating a Maybe monad with an initial value
result <- Maybe(5)

Maybe(5)$bind(add_one)$get_value()
result$
  # Chaining operations
  result <- result$bind(add_one) # Maybe(6)
result <- result$bind(square)  # Maybe(36)

# Retrieving the final value
final_value <- result$get_value() # 36

print(final_value) # Output: 36



## summarize dataframe

sales_tbl <- fpaR::sales |>
  mutate(
    month=month(order_date)
  ) |>
  group_by(
    month
    ,order_date
    ,product_key
  ) |>
  summarize(
    quantity=sum(quantity)
    ,net_price=sum(net_price)
    ,mean=mean(unit_price)
    ,.groups="drop"
  ) |>
  group_by(month) |>
  arrange(product_key,.by_group = TRUE) |>
  ungroup() |>
  mutate(
    price_realization=net_price/quantity
    ,mix=net_price/sum(net_price)
    # ,quantity_lead=lead(quantity,2517)
    # ,net_price_lead=lead(quantity,2517)
    # ,mean_price_lead=lead(mean,2517)
    # ,price_realization_lead=lead(price_realization,2517)
    # ,quantity_delta=lead(quantity,2517)-quantity
    # ,net_price_delta=lead(quantity,2517)-net_price
    # ,mean_delta=lead(mean,2517)-mean
    # ,price_realization_delta=lead(price_realization,2517)-price_realization
  ) |>
  filter(
    month %in% c(1,2)
  )


sales_tbl

mod <- lm(net_price_delta~quantity_delta+price_realization_delta-1,sales_tbl |> filter(month==1))


sales_wide_tbl <- sales_tbl |>
  group_split(month) |>
  map2(
    .y = c("jan", "feb"),
    .f = \(.x,.y) {
      x <- .x  # Assign .x to x for clarity
      y <- .y  # Assign .y to y for clarity

      x |>
        rename_with(
          .fn = ~ glue::glue("{y}_{.x}"),  # Use y and .col explicitly
          .cols = c(quantity, net_price, mean, price_realization,mix)
        ) |>
        select(-month)
    }
  ) |>
  reduce(
    \(.x,prev){
      .x |>
        left_join(
          prev
          ,by=join_by(product_key)
        )
    }
  ) |>
  mutate(
    quantity_delta=jan_quantity-feb_quantity
    ,net_price_delta=jan_net_price-feb_net_price
    ,mean_delta=jan_mean-feb_mean
    ,pr_delta=jan_price_realization-feb_price_realization
  )


## Methodology 1
## https://www.fpandaclub.com/handson/becoming-the-master-of-factor-analysis-of-profit-the-secrets-of-calculation

sales_wide_tbl |>
  # head(10) |>
  mutate(
    total_vol=sum(feb_quantity,na.rm = TRUE)
    ,jan_mix=jan_quantity/sum(jan_quantity)
    ,feb_mix=feb_quantity/sum(feb_quantity)
    ,vol_reclac=(total_vol*jan_mix)
    ,vol_effect=(vol_reclac*jan_price_realization)
    ,vol=vol_effect-jan_net_price
    ,mix_effect=(feb_quantity*jan_price_realization)
    ,mix=mix_effect-vol_effect
    ,price_effect=(feb_price_realization*feb_quantity)
    ,price=price_effect-mix_effect
  ) |>
  relocate(vol,mix,price,pr_delta,total_vol,feb_mix,vol_reclac) |>
  summarise(
    vol=sum(vol)
    ,mix=sum(mix,na.rm=TRUE)
    ,price=sum(price,na.rm=TRUE)
    ,total_var=sum(net_price_delta,na.rm = TRUE)
  ) |>
  rowwise() |>
  mutate(
    total_fct=sum(c_across(c(-vol,-mix,-price)),na.rm = TRUE)
  )

## methodlogy 2
### https://zebrabi.com/price-volume-mix-analysis-power-bi/
### https://www.fticonsulting.com/insights/white-papers/quantifiable-approach-price-volume-mix-analysis
###   https://www.fpandhey.com/how-to-explain-revenue-performance-using-volume-mix-and-price-vmap/
sales_wide_tbl |>
  # head(10) |>
  mutate(
    price=pr_delta*jan_quantity
    ,vol=quantity_delta*feb_price_realization
    ,jan_mix=jan_quantity/sum(jan_quantity)
    ,feb_mix=feb_quantity/sum(feb_quantity)
    ,mix_vol=(sum(feb_quantity)*jan_mix)-feb_quantity
    ,mix_price=jan_price_realization-mean(jan_price_realization)
    ,mix=mix_vol*mix_price
  ) |>
  summarise(
    vol=sum(vol,na.rm=TRUE)
    ,mix=sum(mix,na.rm=TRUE)
    ,price=sum(price,na.rm=TRUE)
    ,total_var=sum(net_price_delta,na.rm = TRUE)
  ) |>
  rowwise() |>
  mutate(
    total_fct=sum(c_across(c(-vol,-mix,-price)),na.rm = TRUE)
  )


terms_vec <-   stats::terms(net_price ~
                              pp(quantity)*d(price_realization)+
                              d(quantity)*mean(cp(price_realization))



)

time_vars <- c("pp","cp","diff")
agg_vars <- c("sum","mean","row")


terms_vec

labels_vec <- attr(terms_vec,"term.label")

labels_vec[1]


input_string <- "t1(pp(quantity) * d(price_realization))"

# Regular expression pattern to match function calls with their arguments
pattern <- "\\b\\w+\\([^()]+\\)"

# Find all matches of the pattern in the input string
matches <- gregexpr(pattern, input_string)

# Extract the matches from the input string
extracted <- regmatches(input_string, matches)


library(rlang)

# Step 1: Create the call object for the mean function with arguments
out <- rlang::call2("mean", 1:10)

# Step 2: Evaluate the call object using eval_bare
rlang::eval_bare(out)

### methodology 3 reddit

sales_wide_tbl |>
  head(10) |>
  mutate(
    jan_overall_pr=sum(jan_net_price)/sum(jan_quantity)
    ,vol=quantity_delta*jan_overall_pr
    ,price=pr_delta*sum(feb_quantity)
  ) |>
  summarise(
    vol=sum(vol,na.rm=TRUE)
    ,price=sum(price,na.rm=TRUE)
    ,total_var=sum(net_price_delta,na.rm = TRUE)
  ) |>
  rowwise() |>
  mutate(
    total_fct=sum(c_across(c(-vol,-price)),na.rm = TRUE)
  )

## formula approach


terms_vec <-   stats::terms(net_price~
                              v(pp(quantity)*sum(cp(price_realization)))+
                              p(v()*cp(price_realization))+
                              m(mix_factor)+
                              d(date_col)
                            ,time_unit="month"
                            ,discontinued=TRUE
                            ,new_products=TRUE
                            )






data |>
  factor(
    target=net_price
    ,volume=quantity
    ,price=price_realization
    ,mix=mix_factor
    ,date=date_col
    ,time_unit="month"
    ,discontinued=TRUE
    ,new_products=TRUE)




factor_tbl <- sales_wide_tbl |>
  mutate(
    vol=sum(quantity_delta)*jan_mix*jan_price_realization
    ,price=pr_delta*feb_quantity
    ,total_var=vol+price
  ) |>
  relocate(
    vol
    ,price
    ,net_price_delta
    ,total_var
  ) |>
  summarise(
    vol=sum(vol,na.rm=TRUE)
    ,price=sum(price,na.rm = TRUE)
    ,net_price=sum(net_price_delta,na.rm = TRUE)
    ,total_var=sum(total_var,na.rm=TRUE)
    ,quantity=sum(quantity_delta,na.rm=TRUE)
  )


stats::model.frame()

stats::terms()

test <- stats::terms(out~inside+inner+outter)



str(test)

## pull in columns
variables_vec <- attr(test,"variables")
target_vec <- as.character(variables_vec[[2]])

input_vec <- attr(test,"term.labels")

# extract the data base don columns
model.frame(formula(mpg~cyl+am+vs),data=mtcars)


factor_tbl
sales_tbl |>
  lm(net_price~0+quantity+mean,data=_)


sales_wide_tbl |>
  lm(net_price_delta~quantity_delta+jan_price_realization+pr_delta+feb_quantity+product_key-1,data=_) |>
  # broom::tidy()
  broom::augment()






