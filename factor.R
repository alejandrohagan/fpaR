
#
# sales_tbl <- fpaR::sales |>
#   mutate(
#     month=month(order_date)
#   ) |>
#   group_by(
#     month
#     ,product_key
#   ) |>
#   summarize(
#     quantity=sum(quantity)
#     ,net_price=sum(net_price)
#     ,mean=mean(unit_price)
#     ,.groups="drop"
#   ) |>
#   group_by(month) |>
#   arrange(product_key,.by_group = TRUE) |>
#   ungroup() |>
#   mutate(
#     price_realization=net_price/quantity
#     # ,quantity_lead=lead(quantity,2517)
#     # ,net_price_lead=lead(quantity,2517)
#     # ,mean_price_lead=lead(mean,2517)
#     # ,price_realization_lead=lead(price_realization,2517)
#     # ,quantity_delta=lead(quantity,2517)-quantity
#     # ,net_price_delta=lead(quantity,2517)-net_price
#     # ,mean_delta=lead(mean,2517)-mean
#     # ,price_realization_delta=lead(price_realization,2517)-price_realization
#   ) |>
#   filter(
#     month %in% c(1,2)
#   )
#
# mod <- lm(net_price_delta~quantity_delta+price_realization_delta-1,sales_tbl |> filter(month==1))
#
#
# sales_wide_tbl <- sales_tbl |>
#   group_split(month) |>
#   map2(
#     .y = c("jan", "feb"),
#     .f = \(.x,.y) {
#       x <- .x  # Assign .x to x for clarity
#       y <- .y  # Assign .y to y for clarity
#
#       x |>
#         rename_with(
#           .fn = ~ glue::glue("{y}_{.x}"),  # Use y and .col explicitly
#           .cols = c(quantity, net_price, mean, price_realization)
#         ) |>
#         select(-month)
#     }
#   ) |>
#   reduce(
#     \(.x,prev){
#       .x |>
#         left_join(
#           prev
#           ,by=join_by(product_key)
#         )
#     }
#   ) |>
#   mutate(
#     quantity_delta=jan_quantity-feb_quantity
#     ,net_price_delta=jan_net_price-feb_net_price
#     ,mean_delta=jan_mean-feb_mean
#     ,pr_delta=jan_price_realization-feb_price_realization
#   )
#
#
# factor_tbl <- sales_wide_tbl |>
#   mutate(
#     vol=quantity_delta*jan_price_realization
#     ,price=pr_delta*feb_quantity
#     ,total_var=vol+price
#   ) |>
#   relocate(
#     vol
#     ,price
#     ,net_price_delta
#     ,total_var
#   ) |>
#   summarise(
#     vol=sum(vol,na.rm=TRUE)
#     ,price=sum(price,na.rm = TRUE)
#     ,net_price=sum(net_price_delta,na.rm = TRUE)
#     ,total_var=sum(total_var,na.rm=TRUE)
#     ,quantity=sum(quantity_delta,na.rm=TRUE)
#   )
#
#
# stats::model.frame()
#
# stats::terms()
#
# test <- stats::terms(out~inside+inner+outter)
#
#
#
# str(test)
#
# ## pull in columns
# variables_vec <- attr(test,"variables")
# target_vec <- as.character(variables_vec[[2]])
#
# input_vec <- attr(test,"term.labels")
#
# # extract the data base don columns
# model.frame(formula(mpg~cyl+am+vs),data=mtcars)
#
#
# factor_tbl
# sales_tbl |>
#   lm(net_price~0+quantity+mean,data=_)
#
#
# sales_wide_tbl |>
#   lm(net_price_delta~quantity_delta+jan_price_realization+pr_delta+feb_quantity+product_key-1,data=_) |>
#   # broom::tidy()
#   broom::augment()
