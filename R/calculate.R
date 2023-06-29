

calculate <- function(.data,.fn,var,rows,cols,...){

  expr_fn=rlang::expr(.fn)
  expr_filter=rlang::expr(...)

  .data %>%
      filter(eval(expr_filter)) %>%
    group_by(pick({{rows}}),pick({{cols}}))%>%
    summarise(out=eval(expr_fn)({{var}}))%>%
    pivot_wider(names_from={{cols}},values_from = out)


}
calculate(diamonds,.fn=min,var=price,rows=c(cut,clarity),cols=color,color=="D",cut=="Fair")

## way to have list or argument get seperately expressed and evaluated

test <- list(sum,mean) %>% map(.,rlang::expr)

eval(pluck(test,1))(1:10)
test %>% purrr::list_flatten()[1]
out <- rlang::expr(min)

eval(out)(1:10)
