

calculate <- function(.data,.fn,var,rows,cols,filter){

  expr_fn=rlang::expr(.fn)
 filter_exp <- expr(filter)

.data %>%
    eval(filter_exp) %>%
    group_by(pick({{rows}}),pick({{cols}}))%>%
    summarise(out=eval(expr_fn)({{var}}))%>%
    pivot_wider(names_from={{cols}},values_from = out)


}


calculate(diamonds,.fn=min,var=price,rows=cut,cols=color,filter(.,color=="D",cut=="Fair"))

## way to have list or argument get seperately expressed and evaluated

test <- list(cut=="Fair",color=="D") %>% map(.,rlang::expr)

pluck(test,1)(1:10)

map(test,~pluck(.x)(500:1000))

eval(out)(1:10)

arg <- 'color=="D"'

diamonds %>%
  filter(eval(expr(color=="D"&cut=="Fair")))




full_out <- expr(filter(.,color=="D",cut=="Fair"))

diamonds %>%
eval(envir = .,full_out)
