
.fn="sum"
.data=diamonds

calculate <- function(.data,.fn,var,rows,cols,...){
var="price"
  expr_fn=rlang::expr(sum)

  .data %>%
    group_by(cut,color) %>%
    summarise(sum=eval(expr_fn)(!!sym(var)))


}



