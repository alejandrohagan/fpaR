
TOTALYTD(
  SUM(InternetSales_USD[SalesAmount_USD])
  ,DateTime[DateKey]
  , ALL('DateTime')
  , "6/30"
)

library(tidyverse)

totalytd <- function(.data,dim,date,fn,start_date,end_date){


  .data %>%
    filter(
      {{date}}>{{start_date}}
      ,{{date}}<{{end_date}}
           ) %>%
    group_by({{date}}) %>%
    summarise(sum=sum({{dim}})) %>%
    group_by(year=year(date)) %>%
    mutate(totalytd_=cumsum(sum)) %>%
    ungroup() %>%
    select(-c(year,sum))
}


df <- fpaR::contoso_fact_sales %>%
  mutate(date=mdy(DateKey))

df %>% totalytd(dim=SalesQuantity,date=date)

