

TOTALYTD(
  SUM(InternetSales_USD[SalesAmount_USD])
  ,DateTime[DateKey]
  , ALL('DateTime')
  , "6/30"
  )
library(tidyverse)

totalytd
fpaR::contoso_fact_sales %>%
  mutate(DateKey=mdy(DateKey)) %>%
group_by(DateKey) %>%
  summarise(sum=sum(UnitCost)) %>%
  group_by(year=year(DateKey)) %>%
  mutate(totalytd=cumsum(sum)) %>%
  ungroup() %>%
  select(-c(year,sum))
