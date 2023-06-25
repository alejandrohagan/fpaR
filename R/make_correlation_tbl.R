library(tidyverse)

make_corr
mtcars %>%
  timetk::tk_augment_differences(
    .value = everything()
    ,.lags = 1:10
    ,.differences = 1
    ,.log = TRUE
  ) %>%
  corrr::correlate() %>%
  corrr::correlate() %>%
  corrr::shave() %>% view()

