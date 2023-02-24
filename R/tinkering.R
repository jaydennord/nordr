library(tidyverse)
d <- mtcars %>%
  as_tibble(rownames = "car") %>%
  mutate(
    across(c(cyl, vs, am, gear, carb), factor)
  )

tbls <- d %>%
  select(-car) %>%
  describe()

library(flextable)
#
# tbls$categorical %>%
#   as_grouped_data("Variable") %>%
#   as.data.frame() %>%
#   flextable() %>%
#   merge_h(i = ~ !is.na(Variable)) %>%
#


tbls$categorical %>%
  flextable() %>%
  merge_v(j = 1) %>%
  valign(j = 1, valign = "top") %>%
  padding(i = ~ Variable != lag(Variable), padding.top = 20) %>%
  set_formatter(
    perc = scales::percent,
    valid_perc = scales::percent
  ) %>%
  fix_border_issues() %>%
  autofit()
