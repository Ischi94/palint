library(tidyverse)

dat1 <- tibble(myvalue = c(1, 3, 6, 10, 15, 100, 1, 0, 0, 0), mybin = 1:length(myvalue))


long_term <- function(trend.length = 1){
  dat1 %>%
    mutate(trend.val = lag(myvalue, n = trend.length + 1),
           lag.trend.val = lead(trend.val),
           trend.bin = lag(mybin, n = trend.length + 1),
           lag.trend.bin = lead(trend.bin)) %>%
    group_by(mybin) %>%
    mutate(comb.val = list(c(trend.val, lag.trend.val)),
           comb.bin = list(c(trend.bin, lag.trend.bin))) %>%
    drop_na() %>%
    ungroup() %>%
    unnest(cols = c(comb.val, comb.bin)) %>%
    nest_by(mybin) %>%
    mutate(model = list(lm(comb.val ~ comb.bin, data = data))) %>%
    ungroup() %>%
    mutate(
      long_term = map(model, "coefficients"),
      long_term = map_dbl(long_term, pluck, 2)) %>%
    select(mybin, long_term) %>%
    full_join(dat1) %>%
    select(myvalue, mybin, long_term) %>%
    arrange(mybin)
}

long_term()


dat2 <- dat1 %>%
  mutate(trend.val = lag(myvalue, n = 2),
         lag.trend.val = lead(trend.val),
         trend.bin = lag(mybin, n = 2),
         lag.trend.bin = lead(trend.bin)) %>%
  group_by(mybin) %>%
  mutate(comb.val = list(c(trend.val, lag.trend.val)),
         comb.bin = list(c(trend.bin, lag.trend.bin))) %>%
  drop_na() %>%
  ungroup() %>%
  unnest(cols = c(comb.val, comb.bin)) %>%
  nest_by(mybin) %>%
  mutate(model = list(lm(comb.val ~ comb.bin, data = data))) %>%
  ungroup() %>%
  mutate(
    long_term = map(model, "coefficients"),
    long_term = map_dbl(long_term, pluck, 2)) %>%
  select(mybin, long_term) %>%
  full_join(dat1) %>%
  select(myvalue, mybin, long_term) %>%
  arrange(mybin)

dat2 %>%
  mutate(eff = purrr::map(model, summarise))

map_dbl(dat3, pluck, 2)
