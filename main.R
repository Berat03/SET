library(tidyquant)
library(tidyverse)


AAPL  <- tq_get("AAPL", get = "stock.prices")


AAPL<- AAPL |>
  tq_mutate(mutate_fun = periodReturn, col_rename = 'return', period = "daily") |>
  select(date, close, return) |>
  mutate(cum_return = cumsum(return))

tq_mutate_fun_options()


ggplot(AAPL) +
  geom_line(aes(x = date, y = cum_return, fill ="blue"))

