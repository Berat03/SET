library(tidyquant)
library(tidyverse)

# need to split estimation and event period (+- 2 days?)

AAPL  <- tq_get("AAPL", get = "stock.prices", from = "2021-01-01")
SP500 <- tq_get("^GSPC", get = "stock.prices", from = "2021-01-01")


AAPL<- AAPL |> 
  tq_mutate(mutate_fun = periodReturn, col_rename = 'return', period = "daily") |>
  select(date, close, return) |>
  mutate(cum_return = cumsum(return))


SP500<- SP500 |>
  tq_mutate(mutate_fun = periodReturn, col_rename = 'return', period = "daily") |>
  select(date, close, return) |>
  mutate(cum_return = cumsum(return))



ggplot(AAPL) +
  geom_line(aes(x = date, y = cum_return, color ="red"))+
  geom_line(data = SP500, aes(x = date, y = cum_return, color ="blue"))


