# need to split estimation and event time periods
  # give user option to determine 
library(tidyquant)
library(tidyverse)

stock  <- tq_get("MSFT", get = "stock.prices", from = "2019-11-01", to = "2020-01-01", periodicity = "daily") |>
  tq_mutate(mutate_fun = periodReturn, col_rename = 'stock_return', period = "daily")  |>
  select(symbol, date, adjusted, stock_return)
#mutate(stock_cum_return = cumsum(stock_return))

bench <- tq_get("^GSPC", get = "stock.prices", from = "2019-11-01", to = "2020-01-01", periodicity = "daily") |>
  tq_mutate(mutate_fun = periodReturn, col_rename = 'bench_return', period = "daily") |>
  select(symbol, date, adjusted, bench_return)


#mutate(bench_cum_return = cumsum(bench_return))

#  for market anaylsis

RaRb <- left_join(stock, bench, by = c("date" = "date")) 

CAPM_table <- RaRb |>
  tq_performance(Ra = stock_return, Rb = bench_return, performance_fun = table.CAPM)

alpha <- CAPM_table$Alpha
beta <- CAPM_table$Beta

rm(RaRb)
abnormal_stock <- abnormal_stock |>
  mutate(market_model_return = (alpha + beta * bench_return)) |> # no error term, rv
  mutate(abnormal_return = (stock_return - (alpha + beta * bench_return))) 


ggplot(data = abnormal_stock, aes(x = date)) +
  geom_line(aes(y = stock_return, color = 'blue')) + 
  geom_line(aes(y = bench_return, color = 'red')) + 
  geom_line(aes(y = abnormal_return, color = 'yellow')) +
  geom_line(aes(y = market_model_return, color = 'green')) +
  labs(x = '', y = 'Stock Metrics')



