# need to split estimation and event time periods
# give user option to determine 
# this is a user input, # user to calculate/ determine time frame?
# add user functionality to add time frame later down the line!



library(tidyquant)
library(tidyverse)

event_date <- YMD('2020-02-27') 

# -Begin- Get relevant info & mutate DFs 
ticker_stock = "MSFT"
stock  <- tq_get(ticker_stock, get = "stock.prices", from = "2020-01-10", to = "2020-03-01", periodicity = "daily") |>
  tq_mutate(mutate_fun = periodReturn, col_rename = 'stock_return', period = "daily")  |>
  mutate(days_before = as.numeric(event_date - date)) |> # actual days, not trading days!!!
  select(symbol, date, adjusted, stock_return, days_before)

ticker_bench = "^GSPC" 
bench <- tq_get(ticker_bench, get = "stock.prices", from = "2020-01-10", to = "2020-3-01", periodicity = "daily") |>
  tq_mutate(mutate_fun = periodReturn, col_rename = 'bench_return', period = "daily") |>
  select(symbol, date, adjusted, bench_return)
# -End- Get relevant info & mutate DFs 



# -Begin- CAPM Calc 
RaRb <- left_join(stock, bench, by = c("date" = "date")) 

CAPM_table <- RaRb |>
  tq_performance(Ra = stock_return, Rb = bench_return, performance_fun = table.CAPM)
alpha <- CAPM_table$Alpha
beta <- CAPM_table$Beta
# -End- CAPM Calc 

abnormal_stock <- RaRb |>
  mutate(market_model_return = (alpha + beta * bench_return)) |> # no error term, rv
  mutate(abnormal_return = (stock_return - (alpha + beta * bench_return))) 
# -Begin- Graph Plots

#Should I pivot my data longer?? to enable better ggplot functionality?
ggplot(data = abnormal_stock, aes(x = days_before)) +
  geom_line(aes(y = stock_return, color = 'blue')) + 
  geom_line(aes(y = bench_return, color = 'red')) + 
  geom_line(aes(y = abnormal_return, color = 'yellow')) +
  geom_line(aes(y = market_model_return, color = 'green')) +
  labs(x = '', y = '') +
  geom_vline(aes(xintercept = 0, linetype =''), show.legend = FALSE) +
  scale_x_reverse()

# -End- Graph Plots



