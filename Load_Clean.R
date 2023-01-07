library(tidyverse)
library(tidyquant)
rm(list=ls()) 

# - Begin - User Inputs
ticker_stock <- "ATVI" 
ticker_bench = "^GSPC" 
event_date <- YMD('2018-11-5') 
# - End - User Inputs

begin <-event_date - as.difftime(200, unit="days") 
end <-event_date + as.difftime(30, unit="days") 

# - Begin - Load data
stock <- tq_get(ticker_stock, get = "stock.prices", from = begin, to = end, periodicity = "daily") |>
  tq_mutate(mutate_fun = periodReturn, col_rename = 'stock_return', period = "daily")  |>
  select(symbol, date, stock_adj = adjusted, stock_return) |>
  arrange(date) |>
  slice(-1)

bench <- tq_get(ticker_bench, get = "stock.prices", from = begin, to = end, periodicity = "daily") |>
  tq_mutate(mutate_fun = periodReturn, col_rename = 'bench_return', period = "daily") |>
  select(symbol, date, bench_adj = adjusted, bench_return) |>
  arrange(date) |>
  slice(-1)
# - End - Load data

# - Begin - Mutate data
abnormal_returns <- left_join(stock, bench, by = c("date" = "date")) |>
  mutate(ID = row_number()) 
eventID <- which(abnormal_returns$date == event_date, arr.ind=TRUE)

abnormal_returns <- abnormal_returns |>
  mutate(dates_relative = -1 *(ID - eventID)) |>
  mutate(time_period = ifelse(dates_relative >= 11, "EST", ifelse(dates_relative <= 10 & dates_relative >= 1, "ANT", 
                                                                  ifelse(date == event_date, "EVENT", ifelse(dates_relative < 0, "ADJ", NA))))) |>
  filter(dates_relative <= 100 & dates_relative >= -10) |> # 100 is est + adj, 10 is adj/est, 0 dates event 
  select(date, dates_relative, time_period, stock_adj, bench_adj, stock_return, bench_return) 

EST <-abnormal_returns[abnormal_returns$time_period == "EST",]
average_stock_returns_est<- mean(EST$stock_return)
CAPM_table <- EST |>
  tq_performance(Ra = stock_return, Rb = bench_return, performance_fun = table.CAPM)
alpha <- CAPM_table$Alpha
beta <- CAPM_table$Beta 

abnormal_returns <- abnormal_returns |>
  select(date,dates_relative, time_period,stock_adj, bench_adj, stock_return, bench_return,  ) |>
  mutate(constant_return = stock_return - average_stock_returns_est) |>
  mutate(market_model_return = stock_return - bench_return) |>
  mutate(CAPM_return =  stock_return - (alpha + beta*bench_return))
# - End - Mutate data

ggplot(abnormal_returns, aes(x = dates_relative)) +
  geom_line(aes(y = stock_adj, color = time_period)) +
  geom_vline(aes(xintercept = 0), linetype = 2) +
  scale_x_reverse()

