library(tidyquant)
library(tidyverse)

rm(list=ls()) 

ticker_stock <- "ATVI"
ticker_bench = "^GSPC"

event_date <- YMD('2018-11-5') 
# have set adjustment and anticipation periods, add this functionality later

begin <-event_date - as.difftime(200, unit="days") 
end <-event_date + as.difftime(30, unit="days") 


stock <- tq_get(ticker_stock, get = "stock.prices", from = begin, to = end, periodicity = "daily") |>
  tq_mutate(mutate_fun = periodReturn, col_rename = 'stock_return', period = "daily")  |>
  select(symbol, date, stock_adj = adjusted, stock_return) 

bench <- tq_get(ticker_bench, get = "stock.prices", from = begin, to = end, periodicity = "daily") |>
  tq_mutate(mutate_fun = periodReturn, col_rename = 'bench_return', period = "daily") |>
  select(symbol, date, bench_adj = adjusted, bench_return)

stock <- stock[-1,] # get rid of first value as it has incorrect returns value
bench <- bench[-1,]


abnormal_returns <- left_join(stock, bench, by = c("date" = "date")) |>
  mutate(ID = row_number()) 
  
eventID <- which(abnormal_returns$date == event_date, arr.ind=TRUE)

abnormal_returns <- abnormal_returns |>
  mutate(days_before = -1 *(ID - eventID)) |>
  filter(days_before <= 100 & days_before >= -10) |>
  select(date, days_before, stock_adj, bench_adj, stock_return, bench_return)

#mutate(time_frame = ifelse(date >= begin_date & date <= end_date, "EST"|>

est <-abnormal_returns[abnormal_returns$time_frame == "EST",]
average_stock_returns_est<- mean(est$stock_return)

CAPM_table <- est |>
  tq_performance(Ra = stock_return, Rb = bench_return, performance_fun = table.CAPM)

alpha <- CAPM_table$Alpha
beta <- CAPM_table$Beta
rm(est)

abnormal_returns <- abnormal_returns |>
  select(ID, date,days_before, time_frame ,stock_adj, bench_adj, stock_return, bench_return,  ) |>
  mutate(constant_return = stock_return - average_stock_returns_est) |>
  mutate(market_model_return = stock_return - bench_return) |>
  mutate(CAPM_return =  stock_return - (alpha + beta*bench_return))


models = c("constant_market", "market_model", "CAPM")
time_periods = c("Event", "Anticipation", "Adjustment", "Total")
std_errors <- data_frame(models = models, 
                         stev = c(STDEV(abnormal_returns[["constant_return"]]), 
                                  STDEV(abnormal_returns[["market_model_return"]]), STDEV(abnormal_returns[["CAPM_return"]]))) |>
  mutate(std_errors_10 = stev * sqrt(10)) |>
  mutate(std_errors_21 = stev * sqrt(21)) 


returnCAR <- tibble(time_periods = time_periods)





