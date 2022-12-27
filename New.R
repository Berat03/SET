rm(list=ls()) 

ticker_stock <- "ATVI"
ticker_bench = "^GSPC"
event_date <- YMD('2018-11-5') 

temp_begin <- YMD('2018-07-31') # to calc
begin_date = YMD('2018-08-01') 
end_date = YMD('2018-10-19') 

estimation <- 10
anticipation <- 10
total_look <- estimation + anticipation + 1
tq_date_fix <-end_date + as.difftime(100, unit="days") 


end <- YMD('2018-11-20') # not inclusive????

stock <- tq_get(ticker_stock, get = "stock.prices", from = temp_begin, to = end, periodicity = "daily") |>
  tq_mutate(mutate_fun = periodReturn, col_rename = 'stock_return', period = "daily")  |>
  select(symbol, date, stock_adj = adjusted, stock_return) |>
  filter(date >= begin_date) # too speciifc


bench <- tq_get(ticker_bench, get = "stock.prices", from = temp_begin, to = end_date, periodicity = "daily") |>
  tq_mutate(mutate_fun = periodReturn, col_rename = 'bench_return', period = "daily") |>
  select(symbol, date, bench_adj = adjusted, bench_return) |>
  filter(date >= begin_date) # too speciifc



abnormal_returns <- left_join(stock, bench, by = c("date" = "date")) |>
  mutate(ID = row_number())
eventID <- which(abnormal_returns$date == event_date, arr.ind=TRUE)

abnormal_returns <- abnormal_returns |>
  mutate(time_frame = ifelse(date >= begin_date & date <= end_date, "EST", 
                             ifelse(date > event_date, "ADJ",
                                    ifelse(date == event_date, "EVENT",
                                           ifelse(ID < eventID & ID > eventID-(anticipation+1), "ANT", NA))))) |>
  mutate(days_before = -1 *(ID - eventID))



est <-abnormal_returns[abnormal_returns$time_frame == "EST",]
average_stock_returns_est<- mean(est$stock_return)

CAPM_table <- est |>
  tq_performance(Ra = stock_return, Rb = bench_return, performance_fun = table.CAPM)

alpha <- CAPM_table$Alpha
beta <- CAPM_table$Beta


abnormal_returns <- abnormal_returns |>
  select(date, stock_adj, bench_adj, stock_return, bench_return ) |>
  mutate(constant_return = stock_return - average_stock_returns_est) |>
  mutate(market_model_return = stock_return - bench_return) |>
  mutate(CAPM_return =  stock_return - (alpha + beta*bench_return))



