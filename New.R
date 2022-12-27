rm(list=ls()) 

ticker_stock <- "ATVI"
ticker_bench = "^GSPC"
event_date <- YMD('2018-11-5') 
begin <- YMD('2018-07-31') 
act_begin = YMD('2018-08-01') 
est_end = YMD('2018-10-19') 

event_date - as.difftime(100, unit="days") 
  #event_date - as.difftime(100, unit="days") 
#   mutate(days_before = as.numeric(event_date - date)) |> calculate later, always have dates

end <- YMD('2018-11-20') # not inclusive????

stock <- tq_get(ticker_stock, get = "stock.prices", from = begin, to = end, periodicity = "daily") |>
  tq_mutate(mutate_fun = periodReturn, col_rename = 'stock_return', period = "daily")  |>
  select(symbol, date, stock_adj = adjusted, stock_return) |>
  filter(date >= begin + as.difftime(1, unit="days")  ) # too speciifc


bench <- tq_get(ticker_bench, get = "stock.prices", from = begin, to = end, periodicity = "daily") |>
  tq_mutate(mutate_fun = periodReturn, col_rename = 'bench_return', period = "daily") |>
  select(symbol, date, bench_adj = adjusted, bench_return) |>
  filter(date >= begin + as.difftime(1, unit="days")) # too speciifc



######

abnormal_returns <- left_join(stock, bench, by = c("date" = "date"))

estim <- abnormal_returns |>
  filter(date >= act_begin & date <= est_end)

average_stock_returns_est<- mean(estim$stock_return)

CAPM_table <- estim |>
  tq_performance(Ra = stock_return, Rb = bench_return, performance_fun = table.CAPM)

alpha <- CAPM_table$Alpha
beta <- CAPM_table$Beta


abnormal_returns <- abnormal_returns |>
  select(date, stock_adj, bench_adj, stock_return, bench_return ) |>
  mutate(constant_return = stock_return - average_stock_returns_est) |>
  mutate(market_model_return = stock_return - bench_return) |>
  mutate(CAPM =  stock_return - (alpha + beta*bench_return))

