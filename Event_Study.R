calc_abr <- function(ticker_stock, ticker_bench, event_date, est, ant, adj) {
  # Get Data
  begin <-event_date - as.difftime((7 * est), unit="days") 
  end <-event_date + as.difftime((7 * adj), unit="days") 
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
  
  # Mutate data
  abnormal_returns <- left_join(stock, bench, by = c("date" = "date")) |>
    mutate(ID = row_number()) 
  eventID <- which(abnormal_returns$date == event_date, arr.ind=TRUE)
  # eventID will not exist if event day is not a trading day, can check using exists(), not sure what to return without too many var
  abnormal_returns <- abnormal_returns |>
    mutate(dates_relative = as.integer(-1 *(ID - eventID))) |>
    mutate(time_period = ifelse(dates_relative > ant, "EST", 
                                ifelse(dates_relative <= ant & dates_relative >= 1, "ANT", 
                                       ifelse(dates_relative == 0, "EVENT", ifelse(dates_relative < 0, "ADJ", NA))))) |>
    filter(dates_relative <= (est + ant) & dates_relative >= (-1 * adj)) |> 
    select(date, dates_relative, time_period, stock_adj, bench_adj, stock_return, bench_return) 
  
  EST <-abnormal_returns[abnormal_returns$time_period == "EST",]
  average_stock_returns_est<- mean(EST$stock_return)
  
  CAPM_table <- EST |>
    tq_performance(Ra = stock_return, Rb = bench_return, performance_fun = table.CAPM)
  alpha <- CAPM_table$Alpha
  beta <- CAPM_table$Beta 
  
  abnormal_returns <- abnormal_returns |>
    select(date,dates_relative, time_period,stock_adj, bench_adj, stock_return, bench_return) |>
    mutate(ConstantModel= stock_return - average_stock_returns_est) |>
    mutate(MarketModel = stock_return - bench_return) |>
    mutate(CAPM =  stock_return - (alpha + beta*bench_return))
  return (abnormal_returns)
}

calc_stats <- function(abnormal_returns, AA){
  EST <- abnormal_returns[abnormal_returns$time_period == "EST",]
  ANT <- abnormal_returns[abnormal_returns$time_period == "ANT",] 
  EVENT <- abnormal_returns[abnormal_returns$time_period == "EVENT",]
  ADJ <- abnormal_returns[abnormal_returns$time_period == "ADJ",]
  TOTAL <- abnormal_returns[abnormal_returns$time_period != "EST",]
  time_periods = c("Anticipation", "Event", "Adjustment", "Total")
  
  const_stdev <- STDEV(abnormal_returns[["ConstantModel"]])
  market_stdev <- STDEV(abnormal_returns[["MarketModel"]])
  capm_stdev <- STDEV(abnormal_returns[["CAPM"]])
  
  STD_errors <- data_frame(time_periods = time_periods, const_stdev = c((const_stdev * sqrt(10)),const_stdev,(const_stdev * sqrt(10)), (const_stdev * sqrt(21))),
                           market_stdev = c((market_stdev * sqrt(10)), market_stdev,(market_stdev * sqrt(10)), (market_stdev * sqrt(21))),
                           capm_stdev = c((capm_stdev * sqrt(10)),capm_stdev,(capm_stdev * sqrt(10)), (capm_stdev * sqrt(21))))
  
  CAR_returns <- tibble(time_periods = time_periods, 
                        ConstantModel = c(sum(ANT$ConstantModel), sum(EVENT$ConstantModel), sum(ADJ$ConstantModel), SUM(TOTAL$ConstantModel)),
                        MarketModel = c(sum(ANT$MarketModel), sum(EVENT$MarketModel), sum(ADJ$MarketModel), sum(TOTAL$MarketModel)), 
                        CAPM = c(sum(ANT$CAPM), sum(EVENT$CAPM), sum(ADJ$CAPM), sum(TOTAL$CAPM)))
  
  bhar <- function(dataframe, model){
    return(apply ((dataframe[, model] + 1), 2, prod) - 1)
  }
  
  BHAR_returns <- tibble(time_periods = time_periods, 
                         ConstantModel = c(bhar(ANT, "ConstantModel"), bhar(EVENT, "ConstantModel"), bhar(ADJ, "ConstantModel"), bhar(TOTAL, "ConstantModel")),
                         MarketModel = c(bhar(ANT, "MarketModel"), bhar(EVENT, "MarketModel"), bhar(ADJ, "MarketModel"), bhar(TOTAL, "MarketModel")),
                         CAPM = c(bhar(ANT, "CAPM"), bhar(EVENT, "CAPM"), bhar(ADJ, "CAPM"), bhar(TOTAL, "CAPM")))
  
  T_stat_CAR <- cbind(CAR_returns[1],round(CAR_returns[-1]/STD_errors[-1],digits = 6))
  
  T_stat_BHAR <- cbind(BHAR_returns[1],round(BHAR_returns[-1]/STD_errors[-1],digits = 6))
  
  inital_df <- nrow(EST) 
  P_val_CAR <- T_stat_CAR |>
    mutate(ConstantModel = (2 * pt(q=abs(T_stat_CAR$ConstantModel), lower.tail = FALSE, df=(inital_df - 1)))) |>
    mutate(MarketModel = (2 * pt(q=abs(T_stat_CAR$MarketModel), lower.tail = FALSE, df=(inital_df - 1)))) |>
    mutate(CAPM = (2 * pt(q=abs(T_stat_CAR$CAPM), lower.tail = FALSE, df=(inital_df - 2)))) |>
    select(time_periods, ConstantModel, MarketModel, CAPM) 
  
  P_val_BHAR <- T_stat_BHAR |>
    mutate(ConstantModel = (2 * pt(q=abs(T_stat_BHAR$ConstantModel), lower.tail = FALSE, df=(inital_df - 1)))) |>
    mutate(MarketModel = (2 * pt(q=abs(T_stat_BHAR$MarketModel), lower.tail = FALSE, df=(inital_df - 1)))) |>
    mutate(CAPM = (2 * pt(q=abs(T_stat_BHAR$CAPM), lower.tail = FALSE, df=(inital_df - 2)))) |>
    select(time_periods, ConstantModel, MarketModel, CAPM)
  
  
  if(AA == 1) return (P_val_BHAR) else if(AA == 2) return (P_val_BHAR) else NA
}
