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
  select(date, days_before, stock_adj, bench_adj, stock_return, bench_return) |>
  mutate(time_period = ifelse(days_before > 10, "EST", ifelse(days_before <= 10 & days_before >= 1, "ANT", 
                                                              ifelse(date == event_date, "EVENT", ifelse(days_before < 0, "ADJ", NA)))))




EST <-abnormal_returns[abnormal_returns$time_period == "EST",]

average_stock_returns_est<- mean(EST$stock_return)

CAPM_table <- EST |>
  tq_performance(Ra = stock_return, Rb = bench_return, performance_fun = table.CAPM)

alpha <- CAPM_table$Alpha
beta <- CAPM_table$Beta

abnormal_returns <- abnormal_returns |>
  select(date,days_before, time_period,stock_adj, bench_adj, stock_return, bench_return,  ) |>
  mutate(constant_return = stock_return - average_stock_returns_est) |>
  mutate(market_model_return = stock_return - bench_return) |>
  mutate(CAPM_return =  stock_return - (alpha + beta*bench_return))


EST <-abnormal_returns[abnormal_returns$time_period == "EST",]
ANT <-abnormal_returns[abnormal_returns$time_period == "ANT",]
EVENT <-abnormal_returns[abnormal_returns$time_period == "EVENT",]
ADJ <-abnormal_returns[abnormal_returns$time_period == "ADJ",]
TOTAL <- abnormal_returns[abnormal_returns$time_period != "EST",]

# now we want to produce our tables

models = c("constant_market", "market_model", "CAPM")
time_periods = c("Anticipation", "Event", "Adjustment", "Total")

STD_errors <- data_frame(models = models, stev = c(STDEV(abnormal_returns[["constant_return"]]), 
                                  STDEV(abnormal_returns[["market_model_return"]]), STDEV(abnormal_returns[["CAPM_return"]]))) |>
  mutate(std_errors_10 = stev * sqrt(10)) |>
  mutate(std_errors_21 = stev * sqrt(21)) 

CAR_returns <- tibble(time_periods = time_periods, 
                      constant_return = c(sum(ANT$constant_return), sum(EVENT$constant_return), sum(ADJ$constant_return), SUM(TOTAL$constant_return)),
                      market_model_return = c(sum(ANT$market_model_return), sum(EVENT$market_model_return), sum(ADJ$market_model_return), sum(TOTAL$market_model_return)), 
                      CAPM_return = c(sum(ANT$CAPM_return), sum(EVENT$CAPM_return), sum(ADJ$CAPM_return), sum(TOTAL$CAPM_return)))


bhar <- function(dataframe, model){
  return(apply ((dataframe[, model] + 1), 2, prod) - 1)
  
}

BHAR_returns <- tibble(time_periods = time_periods, 
                       constant_return = c(bhar(ANT, "constant_return"), bhar(EVENT, "constant_return"), bhar(ADJ, "constant_return"), bhar(TOTAL, "constant_return")),
                       market_model_return = c(bhar(ANT, "market_model_return"), bhar(EVENT, "market_model_return"), bhar(ADJ, "market_model_return"), bhar(TOTAL, "market_model_return")),
                       CAPM_return = c(bhar(ANT, "CAPM_return"), bhar(EVENT, "CAPM_return"), bhar(ADJ, "CAPM_return"), bhar(TOTAL, "CAPM_return")))




