library(tidyverse)
library(tidyquant)
rm(list=ls()) 

ticker_stock <- "ATVI" 
ticker_bench = "^GSPC" 
event_date <- YMD('2018-11-5') 

begin <-event_date - as.difftime(300, unit="days") 
end <- event_date + as.difftime(100, unit="days") 
ant = 10
adj = 10
est = 30

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
  mutate(time_period = ifelse(dates_relative > ant, "EST", ifelse(dates_relative <= ant & dates_relative > 1, "ANT", 
                                                                  ifelse(date == event_date, "EVENT", ifelse(dates_relative < 0, "ADJ", NA)))))
max_d <- est + ant
min_d <- -1 * adj
abnormal_returns <- abnormal_returns |>
  filter(dates_relative <= max_d & dates_relative >= min_d) |> 
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

# - Begin - Separate data frames
EST <- abnormal_returns[abnormal_returns$time_period == "EST",]
ANT <- abnormal_returns[abnormal_returns$time_period == "ANT",]
EVENT <- abnormal_returns[abnormal_returns$time_period == "EVENT",]
ADJ <- abnormal_returns[abnormal_returns$time_period == "ADJ",]
TOTAL <- abnormal_returns[abnormal_returns$time_period != "EST",]
# - End - Separate data frames 

# - Begin -  Produce tibbles for STD error, CAR and BHAR returns
models = c("constant_market", "market_model", "CAPM")
time_periods = c("Anticipation", "Event", "Adjustment", "Total")


const_stdev <- STDEV(abnormal_returns[["constant_return"]])
market_stdev <- STDEV(abnormal_returns[["market_model_return"]])
capm_stdev <- STDEV(abnormal_returns[["CAPM_return"]])


STD_errors <- data_frame(time_periods = time_periods, const_stdev = c((const_stdev * sqrt(10)),const_stdev,(const_stdev * sqrt(10)), (const_stdev * sqrt(21))),
                         market_stdev = c((market_stdev * sqrt(10)), market_stdev,(market_stdev * sqrt(10)), (market_stdev * sqrt(21))),
                         capm_stdev = c((capm_stdev * sqrt(10)),capm_stdev,(capm_stdev * sqrt(10)), (capm_stdev * sqrt(21))))

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
# - End -  Produce tibbles for STD error, CAR and BHAR returns

# - Begin - Calculating statistical T and P-values
T_stat_CAR <- cbind(CAR_returns[1],round(CAR_returns[-1]/STD_errors[-1],digits = 6))

T_stat_BHAR <- cbind(BHAR_returns[1],round(BHAR_returns[-1]/STD_errors[-1],digits = 6))

inital_df <- nrow(EST) # number of degrees of freedom, i.e estimation days

P_val_CAR <- T_stat_CAR |>
  mutate(constant_p_val = (2 * pt(q=abs(T_stat_CAR$constant_return), lower.tail = FALSE, df=(inital_df - 1)))) |>
  mutate(market_model_p_val = (2 * pt(q=abs(T_stat_CAR$market_model_return), lower.tail = FALSE, df=(inital_df - 1)))) |>
  mutate(CAPM_p_val = (2 * pt(q=abs(T_stat_CAR$CAPM_return), lower.tail = FALSE, df=(inital_df - 2)))) |>
  select(time_periods, constant_p_val, market_model_p_val, CAPM_p_val) 

P_val_BHAR <- T_stat_BHAR |>
  mutate(constant_p_val = (2 * pt(q=abs(T_stat_BHAR$constant_return), lower.tail = FALSE, df=(inital_df - 1)))) |>
  mutate(market_model_p_val = (2 * pt(q=abs(T_stat_BHAR$market_model_return), lower.tail = FALSE, df=(inital_df - 1)))) |>
  mutate(CAPM_p_val = (2 * pt(q=abs(T_stat_BHAR$CAPM_return), lower.tail = FALSE, df=(inital_df - 2)))) |>
  select(time_periods, constant_p_val, market_model_p_val, CAPM_p_val)


CAR <- left_join(P_val_CAR, T_stat_CAR, by=("time_periods"))
BHAR <- left_join(P_val_BHAR, T_stat_BHAR, by=("time_periods"))

?rbind
# - End - Calculating statistical T and P-values

ar_compare_models <- abnormal_returns |>
  pivot_longer(cols = c(constant_return, market_model_return, CAPM_return), names_to = 'model', values_to = 'value') |>
  select(date, dates_relative, model, value )


x <- ggplot(ar_compare_models, aes(x = dates_relative)) +
  geom_line(aes(y= value, color = model)) +
  labs(x = 'Trading Days Before Event', y = 'Abnormal Returns') +
  geom_vline(aes(xintercept = 0), linetype = 2) +
  scale_x_reverse()


ggplotly(x)
