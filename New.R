rm(list=ls()) 

# - Get and format data for time period(s)- 

ticker_stock <- "ATVI" # !User input!
ticker_bench = "^GSPC" # !User input!

event_date <- YMD('2018-11-5') # !User input!

### Will implement at a later date --
estimation_period <- 10 # !User input!
event_period <- 1 # !User input!
anticipation_period <- 10 # !User input!
adjustment_period <- 10 # !User input!
### Will implement at a later date --


begin <-event_date - as.difftime(200, unit="days") 
end <-event_date + as.difftime(30, unit="days") 


stock <- tq_get(ticker_stock, get = "stock.prices", from = begin, to = end, periodicity = "daily") |>
  tq_mutate(mutate_fun = periodReturn, col_rename = 'stock_return', period = "daily")  |>
  select(symbol, date, stock_adj = adjusted, stock_return) 
stock <- stock[-1,] # remove first value due to incorrect returns value

bench <- tq_get(ticker_bench, get = "stock.prices", from = begin, to = end, periodicity = "daily") |>
  tq_mutate(mutate_fun = periodReturn, col_rename = 'bench_return', period = "daily") |>
  select(symbol, date, bench_adj = adjusted, bench_return)
bench <- bench[-1,]

stock <- stock[order(stock$date),] # just for code safety, r should always give in order
bench <- bench[order(bench$date),] 


abnormal_returns <- left_join(stock, bench, by = c("date" = "date")) |>
  mutate(ID = row_number()) 
eventID <- which(abnormal_returns$date == event_date, arr.ind=TRUE)

abnormal_returns <- abnormal_returns |>
  mutate(days_before = -1 *(ID - eventID)) |>
  filter(days_before <= 100 & days_before >= -10) |> # later add ability for user to adjust time periods,  
  select(date, days_before, stock_adj, bench_adj, stock_return, bench_return) |>
  mutate(time_period = ifelse(days_before > 10, "EST", ifelse(days_before <= 10 & days_before >= 1, "ANT", #should NOT be any NA values
                                                              ifelse(date == event_date, "EVENT", ifelse(days_before < 0, "ADJ", NA))))) 
                                                                                                  
# - End -

# - Calculate CAR, BHAR, T-stats, P-values for each period EST, ANT, EVENT, ADJ - 

EST <-abnormal_returns[abnormal_returns$time_period == "EST",]

average_stock_returns_est<- mean(EST$stock_return)

CAPM_table <- EST |>
  tq_performance(Ra = stock_return, Rb = bench_return, performance_fun = table.CAPM)

alpha <- CAPM_table$Alpha
beta <- CAPM_table$Beta # !have as an output, so user can decide which model to sue 

abnormal_returns <- abnormal_returns |>
  select(date,days_before, time_period,stock_adj, bench_adj, stock_return, bench_return,  ) |>
  mutate(constant_return = stock_return - average_stock_returns_est) |>
  mutate(market_model_return = stock_return - bench_return) |>
  mutate(CAPM_return =  stock_return - (alpha + beta*bench_return))

# we need to redo EST as we calculated the returns
EST <-abnormal_returns[abnormal_returns$time_period == "EST",]
ANT <-abnormal_returns[abnormal_returns$time_period == "ANT",]
EVENT <-abnormal_returns[abnormal_returns$time_period == "EVENT",]
ADJ <-abnormal_returns[abnormal_returns$time_period == "ADJ",]
TOTAL <- abnormal_returns[abnormal_returns$time_period != "EST",]

# now we want to produce our tables

models = c("constant_market", "market_model", "CAPM")
time_periods = c("Anticipation", "Event", "Adjustment", "Total")


const_stdev <- STDEV(abnormal_returns[["constant_return"]])
market_stdev <- STDEV(abnormal_returns[["market_model_return"]])
capm_stdev <- STDEV(abnormal_returns[["CAPM_return"]])

      
STD_errors <- data_frame(time_periods = time_periods, const_stdev = c(const_stdev,(const_stdev * sqrt(10)),(const_stdev * sqrt(10)), (const_stdev * sqrt(21))),
                           market_stdev = c(market_stdev,(market_stdev * sqrt(10)),(market_stdev * sqrt(10)), (market_stdev * sqrt(21))),
                           capm_stdev = c(capm_stdev,(capm_stdev * sqrt(10)),(capm_stdev * sqrt(10)), (capm_stdev * sqrt(21))))
                           
## FOR ISSUE STD_ERRORS FOR OKAY

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

T_stat_CAR <- cbind(CAR_returns[1],round(CAR_returns[-1]/STD_errors[-1],digits = 6))
T_stat_BHAR <- cbind(BHAR_returns[1],round(BHAR_returns[-1]/STD_errors[-1],digits = 6))

inital_df <- nrow(EST) # number of degrees of freedom i.e estimation days

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

# - End - 

