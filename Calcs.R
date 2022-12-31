# - Begin - Separate data frames (more readable code)
EST <- abnormal_returns[abnormal_returns$time_period == "EST",]
ANT <- abnormal_returns[abnormal_returns$time_period == "ANT",]
EVENT <- abnormal_returns[abnormal_returns$time_period == "EVENT",]
ADJ <- abnormal_returns[abnormal_returns$time_period == "ADJ",]
TOTAL <- abnormal_returns[abnormal_returns$time_period != "EST",]
# - End - Separate data frames (more readable code)

# - Begin -  Produce tibbles for STD error andCAR/BHAR returns
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
# - End -  Produce tibbles for STD error andCAR/BHAR returns

# - Begin - Calculating statistical T and P lvaues
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
# - End - Calculating statistical T and P lvaues


# should i calculate p-values for i = 0: i<= 10: i++ 
# would tell me what period values are significant in
# same for anticipation
# would be a good graph
# make this as a new ?tibble?, dont forget to ignore EST
# can plot this, with a horizontal line at significance level
# graph udpates as you change =- days 
#will give dates when significant


# Not to be used in plotting, hence no need to take up memory
rm(ANT)
rm(ADJ)
rm(EST)
rm(EVENT)
rm(TOTAL)
rm(bench)
rm(stock)
rm(CAPM_table)
rm(eventID)
rm(inital_df)
