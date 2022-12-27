View(abnormal_returns)

models = c("constant_market", "market_model", "CAPM")
time_periods = c("Event", "Anticipation", "Adjustment", "Total")
std_errors <- data_frame(models = models, 
                     stev = c(STDEV(abnormal_returns[["constant_return"]]), 
                              STDEV(abnormal_returns[["market_model_return"]]), STDEV(abnormal_returns[["CAPM_return"]]))) |>
  mutate(std_errors_10 = stev * sqrt(10)) |>
  mutate(std_errors_21 = stev * sqrt(21)) 


returnCAR <- tibble(time_periods = time_periods)
  



      