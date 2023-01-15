# Do morket momemtum, using different day averages and option for different stocks

library(tidyquant)
library(tidyverse) # when deploying, individually select packages
library(tidymod)

average_period <- 125
ticker_stock <- "^GSPC" 
begin_date <- ymd("2022-01-18") 
end_date <- ymd("2023-01-10") 
get_begin_info <- begin_date - as.difftime((4 * average_period), unit="days") 

stock <- tq_get(ticker_stock, from = get_begin_info, to = end_date <- ymd("2023-01-10"), periodicity = "daily") |>
  mutate(moving_average = zoo::rollmean(x = adjusted, k = average_period, fill = NA, align = "right")) |>
  filter(date > begin_date) |>
  select(date, Price = adjusted, "Moving Average" = moving_average)

stock <- stock |>
  pivot_longer(cols = !date, names_to = "Metric", values_to = "Value")



ggplot(stock, aes(x = date)) +
  geom_line(aes(y = Value, color = Metric)) +
  labs(x="", y = "") +
  theme(legend.position="top")

