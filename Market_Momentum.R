# Do morket momemtum, using different day averages and option for different stocks
library(tidyquant)
library(tidyverse)
library(dypl)
# User Inputs
moving_average <- 10
ticker_stock <- "ATVI" 
ticker_bench <- "^GSPC" 
begin_date <- YMD("2018-11-05") 
end_date <- YMD("2018-11-05") 

stock <- tq_get(ticker_stock, get = "stock.prices", from = begin_date, to = end_date, periodicity = "daily")

bench <- tq_get(ticker_bench, get = "stock.prices", from = begin_date, to = end_date, periodicity = "daily") |>
  select(symbol, date, bench_adj = adjusted) |>
  arrange(date)

