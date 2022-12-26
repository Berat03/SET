library(tidyquant)
library(tidyverse) # for docker image only install what i use, and only the newest functioning instance of it
library(formattable)

rm(list=ls()) 
###

# -Begin- Get relevant info & mutate DFs
event_date <- YMD('2001-09-11') 

stock  <- tq_get("MSFT", get = "stock.prices", from = "2001-01-15", to = "2001-09-20", periodicity = "daily") |>
  tq_mutate(mutate_fun = periodReturn, col_rename = 'stock_return', period = "daily")  |>
  mutate(days_before = as.numeric(event_date - date)) |> # actual days, not trading days!!!
  select(symbol, date, adjusted, stock_return, days_before)

ticker_bench = "^GSPC" 
bench <- tq_get(ticker_bench, get = "stock.prices", from = "2001-01-15", to = "2003-09-15", periodicity = "daily") |>
  tq_mutate(mutate_fun = periodReturn, col_rename = 'bench_return', period = "daily") |>
  select(symbol, date, adjusted, bench_return)

# -End- Get relevant info & mutate DFs 

# -Begin-  Calc 
RaRb <- left_join(stock, bench, by = c("date" = "date")) 

CAPM_table <- RaRb |>
  tq_performance(Ra = stock_return, Rb = bench_return, performance_fun = table.CAPM)

alpha <- CAPM_table$Alpha
alpha
beta <- CAPM_table$Beta
beta
rsqaure <- CAPM_table[[1, "R-squared"]]

regression_model <- lm(data = RaRb, bench_return ~ stock_return)
summary_regression_model <- summary(regression_model)
rs <- summary_regression_model$r.squared # same as one from CAPM

residual_se <- summary_regression_model$coefficients[[2,2]]




# -End- CAPM Calc 

abnormal_stock <- RaRb |>
  mutate(market_model_return = (alpha + beta * bench_return)) |> # no error term, rv
  mutate(abnormal_return = (stock_return - (alpha + beta * bench_return))) 

t_test_stock <- abnormal_stock |> # for CI 95%
  mutate(t_value =  abnormal_return / residual_se) |>
  mutate(significant = ifelse(abs(t_value) > 1.96, "yes", "no")) |>
  select(date, days_before, adjusted.x, abnormal_return, t_value, significant) |>
  filter(days_before < 5 & days_before > - 10000)

ggplot(t_test_stock, aes(x = days_before)) +
  geom_line(aes(y=t_value)) +
  geom_hline(yintercept = 1.96, color = 'red')+
  geom_hline(yintercept = -1.96, color = 'red') +
  scale_x_reverse()

# -Begin- Graph Plots

pivot_longer  <- abnormal_stock |> 
  pivot_longer(col = c(symbol.x, symbol.y), names_to = 'Obsolete', values_to='Symbol')
pivot_long_2 <- pivot_longer |>
  pivot_longer(cols = c(stock_return, bench_return, market_model_return, abnormal_return), names_to = "Different_Returns", values_to = "Metric") |>
  select(Symbol, date, days_before, adjusted.x, adjusted.y , Different_Returns, Metric)

ggplot(data = pivot_long_2, aes(x =days_before)) +
  geom_line(aes(y = Metric, color = Different_Returns), ) +
  labs(x = 'Dates before event', y = '% Returns') +
  geom_vline(aes(xintercept = 0)) +
  labs(color='Different Mettrics') +
  scale_x_reverse()


ggplot(data = abnormal_stock, aes(x = days_before)) +
  #geom_line(aes(y = stock_return, color = 'blue')) + 
  #geom_line(aes(y = bench_return, color = 'red')) + 
  geom_line(aes(y = abnormal_return, color = 'yellow')) +
  geom_line(aes(y = market_model_return, color = 'green')) +
  labs(x = '', y = '') +
  geom_vline(aes(xintercept = 0, linetype =''), show.legend = FALSE) +
  scale_x_reverse()

# -End- Graph Plots


