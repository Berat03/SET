packages <- installed.packages()
install.packages('lubridate')
write.csv(packages, file = "requirments.txt")

# need to split estimation and event period (+- 2 days?)

AAPL  <- tq_get("AAPL", get = "stock.prices", from = "2010-01-01", to = "2013-01-01", periodicity = "monthly")
SP500 <- tq_get("^GSPC", get = "stock.prices", from = "2010-01-01", to = "2013-01-01", periodicity = "monthly")


AAPL<- AAPL |> 
  tq_mutate(mutate_fun = periodReturn, col_rename = 'return', period = "monthly") |>
  select(date, close, return) |>
  mutate(cum_return = cumsum(return))


SP500<- SP500 |>
  tq_mutate(mutate_fun = periodReturn, col_rename = 'return', period = "monthly") |>
  select(date, close, return) |>
  mutate(cum_return = cumsum(return))


#graph looks to be working
ggplot(AAPL, aes(x = date)) + 
  geom_line(aes(y = return, colour = "return")) + 
  geom_line(aes(y = cum_return, colour = "cum_return"))


