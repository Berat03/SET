# Need to calculate my own mu and sigma
# Need to add stock price onto matrix
# Calculate quantiles? and only plot the CI ~ 95%
# Rewrite the function as my own, understand dt?


ticker_stock <- "^GSPC" 
begin <- YMD("2019-11-05") 
end <-YMD("2020-11-05") 

stock <- tq_get(ticker_stock, get = "stock.prices", from = begin, to = end, periodicity = "daily") |>
  tq_mutate(mutate_fun = periodReturn, col_rename = 'stock_return', period = "daily")  |>
  select(date, adjusted, stock_return) |>
  arrange(date) |>
  slice(-1)


### Not my code, rewrite
gbm_loop <- function(nsim = 100, t = 25, mu = 0, sigma = 0.1, S0 = 100, dt = 1./365) {
  gbm <- matrix(ncol = nsim, nrow = t)
  for (simu in 1:nsim) {
    gbm[1, simu] <- S0
    for (day in 2:t) {
      epsilon <- rnorm(1)
      dt = 1 / 365
      gbm[day, simu] <- gbm[(day-1), simu] * exp((mu - sigma * sigma / 2) * dt + sigma * epsilon * sqrt(dt))
    }
  }
  return(gbm)
}
### Not my code, rewrite

nsim <- 100
t <- 50
mu <- 0
sigma <- 0.1

S0 <- tail(Ad(stock), 1)[[1]]

gbm <- gbm_loop(nsim, t, mu, sigma, S0)

gbm_df <- as.data.frame(gbm) %>%
  mutate(days = 1:nrow(gbm)) %>%
  pivot_longer(-days, names_to = 'sim', values_to = 'price')

ggplot(gbm_df, aes(x=days, y=price, color=sim)) +
  geom_line() +
  theme(legend.position = 'none')

