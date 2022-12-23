# Use FANG data set
data(FANG) # EXAMPLE DATASET
?FANG

# Get returns for individual stock components grouped by symbol
Ra <- FANG |>
  group_by(symbol) |>
  tq_transmute(adjusted, periodReturn, period = "monthly", col_rename = "Ra")

# Get returns for SP500 as baseline
Rb <- "^GSPC" |>
  tq_get(get  = "stock.prices", from = "2010-01-01", to = "2015-12-31") |>
  tq_transmute(adjusted, periodReturn, period = "monthly", col_rename = "Rb")

# Merge stock returns with baseline
RaRb <- left_join(Ra, Rb, by = c("date" = "date"))


# View options
tq_performance_fun_options()

# Get performance metrics

x<- RaRb |>
  tq_performance(Ra = Ra, Rb = Rb, performance_fun = table.CAPM) #used in market model

