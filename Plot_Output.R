ar_compare_models <- abnormal_returns |>
  pivot_longer(cols = c(constant_return, market_model_return, CAPM_return), names_to = 'model', values_to = 'value') |>
  select(date, dates_relative, model, value )


ggplot(ar_compare_models, aes(x = dates_relative)) +
  geom_line(aes(y= value, color = model)) +
  labs(x = 'Trading Days Before Event', y = 'Abnormal Returns') +
  geom_vline(aes(xintercept = 0), linetype = 2) +
  scale_x_reverse()

