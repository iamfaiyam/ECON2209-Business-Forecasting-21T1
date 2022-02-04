# // Title: Data Exploration and Transformation, Forecasting, Exponential Smoothing and ARIMA Modelling
# // Author: Faiyam Islam
# // Student ID: z5258151

set.seed(5258151)
myseries <- aus_retail %>%
  filter(`Series ID` == sample(aus_retail$`Series ID`,1))

# Data Exploration and Transformation 

myseries %>%
  autoplot(Turnover) +
  labs(y = "Turnover (million $AUD)",
       title = myseries$Industry[1],
       subtitle = myseries$State[1])

myseries %>%
  gg_season(Turnover, labels = "both") +
  labs(y = "Turnover (million $AUD)",
       title = myseries$Industry[1],
       subtitle = myseries$State[1])

myseries %>%
  gg_subseries(Turnover) +
  labs(y = "Turnover (million $AUD)", x="")

myseries %>%
  gg_lag(Turnover, lags=1:24, geom='point') + facet_wrap(~ .lag, ncol=6)

myseries %>%
  ACF(Turnover, lag_max = 50) %>%
  autoplot()

myseries %>%
  autoplot(box_cox(Turnover, 0)) +
  labs(
    title = myseries$Industry[1],
    subtitle = myseries$State[1]
  )

myseries %>%
  features(Turnover, features = guerrero)

# Forecasting

myseries_train <- myseries %>%
  filter(year(Month) < 2011)
autoplot(myseries, Turnover) +
  autolayer(myseries_train, Turnover, colour = "red")

fit <- myseries_train %>%
  model(SNAIVE(Turnover))

fit %>% gg_tsresiduals()

fc <- fit %>%
  forecast(new_data = anti_join(myseries, myseries_train))

fc %>% autoplot(filter(myseries, year(Month) > 2004))

bind_rows(
  accuracy(fit),
  accuracy(fc, myseries)
) %>%
  select(-State, -Industry, -.model)

# Exponential Smoothing

fit <- myseries %>%
  model(
    hw = ETS(Turnover ~ error("M") + trend("A") + season("M")),
    hwdamped = ETS(Turnover ~ error("M") + trend("Ad") + season("M"))
  )
fc <- fit %>% forecast(h = 36)
fc %>% autoplot(filter(myseries, year(Month) > 2004))

accuracy(fit)

fit %>%
  select("hwdamped") %>%
  gg_tsresiduals()

myseries %>%
  filter(year(Month) < 2011) %>%
  model(
    snaive = SNAIVE(Turnover),
    hw = ETS(Turnover ~ error("M") + trend("A") + season("M")),
    hwdamped = ETS(Turnover ~ error("M") + trend("Ad") + season("M"))
  ) %>%
  forecast(h = "8 years") %>%
  accuracy(myseries)

stl <- decomposition_model(
  STL(log(Turnover)),
  ETS(season_adjust)
)
fc <- myseries %>%
  filter(year(Month) < 2011) %>%
  model(
    stl_ets = stl,
    snaive = SNAIVE(Turnover),
    hwdamped = ETS(Turnover ~ error("M") + trend("Ad") + season("M"))
  ) %>%
  forecast(h = "8 years")
fc %>% autoplot(filter(myseries, year(Month) > 2004), level = NULL)

fc %>% accuracy(myseries)

# ARIMA Modelling

myseries %>% autoplot(log(Turnover))

myseries %>% autoplot(log(Turnover) %>% difference(lag = 12))

myseries %>% autoplot(log(Turnover) %>% difference(lag = 12) %>% difference())

# Number of seasonal differences:
myseries %>%
  features(log(Turnover), unitroot_nsdiffs)

# Suggests one, so take a seasonal difference and check if a first difference is also needed:
myseries %>%
  features(difference(log(Turnover), 12), unitroot_ndiffs)

fit <- myseries %>%
  model(arima = ARIMA(log(Turnover)))
report(fit)

gg_tsresiduals(fit)

stlets <- decomposition_model(
  STL(log(Turnover)),
  ETS(season_adjust)
)
stlarima <- decomposition_model(
  STL(log(Turnover)),
  ARIMA(season_adjust)
)
fit <- myseries_train %>%
  model(
    snaive = SNAIVE(Turnover),
    hwdamped = ETS(Turnover ~ error("M") + trend("Ad") + season("M")),
    arima = ARIMA(log(Turnover)),
    stlets = stlets,
    stlarima = stlarima
  )
fc <- fit %>%
  forecast(h="8 years")
fc %>%
  accuracy(myseries) %>%
  select(.model, RMSE, MAE)

fc %>% autoplot(filter(myseries, year(Month) > 2004), level = NULL)
  
  
  














