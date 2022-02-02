# // Title: Forecasting with different models and transformations
# // Author: Faiyam Islam
# // Student ID: z5258151

# Question 1

# Figure 1: Autoplot with no transformations
view(global_economy) 
china_gdp <- global_economy %>%
  filter(country == "China")
view(china_gdp)
china_auto <- china_gdp %>%
  autoplot(GDP/1e9) + 
  labs(title = "Chinese GDP", subtitle = "Autoplot with no transformations")
china_auto

# Figure 2: Autoplot with log transformation 
china_gdp %>%
  autoplot(log(GDP)) + 
  labs(title = "Log transformation on Chinese GDP")

# Figure 3: Autoplot with box-cox transformation 
china_lambda <- china_gdp %>%
  features(GDP, guerrero) 
china_gdp %>%
  autoplot(box_cox(GDP, -0.0345)) + 
  labs(title = "Box-Cox transformation on Chinese GDP")

# Figure 4: ETS model (A, A, N) of Chinese GDP
chinese_gdp <- global_economy %>%
  filter(Country == "China") %>%
  summarise(GDP = sum(GDP))
fit <- chinese_gdp %>%
  model(ETS(GDP ~ error("A") + trend("A") + season("N")))
fc <- fit %>% forecast(h=20)
fc %>%
  autoplot(chinese_gdp) + 
  labs(y = "GDP", title = "Chinese GDP", subtitle = "Holt's linear model 
       (A, A, N)")

# ETS model (A, A, N) report
fit <- china_gdp %>%
  model(ETS(GDP ~ error("A") + trend("A") + season("N")))
report(fit)

# Figure 5: ETS model (A, Ad, N) of Chinese GDP 
chinese_gdp <- global_economy %>%
  filter(Country == "China") %>%
  summarise(GDP = sum(GDP))
fit <- chinese_gdp %>%
  model(ETS(GDP ~ error("A") + trend("Ad") + season("N")))
fc <- fit %>% forecast(h=20)
fc %>%
  autoplot(chinese_gdp) + 
  labs(y = "GDP", title = "Chinese GDP", subtitle = "ETS model with (A, Ad, N)")

# ETS model (A, Ad, N) report
fit <- china_gdp %>%
  model(ETS(GDP ~ error("A") + trend("Ad") + season("N")))
report(fit) 

# Figure 6: ETS model (A, Md, N) of Chinese GDP
chinese_gdp <- global_economy %>%
  filter(Country == "China") %>%
  summarise(GDP = sum(GDP))
fit <- chinese_gdp %>%
  model(ETS(GDP ~ error("A") + trend("Md") + season("N")))
fc <- fit %>% forecast(h=20)
fc %>%
  autoplot(chinese_gdp) + 
  labs(y = "GDP", title = "Chinese GDP", subtitle = "ETS model with (A, Md, N)")

# ETS model (A, Md, N) report
fit <- china_gdp %>%
  model(ETS(GDP ~ error("A") + trend("Md") + season("N")))
report(fit)

# Figure 7: ETS model (M, A, N) of Chinese GDP 
chinese_gdp <- global_economy %>%
  filter(Country == "China") %>%
  summarise(GDP = sum(GDP))
fit <- chinese_gdp %>%
  model(ETS(GDP ~ error("M") + trend("A") + season("N")))
fc <- fit %>% forecast(h=20) 
fc %>%
  autoplot(chinese_gdp) + 
  labs(y = "GDP", title = "Chinese GDP", subtitle = "Holt's linear model 
       (M, A, N)")

# ETS model (M, A, N) report
fit <- china_gdp %>%
  model(ETS(GDP ~ error("M") + trend("A") + season("N")))
report(fit)

# Figure 8: ETS model (A, N, N) of Chinese GDP
chinese_gdp <- global_economy %>%
  filter(Country == "China") %>%
  summarise(GDP = sum(GDP))
fit <- chinese_gdp %>%
  model(ETS(GDP ~ error("A") + trend("N") + season("N")))
fc <- fit %>% forecast(h=20) 
fc %>% 
  autoplot(chinese_gdp) + 
  labs(y = "GDP", title = "Chinese GDP", subtitle = "Simple exponential 
       smoothing (A, N, N)")

# ETS model (A, N, N) report
fit <- china_gdp %>%
  model(ETS(GDP ~ error("A") + trend("N") + season("N")))
report(fit)

# Figure 9: Log transformation with Holt's Linear Method
china_log <- china_gdp %>%
  model(ETS(log(GDP)))
china_log
china_log %>%
  forecast(h=20) %>%
  autoplot(china_gdp) + 
  labs(title = "Forecast using log(GDP) Chinese GDP", 
       subtitle = "Holt's linear method with log transformation")

# ETS model of log transformation on Holt's Linear Method
fit <- china_gdp %>%
  model(ETS(log(GDP)))
report(fit)

# Figure 10: Box-Cox transformation with Holt's Linear Method
china_lambda <- china_gdp %>%
  features(GDP, guerrero)
china_lambda
auto_box_fit <- china_gdp %>%
  model(ETS(box_cox(GDP, -0.0345)))
auto_box_fit 
auto_box_fit %>%
  forecast(h=20) %>%
  autoplot(china_gdp) + 
  labs(title = "Forecast using Box-Cox transformation on Chinese GDP",
       subtitle = "Box-Cox transformation: lambda = -0.0345")

# ETS model of Box-Cox transformation Holt's Linear Method
fit <- china_gdp %>%
  model(ETS(box_cox(GDP, -0.0345)))
report(fit) 

# Figure 12: Model Selection on ETS models of Chinese GDP
fit <- chinese_gdp <- global_economy %>%
  filter(Country == "China") %>%
  model( 
    given = ETS(GDP), 
    ann = ETS(GDP ~ error("A") + trend("N") + season("N")), 
    aadn = ETS(GDP ~ error("A") + trend("Ad") + season("N")), 
    amdm = ETS(GDP ~ error("A") + trend("Md") + season("N")), 
    holtsALM = ETS(GDP ~ error("A") + trend("A") + season("N")), 
    holtsMLM = ETS(GDP ~ error("M") + trend("A") + season("N")), 
    mnn = ETS(GDP ~ error("M") + trend("N") + season("N")), 
    madn = ETS(GDP ~ error("M") + trend("Ad") + season("N")), 
    mmdn = ETS(GDP ~ error("M") + trend("Md") + season("N")), 
    log = ETS(log(GDP)), 
    boxcox = ETS(box_cox(GDP, 0.2))
  )
fit %>%
  glance()

# Problem 2
# Part a) 
NZ_arrivals <- aus_arrivals %>%
  filter(Origin == "NZ") %>%
  summarise(NZarrivals = sum(Arrivals)) 
autoplot(NZ_arrivals) + labs(y="NZ Passengers", title = "Australian Arrivals
                             from New Zealand")

gg_season(NZ_arrivals) + labs(title = "Seasonality in Australian arrivals
                              from New Zealand")

# Part b) 
train_NewZealandPassengers <- NZ_arrivals %>%
  filter_index("1981 Q1" ~ "2010 Q3")
fit <- train_NewZealandPassengers %>% model(multiplicative = ETS(NZ_arrivals ~
error("M") + trend("A") + season("M")))
fit <- train_NewZealandPassengers %>%
  model(multiplicative = ETS(NZarrivals ~ error("M") + trend("A") + season("M")))
fc <- fit %>% forecast(h=10) 
fc %>% autoplot(train_NewZealandPassengers, level = NULL) + xlab("Year") + 
  ylab("NZ Arrivals") + scale_color_brewer(type = "qual", palette = "Dark2") + 
  labs(title = "Australian Arrivals from New Zealand",
       subtitle = 'Australian Arrivals from New Zealand using Holt Winters'
       'multiplicative method')

# Part d) 
fc_new <- train_NewZealandPassengers %>% model(
  ets = ETS(NZarrivals), 
  log_ets = ETS(log(NZarrivals)), 
  snaive = SNAIVE(NZarrivals), 
  stl = decomposition_model(STL(log(NZarrivals)), ETS(season_adjust))
) %>%
  forecast(h = "2 years")
fc_new %>% autoplot(train_NewZealandPassengers, level = NULL) +
  xlab("Year") + ylab("NZ Arrivals") + labs(title = "NZ Arrivals to Australia", 
                                            sutitle = "Forecast of ETS models
                                            using training set")
fc_new %>% autoplot(level = NULL) + autolayer(filter(NZ_arrivals, 
year(Quarter) > 2005), NZarrivals) + 
  labs(title = "NZ arrivals to Australia", 
       subtitle = "Forecast of ETS models using training set")

# Part e) 
fc_new %>%
  accuracy(NZ_arrivals)

LogETS <- train_NewZealandPassengers %>% model(ETS(log(NZarrivals)))
augment(LogETS) %>% gg_tsdisplay(.resid, lag_max = 24, plot_type = "histogram")
augment(LogETS) %>%
  features(.innov, ljung_box, lag = 24, dof = 7)










