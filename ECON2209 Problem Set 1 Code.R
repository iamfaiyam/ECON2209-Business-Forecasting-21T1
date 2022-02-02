# // Title: Forecasting annual as consumption 
# // Author: Faiyam Islam 
# // Student: ID: z5258151

# Problem 1
# install USgas package
install.packages("USgas")
library(USgas)

# Create a tsibble from us_total with index year and key state
us_tsibble <- us_total %>%
  as_tsibble(index = year, key = state)
view(us_tsibble)

# Refine the tsibble to only states in the New England area
us_tsibble %>%
  filter(state %in% c("Maine", "Vermont", "New Hampshire", "Massachusetts",
                      "Connecticut", "Rhode Island")) %>%
  autoplot(y/1e3) + 
  labs(y = "Cubic feet in billions") + labs(title = "Annual natural gas 
                                            consumption in the US")

# Problem 2
view(us_employment) 

# Autoplot of Total Private Employment in US
employment <- us_employment %>%
  filter(title == "Total Private")
auto <- employment %>%
  autoplot(Employed) + labs(title = "Total Private Employment in the US")
auto

## gg_season: Total Private Employment in US
us_employment %>%
  filter(Title == "Total Private") -> us_employment_priv
us_employment_priv %>% gg_season(Employed) + labs(title = "Total Private 
                                                  Employment in US")

## gg_subseries: Total Private Employment in US
us_employment_priv %>%
  gg_subseries(Employed) + labs(title = "Total Private Employment in US")

## gg_lag: Total Private Employment in US
us_employment_priv %>%
  gg_lag(Employed, geom = 'point', lags = 1:12) + labs(title = "Total Private
                                                       Employment in US")

# Autocorrelation Function (ACF): Total Private Employment in US
us_employment_priv %>%
  filter(year(Month) >= 2000) %>%
  ACF(Employed, lag_max = 48) %>%
  autoplot() + labs(title = "Total Private Employment in US")

# Problem 3
# Tobacco from aus_production (before transformation) 
aus_production %>%
  autoplot(Tobacco) + labs(y = "Tobacco and cigarette production in tonnes", 
                           title = "Tobacco production in Australia")

# Tobacco from aus_production (Box-Cox transformation) 
tobacco_lambda <- aus_production %>%
  features(Tobacco, features = guerrero) 
tobacco_lambda
tobacco_box <- aus_production %>%
  autoplot(box_cox(Tobacco, 0.929)) + 
  labs(y = "Box-Cox transformed tobacco production", title = "Tobacco
       production in Australia")
tobacco_box

# Economy class passengers MEL-SYD (before transformation) 
mel_syd_eco <- ansett %>%
  filter(Airports == "MEL-SYD", Class == "Economy")
mel_syd_eco %>%
  autoplot(Passengers) + ggtitle("Economy Class passengers between Melbourne
                                 and Sydney")

# Economy class passengers MEL-SYD (Box-Cox transformation) 
mel_syd_eco <- ansett %>%
  filter(Airports == "MEL-SYD", Class == "Economy")
view(mel_syd_eco)
ansett_lambda <- mel_syd_eco %>%
  features(Passengers, features = guerrero)
ansett_lambda
ansett_box <- mel_syd_eco %>%
  autoplot(box_cox(Passengers, 2.00)) + 
  labs(y = "Box-Cox transformed passengers", 
       title = "Economy class passengers between Melbourne and Sydney")
ansett_box

# Pedestrian counts (before transformation) 
ped_count <- pedestrian %>%
  filter(Sensor == "Southern Cross Station")
ped_count %>%
  autoplot(Count) + labs(y = "Pedestrian counts", 
                         title = "Pedestrian counts at Southern Cross Station")

# Pedestrian counts (Box-Cox transformation) 
pedestrian_lambda <- ped_count %>%
  features(Count, features = guerrero)
pedestrian_lambda
pedestrian_box <- ped_count %>%
  autoplot(box_cox(Count, -0.226)) + 
  labs(y = "Box-Cox transformed Pedestrian counts", 
       title = "Pedestrian counts at Southern Cross Station")
pedestrian_box





