getwd()
setwd("/Users/william/Desktop/Predictive")
homedat <- read.csv("hw4_home_starts.csv")
str(homedat)

#1.
homedat$Date <- as.Date(homedat$Date, format = "%m/%d/%Y")
homedat$Month <- as.factor(homedat$Month)
homedat$Quarter <- as.factor(homedat$Quarter)
homedat$timetr <- 1:nrow(homedat)
homedat$timetrsq <- homedat$timetr^2
names(homedat)

#2.
reg1 <- lm(Starts ~ timetr, data = homedat)
sum1 <- summary(reg1)
reg1_quadratic <- lm(Starts ~ timetr + I(timetr^2), data = homedat)
sum1_quadratic <- summary(reg1_quadratic)
reg1_cubic <- lm(Starts ~ timetr + I(timetr^2) + I(timetr^3), data = homedat)
sum1_cubic <- summary(reg1_cubic)
reg1_quartic <- lm(Starts ~ timetr + I(timetr^2) + I(timetr^3) + I(timetr^4), data = homedat)
sum1_quartic <- summary(reg1_quartic)


sum1
sum1_quadratic
sum1_cubic
sum1_quartic
# R-squared of reg1: 0.06702
# R-squared of reg1_quadratic: 0.09831
# R-squared of reg1_cubic: 0.1126
# R-squared of reg1_quartic:  0.1184
# The interpretability of time trend variables decreases when higher order items are used.
# In higher-order models, the coefficients of the time trend represent the instantaneous rate of change at a specific point in time.
# There is a significant trend in the data set because the intercept is significant in four models.

predicted_quadratic <- predict(reg1_quadratic)
RMSE_quadratic <- sqrt(mean((homedat$Starts - predicted_quadratic)^2))
print(paste("RMSE for quadratic trend model:", RMSE_quadratic))
# RMSE for quadratic trend model: 36.021

library(ggplot2)
ggplot(homedat) +
  geom_line(aes(x = Date, y = Starts, color = "Actual")) +
  geom_line(aes(x = Date, y = predicted_quadratic, color = "Fitted")) +
  ggtitle("Actual vs. Fitted Housing Starts") +
  ylab("Housing Starts") +
  xlab("Date") +
  scale_color_manual(values = c("blue", "red"))

#3.
reg1_quadratic_quarter <- lm(Starts ~ timetr + I(timetr^2) + Quarter, data = homedat)
sum1_quadratic_quarter <- summary(reg1_quadratic_quarter)
sum1_quadratic_quarter
# R-squared of reg1_quadratic_quarter: 0.264 better than reg1_quadratic

predicted_quadratic_quarter <- predict(reg1_quadratic_quarter)
RMSE_quadratic_quarter <- sqrt(mean((homedat$Starts - predicted_quadratic_quarter)^2))
print(paste("RMSE for quadratic trend model with quarter variables:", RMSE_quadratic_quarter))
# RMSE for quadratic trend model with quarter variables: 32.54321

ggplot(homedat) +
  geom_line(aes(x = Date, y = Starts, color = "Actual")) +
  geom_line(aes(x = Date, y = predicted_quadratic_quarter, color = "Fitted")) +
  ggtitle("Actual vs. Fitted Housing Starts") +
  ylab("Housing Starts") +
  xlab("Date") +
  scale_color_manual(values = c("blue", "red"))

#4.
reg1_quadratic_quarter_month <- lm(Starts ~ timetr + I(timetr^2) + Quarter + Month, data = homedat)
sum1_quadratic_quarter_month <- summary(reg1_quadratic_quarter_month)
sum1_quadratic_quarter_month
# A specific pattern or trend is not evident in the data or is insufficient to make a reliable estimate.
# Because Quarter and Month is highly related, so I remove Quarter.

reg1_quadratic_month <- lm(Starts ~ timetr + I(timetr^2) + Month, data = homedat)
sum1_quadratic_month <- summary(reg1_quadratic_month)
sum1_quadratic_month
# R-squared of reg1_quadratic_month: 0.3404 better than reg1_quadratic_quarter
# If we want to predict Q2-Q4, reg1_quadratic_quarter is the best with intercepts of Q2-Q4 are all significiant.

predicted_quadratic_month <- predict(reg1_quadratic_quarter_month, newdata = homedat)
RMSE_quadratic_month <- sqrt(mean((homedat$Starts - predicted_quadratic_month)^2))
print(paste("RMSE for quadratic model with month variables:", RMSE_quadratic_month))
# RMSE for quadratic model with month variables: 30.8077

ggplot(homedat) +
  geom_line(aes(x = Date, y = Starts, color = "Actual")) +
  geom_line(aes(x = Date, y = predicted_quadratic_month, color = "Fitted")) +
  ggtitle("Actual vs. Fitted Housing Starts") +
  ylab("Housing Starts") +
  xlab("Date") +
  scale_color_manual(values = c("blue", "red"))

#5.
homedat <- mutate(homedat, Month = yearmonth(Date))
HSdat <- as_tsibble(homedat[, c("Month", "Starts")], index = Month)
str(HSdat)

autoplot(HSdat, Starts) +
  ggtitle("Housing Starts Over Time") +
  ylab("Housing Starts") +
  xlab("Month")
# There is no stable trend overall. 
# It continued to decrease from 1983 to 1992, increased from 1990 to 2004, and increased from 2010 to 2020.

gg_season(HSdat) +
  ggtitle("Housing Starts Over Time") +
  ylab("Housing Starts") +
  xlab("Month")
gg_lag(HSdat, Starts)
gg_lag(HSdat, Starts, lags = 1:12)
# Q2 Q3 performs better than Q1 Q3

#6.
mod_drift <- model(HSdat, RW(Starts ~ drift()))
forc_drift <- forecast(mod_drift, h = 60)
mod_snaive <- model(HSdat, SNAIVE(Starts))
forc_snaive <- forecast(mod_snaive, h = 60, level = NULL)

mod_t <- model(HSdat, Seasonal_Naive = SNAIVE(Starts), Drift = RW(Starts ~ drift()))
forc_t <- forecast(mod_t, h = 60)
autoplot(forc_t, HSdat, level = NULL)

accuracy(mod_drift)
accuracy(mod_snaive)
# RMSE of mod_drift: 16.9
# RMSE of mod_snaive: 24.6

aug_drift <- augment(mod_drift)
autoplot(aug_drift, .resid)
aug_snaive <- augment(mod_snaive)
autoplot(aug_snaive, .resid)

#7.
HSdat1_MA5 <- mutate(HSdat, MA5 = slider::slide_dbl(Starts, mean, .before = 2, .after = 2, complete = TRUE))
head(HSdat1_MA5)
SMSE_MA5 <- HSdat1_MA5$Starts - HSdat1_MA5$MA5
RMSE_MA5 <- sqrt(mean(SMSE_MA5^2))
RMSE_MA5
# RMSE_MA5: 10.69274

HSdat1_MA9 <- mutate(HSdat, MA9 = slider::slide_dbl(Starts, mean, .before = 4, .after = 4, complete = TRUE))
SMSE_MA9 <- HSdat1_MA9$Starts - HSdat1_MA9$MA9
RMSE_MA9 <- sqrt(mean(SMSE_MA9^2))
RMSE_MA9
# RMSE_MA9: 17.72286

HSdat1_MA11 <- mutate(HSdat, MA11 = slider::slide_dbl(Starts, mean, .before = 5, .after = 5, complete = TRUE))
SMSE_MA11 <- HSdat1_MA11$Starts - HSdat1_MA11$MA11
RMSE_MA11 <- sqrt(mean(SMSE_MA11^2))
RMSE_MA11
# RMSE_MA11:  20.18822
# MA5 have the best RMSE
# Smaller range models are more likely to fit better because they capture shorter-term fluctuations or variations in the data
# Larger range models smooth out these fluctuations over a longer period

autoplot(HSdat1, Starts) + 
  geom_line(aes(y = MA5), color = "Red") +
  geom_line(data = HSdat1_MA9, aes(y = MA9), color = "Blue") +
  geom_line(data = HSdat1_MA9, aes(y = MA9), color = "Green")

#8. 
mod_cda <- model(HSdat, classical_decomposition(Starts, type = "additive"))
comp_cda <- components(mod_cda)
autoplot(comp_cda)
MSE_cda <- mean(comp_cda$random^2, na.rm = TRUE)
RMSE_cda <- MSE_cda^0.5
RMSE_cda
# RMSE of additive model: 10.35697

mod_cdm <- model(HSdat, classical_decomposition(Starts, type = "multiplicative"))
comp_cdm <- components(mod_cdm)
autoplot(comp_cdm)
MSE_cdm <- mean(comp_cdm$random^2, na.rm = TRUE)
RMSE_cdm <- MSE_cdm^0.5
RMSE_cdm
# RMSE of multiplicative model: 1.001369
# The trend and seasonal effect is quit the same in additive and multiplicative model
# There is trend effect decrease from 1983 to 1992, increased from 1990 to 2004, and increased from 2010 to 2020, other years fluctuate
# There is seasonal effect every year from -30 ~ 20

#9.
mod_STL <- model(HSdat, STL(Starts ~ trend(window = 13) + season(window = "periodic"), robust = TRUE))
comp_STL <- components(mod_STL)
autoplot(comp_STL)

names(comp_STL)
comp_STL$remainder[1:10]
err_STL <- comp_STL$remainder
MSE_STL <- mean(err_STL^2)
RMSE_STL <- MSE_STL^0.5
RMSE_STL
# RMSE of STL decomposition model: 9.305472

#10.
#a.  
# In this dataset, signals of trend and seasonality were detected. 
# The trend indicates varying patterns in housing starts over different time periods, while seasonality reveals recurring patterns at specific times of the year.

#b. 
# Among all models, the STL decomposition model had the lowest RMSE, indicating superior performance in predicting the data.

#c. 
# In this case, the comparison between decomposition and regression models leans towards favoring decomposition models like STL for forecasting needs.
# A time-series decomposition procedure is not strictly a forecasting model. 
# While it provides valuable insights into the underlying components of a time series (trend, seasonality, and residual), it does not directly generate future forecasts. 
# Instead, it aids in understanding the patterns and structures within the data, which can inform the selection and development of forecasting models.

#d. 
# The preferred model (STL decomposition model) may offer effective forecasts to some extent, especially for future data with similar time trends and seasonal patterns.
# However, the accuracy of predictions may decrease when facing new patterns or changes in the future. 
# Therefore, even with the selection of the best model, continuous updates and adjustments are necessary to adapt to new data characteristics.


















