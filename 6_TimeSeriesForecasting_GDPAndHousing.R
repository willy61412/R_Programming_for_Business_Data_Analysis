getwd()
setwd("/Users/william/Desktop/Predictive")
dat1 <- read.csv("hw6_one_family_homes.csv")
library(fpp3)

# 1.
# a. White noise
set.seed(333222)
wn_time <- ts(data.frame(rnorm(200)))
wnts <- as_tsibble(wn_time)
plot(wn_time, xlab = "Time", ylab = "Value")
acf_wn <- acf(wn_time)
pacf_wn <- pacf(wn_time)
print(acf_wn)
print(pacf_wn)
# The values at each time point are independent, showing no evident autocorrelation or trend.

# b. AR(1) with parameter, ϕ1 = 0.6
ysim1 <- ts(data.frame(matrix(rep(0),200,1)))
ysim1[1,1] <- wn_time[1]
for (i in 2:200) {
  ysim1[i,1] <- 0.6*ysim1[i-1,1] + wn_time[i] 
}
ysim1 <- as_tsibble(ysim1)
autoplot(ysim1, value)
ACF(ysim1, value)
autoplot(ACF(ysim1, value))
PACF(ysim1, value)
autoplot(PACF(ysim1, value))
# There is a significant autocorrelation at lag 1, 
# indicating a direct relationship between the current observation and the preceding one.

# c. AR(2) with parameters, ϕ1 = 0.6 and ϕ2 = 0.3
ysim2 <- ts(data.frame(matrix(rep(0),200,1)))
ysim2[1,1] <- wn_time[1]
ysim2[2,1] <- wn_time[2]
for (i in 3:200) {
  ysim2[i,1] <- 0.6*ysim2[i-1,1] + 0.3*ysim2[i-2,1] + wn_time[i] 
}
ysim2 <- as_tsibble(ysim2)
autoplot(ysim2, value)
ACF(ysim2, value)
autoplot(ACF(ysim2, value))
PACF(ysim2, value)
autoplot(PACF(ysim2, value))
# There is a significant autocorrelation at lags 1 and 2, 
# indicating a direct relationship between the current observation and the two preceding observations.

# d. AR(2) with parameters, ϕ1 = 0.6 and ϕ2 = -0.3
ysim3 <- ts(data.frame(matrix(rep(0),200,1)))
ysim3[1,1] <- wn_time[1]
ysim3[2,1] <- wn_time[2]
for (i in 3:200) {
  ysim3[i,1] <- 0.6*ysim3[i-1,1] - 0.3*ysim3[i-2,1] + wn_time[i] 
}
ysim3 <- as_tsibble(ysim3)
autoplot(ysim3, value)
ACF(ysim3, value)
autoplot(ACF(ysim3, value))
PACF(ysim3, value)
autoplot(PACF(ysim3, value))
# There is some autocorrelation at lags 1 and 2, indicating a certain level of dependency between consecutive observations. 
# At lag 2, the negative partial autocorrelation coefficient of -0.258 suggests a negative correlation with the two preceding observations.

# e. MA(1) with parameter, θ1 = 0.6
ysim4 <- ts(data.frame(matrix(rep(0),200,1)))
ysim4[1,1] <- wn_time[1]
for (i in 2:200) {
  ysim4[i,1] <- wn_time[i] + 0.6*wn_time[i-1] 
}
ysim4 <- as_tsibble(ysim4)
autoplot(ysim4, value)
ACF(ysim4, value)
autoplot(ACF(ysim4))
PACF(ysim4, value)
autoplot(PACF(ysim4))
# There is some autocorrelation at lag 1
# The partial autocorrelation function shows a negative correlation at lag 2.

# f.	ARMA(1,1) with parameters, ϕ1 = 0.5 and θ1 = 0.4
ysim5 <- ts(data.frame(matrix(rep(0),200,1)))
ysim5[1,1] <- wn_time[1]
ysim5[2,1] <- wn_time[2]
for (i in 3:200) {
  ysim5[i,1] <- wn_time[i] + 0.5*wn_time[i-1] - 0.4*wn_time[i-2] 
}
ysim5 <- as_tsibble(ysim5)
autoplot(ysim5, value)
ACF(ysim5, value)
autoplot(ACF(ysim5))
ACF(ysim5, value)
autoplot(PACF(ysim5))
# There is a significant correlation at lags 1 and 2, fading thereafter, 
# reflecting the combined effects of autoregression and moving average components.

# g.	ARIMA(1,1,1) with parameters, ϕ1 = 0.5 and θ1 = 0.4
fit_2 <- model(wnts, ARIMA_g = ARIMA(value ~ pdq(1,1,1), fixed = list( ar1 = 0.5, ma1 =0.4)))
autoplot(wnts, value)
ACF(wnts, value)
autoplot(ACF(wnts))
PACF(wnts, value)
autoplot(PACF(wnts))
# The result in a certain level of correlation at lags 1 and 2. 
# As the lag increases, the correlation gradually diminishes, leading to increased randomness in the data.

# h.	ARIMA(1,1,1)(0,1,0)[4]  with parameters, ϕ1 = 0.5 and θ1 = 0.4
diff_series <- diff(wn_time, lag = 4)
ysim7 <- ts(data.frame(matrix(rep(0), 200, 1)))
ysim7[1, 1] <- diff_series[1]
ysim7[2, 1] <- diff_series[2]
for (i in 3:200) {
  ysim7[i, 1] <- diff_series[i] + 0.5 * diff_series[i-1] - 0.4 * diff_series[i-2] + 0.4 * ysim7[i-1, 1]
}
ysim7 <- as_tsibble(ysim7)
autoplot(ysim7, value)
ACF(ysim7, value)
autoplot(ACF(ysim7))
PACF(ysim7, value)
autoplot(PACF(ysim7))
# There is a strong positive autocorrelation at lag 1, while significant negative correlations are observed at lags 2 and 4.

# 2.
# a.
dat2 <- read.csv("hw6_USGDP.csv")
str(dat2)
GDPt <- ts(dat2[,2], frequency = 4, start = 1947)
GDPts <- as_tsibble(GDPt)
names(GDPts)[2] <- "GDP"
str(GDPts)
autoplot(GDPts, GDP) 
# GDP has a long-term stable upward trend
features(GDPts, GDP, features = guerrero)
lambda <- pull(features(GDPts, GDP, features = guerrero), lambda_guerrero)
lambda
GDPts <- mutate(GDPts, GDPT = box_cox(GDP, lambda)) 

# b.
fit_GDP <- model(GDPts, ARIMA(box_cox(GDP, lambda)))
aug_GDP <- augment(fit_GDP)

# c.
fit_3 <- model(GDPts,
               ARIMA1 = ARIMA(GDPT ~ pdq(1,0,0)),
               ARIMA2 = ARIMA(GDPT ~ pdq(1,0,1)),
               ARIMA3 = ARIMA(GDPT ~ pdq(2,0,1)))
glance(fit_3)

# d.
# ARIMA3 has the lowest values of AIC, AICc, and BIC, suggesting that it may be the best-performing model among the three.
report(select(fit_3, ARIMA3))
gg_tsresiduals(select(fit_3,ARIMA3))
# Residuals: 0.06403
# The model is well fits the data.

# e.
fit_4 <- model(GDPts,ARIMA3 = ARIMA(GDPT ~ pdq(2,0,1)))
forc_4 <- forecast(fit_4, h = 20)
autoplot(forc_4, GDPts)

# f.
fit_ETS <- model(GDPts, ETS(GDPT))
report(fit_ETS)
gg_tsresiduals(fit_ETS)
# The ARIMA3 model has lower AIC, AICc, and BIC values and is therefore the better performing model.

# 3.
# a.
dat3 <- read.csv("hw6_one_family_homes.csv")
str(dat3)
dat3$Date <- as.Date(dat3$Date, format = "%m/%d/%Y")
home <- ts(dat3[,2], frequency = 12)
homets <- as_tsibble(home)
names(homets)[2] <- "Sales"
autoplot(homets, Sales)
# PACF dropped sharply after one month, and the data fluctuated in the short term, 
# with a trend of rising then falling sharply and then rising again.
ACF(homets, Sales)
PACF(homets, Sales)
features(homets, Sales, features = guerrero)
lambda_2 <- pull(features(homets, Sales, features = guerrero), lambda_guerrero)
lambda_2
homets <- mutate(homets, Sales = box_cox(Sales, lambda_2))

# b.
fit_Sales <- model(homets, ARIMA(box_cox(Sales, lambda_2)))
aug_Sales <- augment(fit_Sales)

# c.
fit_5 <- model(homets,
               ARIMA1 = ARIMA(home ~ pdq(1,0,0)),
               ARIMA2 = ARIMA(home ~ pdq(1,0,1)),
               ARIMA3 = ARIMA(home ~ pdq(2,0,1)))
glance(fit_5)

# d.
# ARIMA2 has the lowest values of AIC, AICc, and BIC, suggesting that it may be the best-performing model among the three.
report(select(fit_5, ARIMA2))
gg_tsresiduals(select(fit_5,ARIMA2))
# Residuals: 2005
# The model is well fits the data.

# e.
fit_4 <- model(GDPts,ARIMA3 = ARIMA(GDPT ~ pdq(2,0,1)))
forc_4 <- forecast(fit_4, h = 20)
autoplot(forc_4, GDPts)

# f.
fit_ETS_2 <- model(homets, ETS(home))
report(fit_ETS_2)
gg_tsresiduals(fit_ETS_2)
# The ARIMA2 model has lower AIC, AICc, and BIC values and is therefore the better performing model.
