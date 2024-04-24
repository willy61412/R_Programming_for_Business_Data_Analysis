getwd()
setwd("/Users/william/Desktop/Predictive")
dat <- read.csv("hw5_bike_share_day.csv")
head(dat)
library(fpp3)
library(ggplot2)

#1.
dat$dteday <- as.Date(dat$dteday, format = "%m/%d/%Y")
cnts <- ts(dat[,14], frequency = 7)
cntts <- as_tsibble(cnts)
cntts <- mutate(cntts, index = dat[,2])
names(cntts)[2] <- "count"
autoplot(cntts, count)
# The data exhibits seasonal fluctuations with a long-term pattern of alternating increases and decreases.

fit_bike_SES_0.25 <- model(cntts, ETS(count ~ error("A") + trend("N", alpha = 0.25) + season("N")))
fit_bike_SES_0.75 <- model(cntts, ETS(count ~ error("A") + trend("N", alpha = 0.25) + season("N")))
fit_bike_SES_opt <- model(cntts, ETS(count ~ error("A") + trend("N") + season("N")))
fit_bike_naive <- model(cntts, Naive = NAIVE(count))

report(fit_bike_SES_0.25)
accuracy(fit_bike_SES_0.25)
report(fit_bike_SES_0.75)
accuracy(fit_bike_SES_0.75)
report(fit_bike_SES_opt)
accuracy(fit_bike_SES_opt)
report(fit_bike_naive)
accuracy(fit_bike_naive)
# The ETS model (parameter optimization) seems to have a better fit as it has lower RMSE, MAE and MAPE values, as well as a smaller ACF1 value. 
# The Naïve model also performs well, but its prediction error is slightly higher relative to the ETS_opt model.

#2.
fit_bike_Holt_addictive <- model(cntts, ETS(count ~ error("A") + trend("A")))
fit_bike_Holt_damped <- model(cntts, ETS(count ~ error("A") + trend("Ad")))
report(fit_bike_Holt_addictive)
accuracy(fit_bike_Holt_addictive)
report(fit_bike_Holt_damped)
accuracy(fit_bike_Holt_damped)
# The damped additive trend model perform better, with lower RMSE、MAE、MAPE.
# AIC、AICc、BIC are just slightly higher in damped additive trend model than ETS_opt model.

#3.
fit_bike_Holt_addictive <- model(cntts, ETS(count ~ error("A") + trend("A") + season("A")))
fit_bike_Holt_multiplicative <- model(cntts, ETS(count ~ error("A") + trend("A") + season("M")))
fit_bike_Holt_damped_multiplicative <- model(cntts, ETS(count ~ error("A") + trend("Ad") + season("M")))

report(fit_bike_Holt_addictive)
accuracy(fit_bike_Holt_addictive)
report(fit_bike_Holt_multiplicative)
accuracy(fit_bike_Holt_multiplicative)
report(fit_bike_Holt_damped_multiplicative)
accuracy(fit_bike_Holt_damped_multiplicative)
# Holt-Winters Damped Multiplicative perform better than the addictive model and multiplicative model.
# Holt-Winters Damped Multiplicative also perform better than damped additive trend model, because it includes the seasonal changes.

#4.
forc_fit_bike_SES_opt <- forecast(fit_bike_SES_opt, h = 28)
forc_fit_fit_bike_Holt_damped <- forecast(fit_bike_Holt_damped, h = 28)
forc_fit_bike_Holt_damped_multiplicative <- forecast(fit_bike_Holt_damped_multiplicative, h = 28)

autoplot(forc_fit_bike_SES_opt, cntts, colour = "Blue") +
  autolayer(forc_fit_fit_bike_Holt_damped , cntts, colour = "Red") +
  autolayer(forc_fit_bike_Holt_damped_multiplicative , cntts, colour = "Green") +
  labs(y = "Count", title = "Models Comparision",
       x = "Date")

autoplot(forc_fit_bike_SES_opt, cntts,
         level = NULL, colour = "Blue") +
  autolayer(forc_fit_fit_bike_Holt_damped , cntts,
            level = NULL, colour = "Red") +
  autolayer(forc_fit_bike_Holt_damped_multiplicative , cntts,
            level = NULL, colour = "Green") +
  labs(y = "Count", title = "Models Comparision",
       x = "Date")

#5.
data("JohnsonJohnson")
JJts <- as_tsibble(JohnsonJohnson, index = yearquarter())
names(JJts)[2] <- "QE" 
str(JJts)
cat("Data range: ", range(JJts$QE), "\n")
cat("Number of observations (rows): ", nrow(JJts), "\n")
cat("Periodicity of this data set: ", JJts$index[2] - JJts$index[1], "\n")
# Data range:  [0.44, 16.2]
# Number of observations (rows):  84 
# Periodicity of this data set:  1 (every season)

fit_JJ_AAA <- model(JJts, ETS(QE ~ error("A") + trend("A") + season("A")))
report(fit_JJ_AAA)
# Coefficients:
# l[0] = 0.5770136, b[0] = -0.0352184, s[0] = 0.2124434, s[-1] = 0.1273049, s[-2] = -0.3444239, s[-3] = 0.004675636
# The fit:
# Sigma^2: 0.2105, AIC: 250.8828, AICc: 253.3152, BIC: 272.7601

fit_JJ_AAA_augment <- augment(fit_JJ_AAA)
autoplot(fit_JJ_AAA_augment, QE) +
  autolayer(fit_JJ_AAA_augment,.fitted, colour = "Red") +
  autolayer(fit_JJ_AAA_augment,.resid, colour = "Green")

#6.
fit_JJ_AAN_opt <- model(JJts, ETS(QE ~ error("A") + trend("A") + season("N")))
fit_JJ_AAA_addictive <- model(JJts, ETS(QE ~ error("A") + trend("A") + season("A")))
fit_JJ_AAM_multiplicative <- model(JJts, ETS(QE ~ error("A") + trend("A") + season("M")))
fit_JJ_AAdM_damped_multiplicative <- model(JJts, ETS(QE ~ error("A") + trend("Ad") + season("M")))
report(fit_JJ_AAN_opt)
accuracy(fit_JJ_AAN_opt)
report(fit_JJ_AAA_addictive)
accuracy(fit_JJ_AAA_addictive)
report(fit_JJ_AAM_multiplicative)
accuracy(fit_JJ_AAM_multiplicative)
report(fit_JJ_AAdM_damped_multiplicative)
accuracy(fit_JJ_AAdM_damped_multiplicative)
# The fit_JJ_AAA_addictive seems to have a better fit as it has lower RMSE, MAE and MAPE values, as well as a smaller ACF1 value. 

fit_JJ_AAA_addictive_augment <- augment(fit_JJ_AAA_addictive)
autoplot(fit_JJ_AAA_addictive_augment, QE) +
  autolayer(fit_JJ_AAA_addictive_augment,.fitted, colour = "Red") +
  autolayer(fit_JJ_AAA_addictive_augment,.resid, colour = "Green")

forc_fit_JJ_AAA_addictive <- forecast(fit_JJ_AAA_addictive, h = 12)
autoplot(forc_fit_JJ_AAA_addictive, JJts, colour = "Blue") +
  labs(y = "QE", title = "JJts forecast over 3 years",
       x = "Quarter")

#7.
# When the modeler does not specify a certain type of ETS model to be fit, the preferred model is selected based on the algorithm's evaluation of various candidate models according to their fit to the data. 
# The algorithm aims to choose the model that best captures the underlying patterns and structure in the data, considering factors such as the data's trend, seasonality, and error characteristics.

#8.
data("us_employment")
str(us_employment)
usemp <- us_employment
usemp <- filter(usemp, Title == "Total Private")
usemp <- usemp[,c(1,4)]
autoplot(usemp, Employed) 

fit_usemp_AAN_opt <- model(usemp, ETS(Employed ~ error("A") + trend("A") + season("N")))
fit_usemp_AAA_addictive <- model(usemp, ETS(Employed ~ error("A") + trend("A") + season("A")))
fit_usemp_AAM_multiplicative <- model(usemp, ETS(Employed ~ error("A") + trend("A") + season("M")))
fit_usemp_AAdM_damped_multiplicative <- model(usemp, ETS(Employed ~ error("A") + trend("Ad") + season("M")))
report(fit_usemp_AAN_opt)
accuracy(fit_usemp_AAN_opt)
report(fit_usemp_AAA_addictive)
accuracy(fit_usemp_AAA_addictive)
report(fit_usemp_AAM_multiplicative)
accuracy(fit_usemp_AAM_multiplicative)
report(fit_usemp_AAdM_damped_multiplicative)
accuracy(fit_usemp_AAdM_damped_multiplicative)
# The fit_usemp_AAA_addictive seems to have a better fit as it has lower RMSE, MAE and MAPE values, as well as a smaller ACF1 value. 

fit_fit_usemp_AAA_addictive_augment <- augment(fit_usemp_AAA_addictive)
autoplot(fit_fit_usemp_AAA_addictive_augment, Employed) +
  autolayer(fit_fit_usemp_AAA_addictive_augment,.fitted, colour = "Red") +
  autolayer(fit_fit_usemp_AAA_addictive_augment,.resid, colour = "Green")

forc_fit_fit_usemp_AAA_addictive <- forecast(fit_usemp_AAA_addictive, h = 60)
autoplot(forc_fit_fit_usemp_AAA_addictive, usemp, colour = "Blue") +
  labs(y = "Employed", title = "US Employed forecast over 5 years",
       x = "Month")

#9.
goog2015  <- filter(gafa_stock, Symbol == "GOOG", year(Date) == 2015)
goog2015 <- mutate(goog2015, day = row_number())
goog2015 <- update_tsibble(goog2015, index = day, regular = TRUE)  
str(goog2015)

fit_goog2015_AA_opt <- model(goog2015, ETS(Close ~ error("A") + trend("A")))
fit_goog2015_AM_multiplicative <- model(goog2015, ETS(Close ~ error("A") + trend("M")))
fit_goog2015_AAd_damped <- model(goog2015, ETS(Close ~ error("A") + trend("Ad")))
report(fit_goog2015_AA_opt)
accuracy(fit_goog2015_AA_opt)
report(fit_goog2015_AM_multiplicative)
accuracy(fit_goog2015_AM_multiplicative)
report(fit_goog2015_AAd_damped)
accuracy(fit_goog2015_AAd_damped)
# The smallest is the "fit_goog2015_AM_multiplicative" model in terms of AIC, AICc and BIC values, so this model can be considered the best.

forc_fit_goog2015_AM_multiplicative <- forecast(fit_goog2015_AM_multiplicative, h = 30)
autoplot(forc_fit_goog2015_AM_multiplicative, goog2015, colour = "Blue") +
  labs(y = "Close", title = "goog2015 forecast over 30 days",
       x = "Day")

#10.
fit_goog_A <- model(goog2015, ARIMA(Close))
fit_goog_A_2 <- model(goog2015, ARIMA(Close, stepwise = FALSE, approx = FALSE))
fit_goog_A_3 <- model(goog2015, ARIMA(Open ~ pdq(0,1,0)))
report(fit_goog_A)
accuracy(fit_goog_A)
report(fit_goog_A_2)
accuracy(fit_goog_A_2)
report(fit_goog_A_3)
accuracy(fit_goog_A_3)
# fit_goog_A_3 seems to perform slightly better as it has lower RMSE and MAE values

report(fit_goog2015_AM_multiplicative)
accuracy(fit_goog2015_AM_multiplicative)
# fit_goog2015_AM_multiplicative performs slightly better than fit_goog_A_3 as its RMSE and MAE values are slightly lower.
# ME: 0.0762
