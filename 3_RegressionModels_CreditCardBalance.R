getwd()
setwd("/Users/william/Desktop/Predictive")
dat <- read.csv("hw2_credit_card_balance_new.csv")

#a.
dat$Gender <- ifelse(dat$Gender == "Female",1,0)
names(dat)[8] <- "Female"
dat$Married <- as.numeric(as.factor(dat$Married)) - 1
dat$Student <- as.numeric(as.factor(dat$Married)) - 1
Eth <- factor(dat$Ethnicity)
tempEth <- data.frame(model.matrix(~Eth-1))
newdat <- cbind(dat, tempEth)
print(newdat)
dat2 <- newdat[,c(2:10, 13:17, 12)]
names(dat2)
print(dat2)

#b.
correlation_matrix <- cor(dat2)
cor_with_balance <- correlation_matrix["Balance", -1]
print(cor_with_balance)
#Top three most strongly correlated variables are, Rating,Limit, Num_Cards 
#Credit rating often reflects an individual's credit risk. A higher credit rating may indicate lower credit risk, and banks are more willing to provide a higher credit limit.
#Credit limit represents the maximum amount a user can borrow. Therefore, users with higher credit limits may have more purchasing and spending power, potentially resulting in a higher credit card balance.
#The number of credit cards a user has may indicate their access to credit. More credit cards could mean more available credit for various purchases, leading to a higher credit card balance.

#c.
independent_vars <- c("Rating", "Limit", "Num_Cards")
model_1 <- lm(Balance ~ ., data = dat2[, c("Balance", independent_vars)])
summary(model_1)
# RSE: 235.6
# R-squared: 0.7371
# Adjusted R-squared:  0.7353 
# F-statistic: 416.8
# Num_Cardss is significant (p = 0.068)
# Intercept: Predicted balance when all predictors are zero (may not be meaningful in this context).
# Rating and Limit: Changes in these variables are not statistically significant.
# Num_Cards: Predicted increase of approximately 17 units in balance for a one-unit increase in the number of cards.
# The overall model is statistically significant

#d.
model_all <- lm(Balance ~ ., data = dat2)
summary(model_all)
# The significant varibles are Income, Limit, Rating, Num_Cards, Age, Student, Eth3 
# RSE: 167.4
# R-squared: 0.8699
# Adjusted R-squared:  0.8663 
# F-statistic: 243.5
# The overall model is statistically significant
anova_result <- anova(model_1, model_all)
anova_result
#The p-value for the F-test comparing "model1" and "model_all" is extremely small, indicating a highly significant result.
#The significant result suggests that "model_all" provides a significantly better fit to the data compared to "model 1" in terms of building a model to predict balance

#e.
library(leaps)
str(dat2)
regfit.full <- regsubsets(Balance ~ ., data = dat2)
summary(regfit.full)
independent_vars2 <- c("Income", "Limit","Rating" ,"Num_Cards","Age", "Yrs_Ed", "Married","Eth3","Eth5")
model_best <- lm(Balance ~ ., data = dat2[, c("Balance", independent_vars2)])
summary(model_best)
# Using "leaps", we found that the model with those variables have the highest R squared.
anova_result2 <- anova(model_best, model_all)
anova_result2
# It is as effective as "model all" for predicting Balance.(Pr > F)
obs_250 <- dat2[250, c("Income", "Limit", "Rating", "Num_Cards", "Age", "Yrs_Ed", "Married", "Eth3", "Eth5")]
prediction_interval <- predict(model_best, newdata = obs_250, interval = "prediction", level = 0.95)
print(prediction_interval)
#The interval for the observation at index 250 is approximately (375.0331, 1036.844)

#f.
dat2$Age_squared <- dat2$Age^2
model_poly <- lm(Balance ~ Age + Age_squared + Income + Limit + Rating + Num_Cards + Yrs_Ed + Female + Student + Married + Eth1 + Eth2 + Eth3 + Eth4 + Eth5, data = dat2)
summary(model_poly)
#R-squared:  0.8702
#p-value: < 2.2e-16
#A curvilinear effect might be expected if there's a belief that the relationship between a quantitative predictor (e.g., Age) and Balance is not linear but follows a curve.
dat2$Rating_Income_interaction <- dat2$Rating * dat2$Income
model_interaction <- lm(Balance ~ Rating + Income + Rating_Income_interaction + Limit + Num_Cards + Age + Yrs_Ed + Female + Student + Married + Eth1 + Eth2 + Eth3 + Eth4 + Eth5, data = dat2)
summary(model_interaction)
#R-squared:  0.8736
#p-value: < 2.2e-16
#Rating_Income_interaction is significant, it suggests that the effect of Rating on Balance is different at different levels of Income.

#g.
set.seed(975246)
train_indices <- sample(1:nrow(dat2), 300)
train_data <- dat2[train_indices,]
test_data <- dat2[-train_indices,]
model_train <- lm(Balance ~ ., data = train_data)
summary(model_train)
predictions_train <- predict(model_train, newdata = train_data)
rss_train <- sum((train_data$Balance - predictions_train)^2)
mse_train <- mean((train_data$Balance - predictions_train)^2)
rmse_train <- sqrt(mse_train)
cat("RSS (Residual Sum of Squares) for model_train:", rss_train, "\n")
cat("MSE (Mean Squared Error) for model_train:", mse_train, "\n")
cat("RMSE (Root Mean Squared Error) for model_train:", rmse_train, "\n")
predictions_best <- predict(model_best, newdata = dat2)
rss_best <- sum((dat2$Balance - predictions_best)^2)
mse_best <- mean((dat2$Balance - predictions_best)^2)
rmse_best <- sqrt(mse_best)
cat("RSS (Residual Sum of Squares) for model_best:", rss_best, "\n")
cat("MSE (Mean Squared Error) for model_best:", mse_best, "\n")
cat("RMSE (Root Mean Squared Error) for model_best:", rmse_best, "\n")
#Comparing two regression models, lower values for RSS, MSE and RMSE indicate better performance, thus model_train perform better.

#h.
predictions_test <- predict(model_train, newdata = test_data)
rss_test <- sum((test_data$Balance - predictions_test)^2)
mse_test <- mean((test_data$Balance - predictions_test)^2)
rmse_test <- sqrt(mse_test)
cat("RSS (Residual Sum of Squares) for model_train on test data:", rss_test, "\n")
cat("MSE (Mean Squared Error) for model_train on test data:", mse_test, "\n")
cat("RMSE (Root Mean Squared Error) for model_train on test data:", rmse_test, "\n")
#Model_train may have exhibited overfitting or higher variance on the test data, leading to a larger RMSE and MSE.

#i.
predictions_test_all <- predict(model_all, newdata = test_data)
rss_test_all <- sum((test_data$Balance - predictions_test_all)^2)
mse_test_all <- mean((test_data$Balance - predictions_test_all)^2)
rmse_test_all <- sqrt(mse_test_all)
cat("RSS (Residual Sum of Squares) for model_all on test data:", rss_test_all, "\n")
cat("MSE (Mean Squared Error) for model_all on test data:", mse_test_all, "\n")
cat("RMSE (Root Mean Squared Error) for model_all on test data:", rmse_test_all, "\n")
#Model_all seems to strike a better balance between bias and variance on the test data, resulting in lower RMSE and MSE.

#j.
#The bank's managers should be informed that "model_train" demonstrates effective predictive performance, showing lower error metrics on both the training and test datasets. 
#Strategies for management may involve incorporating a well-balanced model that generalizes well to new data, monitoring for potential overfitting, and considering regular updates to ensure continued accuracy. 
#Confidence in predictions is reasonably high; however, additional information on evolving customer behaviors, economic conditions, or external factors could further enhance predictive capabilities and contribute to a more robust credit card balance prediction model.


