#1.
setwd("/Users/william/Desktop/Predictive")
dat <- read.csv("hw3_hour.csv")
head(dat)
str(dat)

mnth <- as.factor(dat$mnth)
season <- as.factor(dat$season)
hr <- as.factor(dat$hr)
wkday <- as.factor(dat$wkday)
weathersit <- as.factor(dat$weathersit)
tmp_mnth <- data.frame(model.matrix(~mnth-1))
tmp_season <- data.frame(model.matrix(~season-1))
tmp_hr <- data.frame(model.matrix(~hr-1))
tmp_wkday <- data.frame(model.matrix(~wkday-1))
tmp_weathersit <- data.frame(model.matrix(~weathersit-1))
dat1 <- cbind(dat[,c(15,4)], tmp_season[,1:3], 
              tmp_mnth[,1:11], dat[,c(9, 7)], 
              tmp_wkday[,1:6], tmp_hr[,1:23], 
              tmp_weathersit[,2:4], dat[,11:14])
rm(mnth, season, hr, wkday, weathersit)
rm(tmp_mnth, tmp_season, tmp_hr, tmp_wkday, tmp_weathersit)
str(dat1)
model_all1 <- lm(cnt ~ ., data = dat1)
summary(model_all1)
library(corrplot)
cor_matrix_dat1 <- cor(dat1) 
corrplot(cor_matrix_dat1, method = "color")

dat2 <- subset(dat1, select = -c(season1, season2, season3, wkday0, atemp, wkday5))
str(dat2)
model_all <- lm(cnt ~ ., data = dat2)
summary_model_all <- summary(model_all)
RMSE_all <- summary_model_all$sigma
RMSE_all
cor_matrix_dat2 <- cor(dat2) 
corrplot(cor_matrix_dat2, method = "color")
# I remove all the variables that have correlation higher than 0.7 and lower than -0.7
# Season is highly related to month, weekday0 = workday, temp is highly related to atemp, weekday5 has no coefficient in the model.

#2.
# Set seed and split the data
set.seed(14274637)
train <- sample(nrow(dat2), 0.5 * nrow(dat2))
dat2_train <- dat2[train,]
dat2_test <- dat2[-train,]

# Fit the model on the training data
reg_train <- lm(cnt ~ ., data = dat2_train)
summary_reg_train <- summary(reg_train)
summary_reg_train
RMSE_train <- summary_reg_train$sigma
RMSE_train

# Evaluate it on the test data
yhat_test <- predict(reg_train, newdata = dat2_test)
RSS_test <- sum((dat2_test$cnt - yhat_test)^2)
MSE_test <- RSS_test / nrow(dat2_test)
RMSE_test <- sqrt(MSE_test)
RMSE_test
# RMSE for the test data (103.2335) is slightly higher than the RMSE for the training data (102.0986)

#3.
library(leaps)
str(dat2_train)
regfit.full <- regsubsets(cnt ~ ., data = dat2_train)
summary(regfit.full)
correlation_values <- cor(dat2[, -1], dat2$cnt)
correlation_values <- as.vector(correlation_values)
barplot(correlation_values, main = "Correlation with cnt", names.arg = colnames(dat2)[-1])

independent_vars2 <- c("yr", "mnth1", "mnth2", "mnth6", "mnth7", "mnth8", "mnth9", "wkday4", "hr7", "hr8", "hr16", "hr17", "hr18", "temp", "hum", "windspeed")
model_best <- lm(cnt ~ ., data = dat2_train[, c("cnt", independent_vars2)])

summary_model_best <- summary(model_best)
summary_model_best
RMSE_train_best <- summary_model_best$sigma
RMSE_train_best
anova_result <- anova(reg_train, model_best)
anova_result
# R squared:0.5319, RMSE:124.82
# Model_best is simpler. I choose the high related variables except for the period from midnight to early morning
# Because I think it is common sense not to rent bicycles during this period, there is no need to put it in the model


#4.
library(boot)
dat2_best <- dat2[, c("cnt", independent_vars2)]
glm.1 <- glm(cnt ~ ., data = dat2_best)
summary(glm.1)
RMSE.glm.1 <- sqrt(sum(glm.1$residuals^2) / glm.1$df.residual)
RMSE.glm.1
cv.err <- cv.glm(dat2_best, glm.1)
cv.err$delta
MSE.LOOCV <- cv.err$delta[2]
RMSE.LOOCV <- MSE.LOOCV^0.5
RMSE.LOOCV

cv.err.5 <- cv.glm(dat2_best, glm.1, K = 5)
MSE.5 <- cv.err.5$delta[2]
RMSE.5 <- MSE.5^0.5
RMSE.5

cv.err.10 <- cv.glm(dat2_best, glm.1, K = 10)
MSE.10 <- cv.err.10$delta[2]
RMSE.10 <- MSE.10^0.5
RMSE.10

MSE.table <- data.frame(matrix(0,1,5))
names(MSE.table) <- c("All Data-All Variables", "Valid Set","LOOCV", "5-Fold", "10-Fold")
MSE.table[1,] <- c(RMSE_all, RMSE_test, RMSE.LOOCV, RMSE.5, RMSE.10)
MSE.table
# I can't get my RMSE.LOOCV because cv.err is not working.
# RMSE_all:102.54, RMSE_test:102.09, RMSE.5: 123.64, RMSE.10:123.60
# The higher values of RMSE_5 and RMSE_10 may be due to uneven data, improper selection of equilibrium models, and overfitting.

#5.
str(dat2_train)
regfit.full <- regsubsets(cnt ~ ., data = dat2_train)
summary(regfit.full)
independent_vars3 <- c("yr","hr7", "hr8", "hr16", "hr17", "hr18","hr19", "temp", "hum")
model_resubset <- lm(cnt ~ ., data = dat2_train[, c("cnt", independent_vars3)])
summary_model_resubset <- summary(model_resubset)
model_resubset
RMSE_train_resubset <- summary_model_resubset$sigma
RMSE_train_resubset
# Model_resubset pick eight variables that is most moderated. 
# The model with eight variables has the highest R squared compared to the model with fewer variables.
# RMSE_train_resubset: 124.181

#6.
# Forward Stepwise Regression
regfit.fwd <- regsubsets(cnt ~ ., data = dat2_train, nvmax = 14, method = "forward")
summary_regfit.fwd <- summary(regfit.fwd)
coef(regfit.fwd, 8)
coef(regfit.full, 8)
independent_vars_fwd <- c("yr","hr7", "hr8", "hr17", "hr18","hr19", "temp", "hum")
model_fwd <- lm(cnt ~ ., data = dat2_train[, c("cnt", independent_vars_fwd)])
summary_model_fwd <- summary(model_fwd)
summary_model_fwd
RMSE_fwd <- summary_model_fwd$sigma
RMSE_fwd

# Backward Stepwise Regression
regfit.bwd <- regsubsets(cnt ~ ., data = dat2_train, nvmax = 14, method = "backward")
summary(regfit.bwd)
coef(regfit.bwd, 8)
coef(regfit.full, 8)
independent_vars_bwd <- c("yr","hr7", "hr8", "hr16", "hr17","hr18", "hr19", "temp")
model_bwd <- lm(cnt ~ ., data = dat2_train[, c("cnt", independent_vars_bwd)])
summary_model_bwd <- summary(model_bwd)
summary_model_bwd
RMSE_bwd <- summary_model_bwd$sigma
RMSE_bwd
# The results of forward stepwise and backward stepwise regression differ because of their different selection procedures. 
# Forward stepwise regression selects the variable that improves the fit the most
# Backward stepwise regression selects the variable that reduces the fit the least

#7.
library(glmnet)
X_train_best <- model.matrix(cnt ~ ., data = dat2_train[, c("cnt", independent_vars2)])[,-1]
y_train_best <- dat2_train$cnt
X_test_best <- model.matrix(cnt ~ ., data = dat2_test[, c("cnt", independent_vars2)])[,-1]
y_test_best <- dat2_test$cnt
ridge_mod_best <- cv.glmnet(X_train_best, y_train_best, alpha = 0, lambda = grid, thresh = 1e-12)
ridge.pred <- matrix(0, nrow = length(y_test_best), ncol = 100)
testerr <- matrix(0, nrow = 100, ncol = 1)
for (j in 1:100) {
  ridge.pred[,j] <- predict(ridge_mod_best, s = grid[j], newx = X_test_best)
  testerr[j] <- mean((ridge.pred[,j] - y_test_best)^2)
}
best_lambda_index <- which.min(testerr)
best_lambda_index
best_lambda <- grid[best_lambda_index]
best_lambda
ridge_mod_best$lambda[81]
testerr[81]
RMSE.R.81 <- testerr[81]^0.5
RMSE.R.81
# Best lambda: 0.2064418
# Test RMSE for Ridge Regression with Best Model: 122.2951

#8.
cv.out1 <- cv.glmnet(X_train_best, y_train_best, alpha = 1)
plot(cv.out1)
best_lambda2 <- cv.out1$lambda.min
best_lambda2
lasso_mod_best <- glmnet(X_train_best, y_train_best, alpha = 1, lambda = best_lambda2, thresh = 1e-12)
lasso_pred <- predict(lasso_mod_best, newx = X_test_best)
RSS_lasso <- sum((y_test_best - lasso_pred)^2)
MSE_lasso <- RSS_lasso / nrow(X_test_best)
RMSE_lasso <- sqrt(MSE_lasso)
RMSE_lasso
# Best lambda: 0.1713916
# Test RMSE for LASSO Regression with Best Model: 122.3076

#9.
# Model All
yhat_test_model_all <- predict(reg_train, newdata = dat2_test)
MSE_test_model_all <- mean((dat2_test$cnt - yhat_test_model_all)^2)
# Model Best
yhat_test_model_best <- predict(model_best, newdata = dat2_test)
MSE_test_model_best <- mean((dat2_test$cnt - yhat_test_model_best)^2)
# Model LOOCV
yhat_test_model_LOOCV <- predict(glm.1, newdata = dat2_test)
MSE_test_model_LOOCV <- mean((dat2_test$cnt - yhat_test_model_LOOCV)^2)
# Model 5-Fold Cross-Validation
cv.err_5 <- cv.glm(dat2_best, glm.1, K = 5)
MSE_5 <- cv.err_5$delta[2]
# Model 10-Fold Cross-Validation
cv.err_10 <- cv.glm(dat2_best, glm.1, K = 10)
MSE_10 <- cv.err_10$delta[2]
# Model Resubset
yhat_test_model_resubset <- predict(model_resubset, newdata = dat2_test)
MSE_test_model_resubset <- mean((dat2_test$cnt - yhat_test_model_resubset)^2)
# Model Forward Stepwise
yhat_test_model_fwd <- predict(model_fwd, newdata = dat2_test)
MSE_test_model_fwd <- mean((dat2_test$cnt - yhat_test_model_fwd)^2)
# Model Backward Stepwise
yhat_test_model_bwd <- predict(model_bwd, newdata = dat2_test)
MSE_test_model_bwd <- mean((dat2_test$cnt - yhat_test_model_bwd)^2)
# Model Ridge
ridge_pred <- predict(ridge_mod_best, newx = X_test_best, s = best_lambda)
MSE_test_model_ridge <- mean((y_test_best - ridge_pred)^2)
# Model LASSO 
lasso_pred <- predict(lasso_mod_best, newx = X_test_best, s = best_lambda2)
MSE_test_model_lasso <- mean((y_test_best - lasso_pred)^2)

cat("Test MSE for Model All (Multiple Linear Regression):", MSE_test_model_all, "\n")
cat("Test MSE for Model Best (Subset Regression):", MSE_test_model_best, "\n")
cat("Test MSE for Model LOOCV (GLM):", MSE_test_model_LOOCV, "\n")
cat("Test MSE for Model 5-Fold Cross-Validation (GLM):", MSE_5, "\n")
cat("Test MSE for Model 10-Fold Cross-Validation (GLM):", MSE_10, "\n")
cat("Test MSE for Model Resubset (Subset Regression with Selected Variables):", MSE_test_model_resubset, "\n")
cat("Test MSE for Model Forward Stepwise Regression:", MSE_test_model_fwd, "\n")
cat("Test MSE for Model Backward Stepwise Regression:", MSE_test_model_bwd, "\n")
cat("Test MSE for Model Ridge Regression:", MSE_test_model_ridge, "\n")
cat("Test MSE for Model LASSO Regression:", MSE_test_model_lasso, "\n")
# The inclusion of all variables in the model resulted in the lowest test MSE.
# Automated model selection did not lead to a notable improvement in predictive performance. 
# Ridge and LASSO did not provide significant advantages over the simpler models. 

#10.
# Importance of Feature Selection: 
# Observing the performance of different models, it is recommended that the company focuses on selecting features that significantly contribute to the predictive target. 
# In-depth understanding of the impact of each feature can enhance the prediction accuracy of the model and reduce unnecessary computational costs.

# Complexity in Model Selection: 
# The complexity of the model has a significant impact on prediction results. 
# It is suggested that the company carefully considers the simplicity and interpretability of the model during the selection process to avoid overfitting and improve the model's generalization ability.

# Application of Cross-Validation: 
# Cross-validation is a crucial means of evaluating model performance, especially in situations where the dataset is limited. 
# It is recommended that the company extensively applies cross-validation techniques, such as 5-Fold and 10-Fold, during the model evaluation process to obtain more reliable assessments of the model's generalization ability.
