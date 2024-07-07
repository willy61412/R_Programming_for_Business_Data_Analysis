getwd()
setwd("/Users/william/Desktop/Predictive")
dat <- read.csv("hw7_bank_term_deposit_big.csv")
names(dat)
str(dat)

#1.
#  Create factor for some variables
jb <- as.factor(dat$job)
mari <- as.factor(dat$marital)
ed <- as.factor(dat$education)
cntct <- as.factor(dat$contact)
m <- as.factor(dat$month)
pout <- as.factor(dat$poutcome)
#  Convert variables to indicators
#  Note some variables have 2 levels and some have more
tmp_job <- data.frame(model.matrix(~jb - 1))
tmp_marit <- data.frame(model.matrix(~mari - 1))
tmp_educ <- data.frame(model.matrix(~ed - 1))
tmp_contact <- data.frame(model.matrix(~cntct - 1))
tmp_month <- data.frame(model.matrix(~m - 1))
tmp_poutcome <- data.frame(model.matrix(~pout - 1))
dat$loan <- as.numeric(as.factor(dat$loan)) - 1
dat$default <- as.numeric(as.factor(dat$default)) - 1
dat$housing <- as.numeric(as.factor(dat$housing)) - 1
dat$deposit <- as.numeric(as.factor(dat$deposit)) - 1
#  Take care of “pdays”
pdaysnew <- ifelse(dat$pdays != -1, dat$pdays, 0)
#  Bind stuff together in a new data frame
names(dat)
dat1 <- cbind(dat[,c(17,1,5:8,10,12:13,15)],
              tmp_job[,1:11], tmp_marit[,1:2], tmp_educ[,1:3],
              tmp_contact[,1:2], tmp_month[,1:11], 
              tmp_poutcome[,1:3],
              data.frame(pdaysnew))
names(dat1)
#  Get rid of junk for simplicity
rm(tmp_job, tmp_marit, tmp_contact,
   tmp_month, tmp_poutcome, tmp_educ)
rm(jb, mari, ed, cntct, m, pout, pdaysnew)
str(dat1)

#a.
# The response variable ("Y") represents whether a client subscribed to a term deposit or not. 
# It is found in the first position of dat1.

#b.
# The "pdays" variable represents the number of days since the client was last contacted. 
# But it has a value of -1 for clients who were not previously contacted. 
# A transformation is applied where -1 values are replaced with 0, indicating that the client was not previously contacted.

#c.
# The script involves transforming categorical variables into indicator variables, 
# converting binary categorical variables into numeric indicators, 
# and handling missing values in the "pdays" variable by replacing them with zeros.

#2.
#a.
correlation_matrix <- cor(dat1)
correlation_matrix
deposit_correlation <- correlation_matrix["deposit", ]
deposit_correlation
# Duration (0.394):
# Longer durations of contact indicate deeper engagement, potentially leading to a higher likelihood of making a term deposit.
# Previous (0.307):
# Customers contacted more frequently in the past may respond more positively to the current campaign.
# Pdaysnew (0.103):
# Customers contacted further in the past and re-contacted may have had more time to consider the offer, 
# increasing the likelihood of making a term deposit.

#b.
set.seed(112233)
deposit_yes <- dat1[dat1$deposit == 1, ]
deposit_no <- dat1[dat1$deposit == 0, ]
train.yes <- sample(1:nrow(deposit_yes),2000)
train.no <- sample(1:nrow(deposit_no),2000)
dat.train <- rbind(deposit_yes[train.yes,],deposit_no[train.no,])

#c.
deposit_yes_new <- deposit_yes[-train.yes,]
deposit_no_new <- deposit_no[-train.no,]
test.yes <- sample(1:nrow(deposit_yes_new),1000)
test.no <- sample(1:nrow(deposit_no_new),1000)
dat.test <- rbind(deposit_yes_new[test.yes,],deposit_no_new[test.no,])

#3.
#a.
logreg <- glm(deposit ~ ., data = dat.train, family = binomial)
summary(logreg)
# At a 99.9% confidence level, significant variables include duration, housing, loan, cntctcellular, cntcttelephone, 
# mapr, maug, mfeb, mjan, mjul, mjun, mmay, mnov, moct, poutother, and poutsuccess.

#b.
yhat.train <- predict(logreg, dat.train, type = "response")
yhat.train.plus.act <- cbind(yhat.train, dat.train$deposit)
yhat.train.class <- ifelse(yhat.train > 0.5, 1, 0)
yhat.train.class[1:20]
tab.lr1.train <- table(dat.train$deposit, yhat.train.class, dnn = c("Actual","Predicted"))
tab.lr1.train
lr1.train.err <- (tab.lr1.train[1,2] + tab.lr1.train[2,1])/sum(tab.lr1.train)
lr1.train.err
# The overall error rate is 0.15675.

#4.
#a.
logreg2 <- glm(deposit ~ duration + previous + pdaysnew, data = dat.train, family = binomial)
summary(logreg2)
# Durationm previous and pdaysnew are the top three variables that realted to deposit.
yhat.train2 <- predict(logreg2, dat.train, type = "response")
yhat.train2.class <- ifelse(yhat.train2 > 0.5, 1, 0)

#b.
tab.lr2.train <- table(dat.train$deposit, yhat.train2.class, dnn = c("Actual","Predicted"))
tab.lr2.train
lr1.train.err2 <- (tab.lr2.train[1,2] + tab.lr2.train[2,1])/sum(tab.lr2.train)
lr1.train.err2
# The overall error rate is 0.2505.

#5.
#a.
library(MASS)
lda.fit <- lda(deposit ~ ., data = dat.train)
lda.fit

#b.
lda.pred <- predict(lda.fit, dat.test)
lda.pred$posterior
names(lda.pred)
lda.pred$class
lda.test.class <- lda.pred$class
tab.lda <- table(dat.test$deposit, lda.test.class, dnn = c("Actual", "Predicted"))
tab.lda
err.lda <- mean(dat.test$deposit != lda.test.class)
err.lda
# The overall error rate is 0.191

#6.
#a.
library(e1071)
nb.fit <- naiveBayes(deposit ~ ., data = dat.train)
nb.fit

#b.
nb.class <- predict(nb.fit, newdata = dat.test)
nb.class
tab.nb <- table(dat.test$deposit, nb.class, dnn = c("Actual", "Predicted"))
tab.nb
err.nb <- mean(dat.test$deposit != nb.class)
err.nb
# The overall error rate is 0.319

#7.
#a.
library(class)
dat.train.x <- dat.train[,2:43]
dat.train.y <- dat.train[,1]
dat.test.x <- dat.test[,2:43]
dat.test.y <- dat.test[,1]
out1 <- knn(dat.train.x, dat.test.x, dat.train.y, k=1)
tab.knn1 <- table(dat.test.y, out1, dnn = c("Actual", "Predicted"))
tab.knn1
knn1.err <- mean(dat.test.y != out1)
knn1.err
# The overall error rate is 0.297

#b.
k_values <- c(3, 5, 7, 9, 11)
knn_errors <- numeric(length(k_values))
for (i in 1:length(k_values)) {
  out <- knn(dat.train.x, dat.test.x, dat.train.y, k = k_values[i])
  knn_errors[i] <- mean(dat.test.y != out)
}
for (i in 1:length(k_values)) {
  cat("k =", k_values[i], "\n")
  cat("Error rate:", knn_errors[i], "\n\n")
}
# k=5 has the lowest overall error rate 0.2565

#8.
#a.
library(tree)
dat.train[,1] <- as.factor(dat.train[,1])
dat.test[,1] <- as.factor(dat.test[,1])
tree1 <- tree(deposit~., data = dat.train)

#b.
tree.pred.tst <- predict(tree1, dat.test, type = "class")
table(dat.test$deposit, tree.pred.tst, dnn = c("Actual", "Predicted"))
err.tree <- mean(dat.test$deposit != tree.pred.tst)
err.tree
# The overall error rate is 0.2335

#9.
#a.
prune1 <- prune.misclass(tree1)
names(prune1)
plot(prune1)
plot(prune1$size, prune1$dev, xlab = "Size of Tree", ylab = "Deviation")
prune.tree1 <- prune.misclass(tree1, best = 4)
summary(prune.tree1)
prune.tree1
plot(prune.tree1)
text(prune.tree1, pretty = 0)

#b.
pt1.pred <- predict(prune.tree1, dat.test, type = "class")
table(dat.test$deposit, pt1.pred, dnn = c("Actual", "Predicted"))
err.tree2 <- mean(dat.test$deposit != pt1.pred)
err.tree2
# When the tree size is 4, it is the smallest size while maintaining an overall error rate of 0.2335

#10.
#a.
# The pruned version of the tree model performed the best on the test set with an error rate of 0.2335

#b.
# Pruning the tree simplifies its structure, avoiding overfitting and improving the model's generalization ability.
# Feature selection is crucial in tree models. The pruned tree might have made better feature selections leading to more accurate predictions.
# Pruning removes branches that are heavily influenced by noise, making the model more robust.

#c.
# The models identify "duration," "previous," and "pdaysnew" as crucial factors influencing term deposit decisions.
# By categorizing customers into potential term depositors and non-depositors, the bank can tailor marketing strategies and services to suit each group's needs effectively.
# Recognizing that models evolve, ongoing evaluation and adjustment of features, parameters, or algorithms are essential for maintaining and improving prediction accuracy over time.
















