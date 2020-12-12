rm(list = ls())
setwd("~/OneDrive/courses/e106/e106_hes2020/lecture_r_codes")

library(C50) # decision tree
library(partykit) # better visualization
library(gmodels) # cross table
library(randomForest) #RF
library(vcd) # confusion matrix
library(rpart) # regression tree
library(rpart.plot) # plots of decision trees

# load and EDA
credit <- read.csv('credit.csv')
str(credit)
head(credit)
table(credit$checking_balance)
summary(credit$checking_balance) # nothing since factor
table(credit$savings_balance)
summary(credit$months_loan_duration)
summary(credit$amount)

# change defaults to yes or no
table(credit$default)
credit$default <- factor(credit$default, levels = c('1', '2'), labels = c('No','Yes'))
table(credit$default)

# data prep - 90% train-test split
nrow(credit)
set.seed(123)
train_sample <- sample(nrow(credit),900)
str(train_sample)

train <- credit[train_sample,]
test <- credit[-train_sample,]

# check for correct train test split
prop.table(table(train$default))

# train decision tree model
# remove target column from X and specify after comma
credit_model <- C5.0(train[-17], train$default)
summary(credit_model)

plot(credit_model)

# evaluate model performance
credit_pred <- predict(credit_model, test)

# use cross table from gmodels
CrossTable(test$default, credit_pred, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('actual default', 'predicted default'))

#################################
# Boosting to improve performance
#################################

# add additional trials parameter; upper limit
# trials - number of separate decision trees to use in boosted team

credit_boost10 <- C5.0(train[-17], train$default, trials = 10)
summary(credit_boost10)

# credit_boost10 <- C5.0(train[-17], train$default,trials = 10)
# summary(credit_boost10)

# predict and check
credit_boost_pred10 <- predict(credit_boost10, test)

CrossTable(test$default, credit_boost_pred10, 
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, 
           dnn = c('actual default', 'predicted default'))

# Making some mistakes cost more than others

matrix_dimensions <- list(c('No', 'Yes'), c('No', 'Yes'))
names(matrix_dimensions) <- c('predicted', 'actual')

# specify cost for mistakes by category
error_cost <- matrix(c(0, 1, 4, 0), nrow = 2, dimnames = matrix_dimensions)
error_cost

credit_cost <- C5.0(train[-17], train$default, costs = error_cost )
credit_cost_pred <- predict(credit_cost, test)

CrossTable(test$default, credit_cost_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

# RandomForest
# RNGversion('3.5.2')
set.seed(300)
rf <- randomForest(default~., data = credit)
rf

Kappa(rf$confusion[1:2, 1:2])

##################
# Regression Tree
##################

wine <- read.csv('whitewines.csv')
str(wine)
head(wine)
hist(wine$quality) # normal distribution
summary(wine) # check for NA and outliers

# 70 - 30 train test split
set.seed(1023)
ind <- sample(nrow(wine), round(0.7 * nrow(wine)))
train <- wine[ind,]
test <- wine[-ind,]

# Run tree
wine_tree <- rpart(quality~., data = train)
wine_tree
# For each node, number of samples reaching decision point is listed
# e.g 3249 at root, 2168 alcohol < 10.85, 1249 > 0.73
# alcohol 1st use ==> most important predictor
# Terminal nodes resulting in prediction - indicated by *
# quality value at end

# plot
rpart.plot(wine_tree, digits = 3)

# fallen leaves - forces the leaf nodes to be 
# aligned at the bottom of the plot
# type and extra parameters - affect the way decision nodes are labeled

rpart.plot(wine_tree, digits = 5, fallen.leaves = TRUE,
           type = 2, extra = 101)

# Evaluating model performance
wine_pred <- predict(wine_tree, test)
summary(wine_pred)
summary(test$quality)
cor(wine_pred, test$quality)
# However, the correlation only measures how strongly the 
#predictions are related to the true value; it is not a measure of how far off the predictions were from 
#the true values.

# Measure performance with MAE
mae <- function(actual, predicted) {
  mean(abs(actual-predicted))
}

# Measure performance with SSE
sse <- function(actual, predicted) {
  sum((actual - predicted)^2)
}

# MEasure performance with RSq
r2 <- function(actual, predicted) {
  1-sum((actual - predicted)^2)/((length(actual)-1)*var(actual))
}

# test MAE
mae(test$quality, wine_pred)

# test SSE
sse(test$quality, wine_pred)

# test R2
r2(test$quality, wine_pred)
1-r2(test$quality, wine_pred)



