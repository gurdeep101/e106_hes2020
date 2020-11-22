rm(list - ls())
setwd("~/OneDrive/courses/e106/e106_hes2020/lecture_r_codes")

library(olsrr)
library(datasets)
library(leaps) # regsubsets
library(car) # avplot
library(caret) # cross validation

exp <- lm(mpg~disp+hp, data = mtcars)

# test all possible combinations of variables in the regression equation
k <- ols_step_all_possible(exp)
k
plot(k)

# select best subset for each set of number of predictors (x)
k1 <- ols_step_best_subset(exp)
k1
plot(k1)
names(k1) # access attributes with $

k1$adjr

surg1 <- lm(y~., data = surgical)
s1 <- ols_step_best_subset(surg1)
s1
s1$adjr

# forward stepwise regression
# select variables based on p-value
#continues till all variables included
# details = True for depth info
# prem = 0.05 for alpha; more than this removed

exp2 <- lm(y~., data = surgical)
k2 <- ols_step_forward_p(exp2)
k2
plot(k2)

# use details = True for info
ols_step_forward_p(exp2, details = TRUE)

# Backward stepwise regression
# start with all and remove based on p-values
# details = True for depth info
# prem = 0.05 for alpha; more than this removed
k4 <- ols_step_backward_p(exp2)
k4

s4 <- ols_step_backward_p(surg1)
s4
# ols_step_both_p - adds and removes variables
# details = TRUE for details of each step
# pent - values < this enter the model
# prem - values > this exit the model
k5 <- ols_step_both_p(exp2, details = TRUE)
k5

# step forward aic; 
# use AIC to add predictors
k6 <- ols_step_forward_aic(exp2)
k6

# step backward aic
# use AIC to remove predictors
k7 <- ols_step_backward_p(exp2)
k7

# step both using AIC
k8 <- ols_step_both_aic(exp2)

# state.x77 - datasets related to 50 US states
state <- data.frame(state.x77)
# regsubsets - LEAPS function for exhaustive model selection
# uses forward, backward or sequential replacement

# leaps: performs an exhaustive search for the best subsets of the variables 
# x‒method=c("Cp", "adjr2", "r2"): Calculate Cp, adjusted Ra2 or R2
# int=TRUE: Add an intercept to the model
#nbest: Number of subsets of each size to report
# can loop over multiple methods

b <- regsubsets(Life.Exp~., data = state)
rs <- summary(b)
rs$which # ??
rs

# always prefer to choose a smaller model if it meets criteria
f <- lm(Life.Exp~., data = state)
step(f) # choose model based on AIC in stepwise algo

# update - update and refit the model
f1 <- update(f,.~.- Area) # remove area
summary(f1)

# Cross-validation
# used to evaluate model performance
# random split into 2 subsets - train and evaluate

# K-fold CV
# dataset subdivided into K randomly chosen subnets of equal sizes
# Process repeated K times - each subset used 1ce for validation

#LOOCV - K = n; test = 1; train = n-1
# Other techniques - data splitting, bootstrap resampling (with and without replacement)

library(caret)
data("mtcars")
str(mtcars)

# 2 ways to split into train and test
ind <- sample(1:nrow(mtcars), round(0.7*32,1))
train <- mtcars[ind,]
test <- mtcars[-ind,]

# caret function
datasplit <- createDataPartition(y=mtcars$mpg, p = 0.7, list = FALSE)
train <- mtcars[datasplit,]
test <- mtcars[-datasplit,]

# build regression model - 2 methods; lmfit has more info
f <- lm(mpg~., data = train)

# caret
# function sets up a grid of tuning parameters 
# for a number of classification & regression routines
# fits each mode and calculates a resampling based performance measure
lmfit <- train(mpg~., data = train, method = 'lm') 

summary(lmfit)
summary(f)
names(lmfit) # more values
names(f)

# predict fitted values
predict_test <- predict(lmfit, test)

# combine predicted and actual into a df; column names and values
model_test1 <- data.frame(obs = test$mpg, pred = predict_test)

# see accuracy metrics; compare R2 with train and investigate or conclude
defaultSummary(model_test1)

# Cross validation
# k-fold cross validation
control1 <- trainControl(method = 'cv', number = 10) # define parameters for train
lmfit2 <- train(mpg~., data = mtcars, method = 'lm', trControl = control1, metric = 'Rsquared')
summary(lmfit2)

# check on test data
predict_test2 <- predict(lmfit2, test)
model_test2 <- data.frame(obs = test$mpg, pred = predict_test2)
defaultSummary(model_test2)

# LOOCV
control2 <- trainControl(method = 'LOOCV')
lmfit3 <- train(mpg~., data = mtcars, method = 'lm', trControl = control2)
summary(lmfit3)

# variable importance calculation
varImp(lmfit3)
plot(varImp(lmfit3))

# Bootstrap method - 1-- resampling
control3 <- trainControl(method = 'boot', number = 100)
lmfit4 <- train(mpg~., data = mtcars, method = 'lm', trControl = control3)
summary(lmfit4)

pred_test4 <- predict(lmfit4, mtcars) # test
model_test4 <- data.frame(obs = mtcars$mpg, pred = pred_test4)
defaultSummary(model_test4)

# ideal steps
# build the model
# variable selection
# CV - test model is stable
# test eval - out of sample performance

# Added variable plots
datat <- read.csv('Dataset_10TA01.csv')
f1 <- lm(Y~X1+X2, data = datat)
par(mfrow=c(1,3))
plot(datat$X1,resid(f1), pch=16)
abline(0,0,lty = 2, col = 'gray')

## Method 1:X1
f2y <- lm(datat$Y~datat$X2)
f2x1 <- lm(datat$X1~datat$X2)
f2yx1 <- lm(f2y$residuals~f2x1$residuals)

plot(f2y$residuals, f2x1$residuals,col = 'blue', pch = 16, xlab = 'e(X_1|X_2)', ylab = 'e(Y|X_2)')
abline(f2yx1, col = 'red')

# Method2 : using avPlot()
avPlot(model=lm(datat$Y~datat$X1+datat$X2), variable = datat$X1)

# Introduction to olsrr

# Regression
ols_regress(mpg~., data = mtcars)
# detects interaction terms and scales / centers automatically
# flag - iterm = TRUE

datat <- read.csv('Dataset_10TA01.csv')
f1 <- lm(Y~X1+X2, data = datat)

# Measures of influence

# DFFITS Plot - Difference in Fits
# used to identify influential data points
# quantifies number of SD that fitted value changes when data point is omitted
# scaled difference between the ith fitted value obtained from full data
# ith fitted value obtained by deleting the ith observation

ols_plot_dffits(f1)

# Cooks Distance Bar Plot
# used to detect influence on all fitted values of ith case
# outlier will pull model towards it
# large ei or large hii implies larger cooks distance
# bar plot of cooks distance to detect observations that strongly influence fitted values
# used to identify influential data points
# data point with large cooks distance indicates that data point strongly influences fitted values
# depends on both residual and leverage - steps below
  # delete observations one at a time
  # refit the regression model on remaining (n-1) observations
  # examine how much all of fitted values change when ith observation is deleted
ols_plot_cooksd_bar(f1)

# cooks D chart
ols_plot_cooksd_chart(f1)

# DFBETAs Panel 
# DFBetas measure the difference on each parameter estimates (betas)
# with and without influential observation
# +ve or -ve sign implies direction of impact
# value implies sign of difference relative to SD; larve value ==> high impact

ols_plot_dfbetas(f1)

# Studentized residual plot
# Plot to detect outliers
# Deleted residual divided by it estimated SD
# More effective for detecting outlying Y observations than standardized residuals
# > 3 ==> outlier
# perform family of tests for each observation
# if the model is appropriate each deleted studentized residual will follow t- # distribution
# 

# Ho : There is no outlier
# Ha : Outliers are present

# Decision Rule : If |t*| < t(1-alpha/2n;n-p-1) accept Ho; i.e. no outlier

ols_plot_resid_stud(f1)

outlierTest(f1, cutoff = 0.10, n.max = Inf)

# Standardized residual Chart
ols_plot_resid_stand(f)

# studentized residuals vs leverage plot
# graph for detecting influential observations
# y axis - leverage; x axis - outlier
ols_plot_resid_lev(f)

# deleted studentized residual vs fitted value plot
# Graph for detecting outliers
ols_plot_resid_stud_fit(f)

# Potential residual plot
# Plot to aid in classifying unusual observations as high leverage points, outliers or combination of both
ols_plot_resid_pot(f)

# identifying outlying observations using hat values
summary(datat)
summary(f1)

# get hat values & plot
hii_datat <- hatvalues(f1)
hii_datat

n <- length(datat$Y)
p <-  ncol(datat)-1
index <- hii_datat>2*p/n

plot(datat$X2, datat$X1, pch = 16)
text(datat$X1+0.5, datat$X2, labels = as.character(1:length(datat$X1)), col = 'red')
points(datat$X1[index], datat$X2[index], cex = 2.0, col = 'blue')

# hat matrix calculation
x <- model.matrix(f1)
xx_inv <- solve(t(x)%*%x)
hat_datat <- x%*%xx_inv%*%t(x)
hat_datat

# all influence measures - dfbeta, dffit, cook, hat,
influence.measures(f1)
# labels outliers based on built-in functions

# Regression plots 
# Residuals vs Leverage 
# Anything above red lines is an influential point
# Compare outliers from regression plot with influence measures
# compare with cooks distance plot
# Look at relative values apart from height of bar

# compare across plots for select points
# Look for consistency acrss
# QQ, Residual vs fitted, Residual vs Leverage

# Extra Functions

# Residual fit spread plot (QQ Plot) - check? NA
ols_plot_resid_fit_spread(f)

# BP Test - Chi Square test NA
ols_test_breusch_pagan(f)

# collinearity diagnostic NA
ols_coll_diag(f)

