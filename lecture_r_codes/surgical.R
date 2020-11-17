rm(list - ls())
setwd("~/OneDrive/courses/e106/e106_hes2020/lecture_r_codes")

library(olsrr)
library(datasets)
library(leaps) # regsubsets

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


