rm(list = ls())
library(glmnet) # ridge, lasso, ElasticNet, Logistic, Poisson, Cox
library(olsrr) # regression tests
library(MASS) # robust regression
library(lmtest) # time series
library(Hmisc) # Lag
library(orcutt) # cochrante-orcutt procedure
library(HoRM) # hildreth Lu


setwd("~/OneDrive/courses/e106/e106_hes2020/lecture_r_codes")

body <- read.csv('BodyFat.csv')
head(body)

# define x and y matrix for glmnet
x <- model.matrix(Y~., data = body) [,-c(1)] # drop last column
y <- body$Y

# Use ridge when severe multicollinearity and don't want to eliminate

ridge_mod <- glmnet(x, y, alpha = 0, nlambda = 100, lambda.min.ratio = 0.0001)
# alpha = 1; lasso
# alpha = 0; ridge
# nlambda - number of lambda values
# lambda.min.ratio = smallest value of lambda

# use CV to select best lambda
cv_ridge_mod <- cv.glmnet(x, y, alpha = 0, nlambda = 100, lambda.min.ratio = 0.0001)

par(mfrow = c(1,1))
plot(cv_ridge_mod) 
# cv curve; upper & lower SD;
# vertical line at min lambda; 1 SE of minimum
# lambda.min - minimum of mean CV error

best_lambda_ridge <- cv_ridge_mod$lambda.min
best_lambda_ridge

# predict
predict(ridge_mod, s = best_lambda_ridge, type = 'coefficients')[1:4,]

# LASSO Regression - severe MC and variable selection ok

lasso_mod <- glmnet(x,y, alpha = 1, nlambda = 100, lambda.min.ratio = 0.0001)
plot(lasso_mod, xvar = 'norm', label = TRUE)

# cross validation
cv_lasso_mod <- cv.glmnet(x, y, alpha = 1, nlambda = 100, lambda.min.ratio = 0.0001)
plot(cv_lasso_mod)

best_lambda_lasso <- cv_lasso_mod$lambda.min
best_lambda_lasso

# 2 ways to predict coefficients
coef(cv_lasso_mod, s = 'lambda.min')
predict(lasso_mod, s = best_lambda_lasso, type = 'coefficients')[1:4,]

# ElasticNet - mix of ridge and lasso - use when severe MC
enet_mod <- glmnet(x, y, alpha = 0.5, nlambda = 100, lambda.min.ratio = 0.0001)
plot(enet_mod)

cv_enet_mod <- cv.glmnet(x, y, alpha = 0.5, nlambda = 100, lambda.min.ratio = 0.0001)
plot(cv_enet_mod)

best_lambda_enet <- cv_enet_mod$lambda.min
best_lambda_enet

# get coefficients
coefficients(enet_mod, s = best_lambda_enet)
coef(cv_enet_mod, s = 'lambda.min')

# IRLS - Iteratively reweighed least squares
# used to mitigate impact of outliers
blood <- read.csv('Blood.Pressure.csv')

# step1 - simple regression
g <- lm(Y~X, data = blood)
summary(g)
par(mfrow = c(2,2))
plot(g)

# step 2 - get residuals 
abs_ei <- abs(g$residuals)

# step 3 - regress residuals on X; get weights & fitted values
g1 <- lm(abs_ei~blood$X)
plot(g1)
summary(g1)

s <- g1$fitted.values

# weight = residual of (fitted values)^2 of lm(res~X)  
wi = 1/(s^2)

# perform regression using weights
g2 <- lm(Y~X, weights = wi, data = blood)
plot(g2)
summary(g2)

# 1 more iteration
abs_ei <- abs(g2$residuals)
g3 <- lm(abs_ei~blood$X)
summary(g3)
plot(g3)

s <- g2$fitted.values
wi <- 1/(s^2)

g4 <- lm(Y~X, weights = wi, data = blood)
summary(g4)
plot(g4)

# Robust Regression - use when outliers; not for MC

# cooks distance chart
ols_plot_cooksd_chart(g)

# studentized residual plot
ols_plot_resid_stud(g)

# studentized residuals vs leverage plot
ols_plot_resid_lev(g)

# deleted studentized residual vs fitted values
ols_plot_resid_stud(g)

# robust regression using rlm

# using huber weights
rr_huber <- rlm(Y~., data = blood)
summary(rr_huber)

# create df of weights
hweights <- data.frame(obs = c(1:54), resid = rr_huber$residuals, weight = rr_huber$w)

hweights2 <- hweights[order(rr_huber$w),]
hweights2[1:15,]

# using bisquare weights
rr_bisq <- rlm(Y~X, psi = psi.bisquare, data = blood)
summary(rr_bisq)

biweights <- data.frame(obs = c(1:54), resid = rr_bisq$residuals, weight = rr_bisq$w)

biweights2 <- biweights[order(rr_bisq$w),]
biweights2[1:15,] # high value outliers have low weights

cbind(g$coefficients, rr_huber$coefficients, rr_bisq$coefficients)

#When comparing the results of a regular OLS regression and a robust regression, if the results are 
#very different, you will most likely want to use the results from the robust regression. 

#Large differences suggest that the model parameters are being highly influenced by outliers.

#Different functions have advantages and drawbacks. Huber weights can have difficulties with severe 
#outliers, and bisquare weights can have difficulties converging or may yield multiple solutions.

######################
# Autoregressive model
######################

blaise <- read.csv('Blaisdell.csv')
head(blaise)

# durbin-watson test for autocorrelation
dwtest(Company.Sales~Industry.Sales, data = blaise)

# cochrane-orcutt procedure
# manually
f <- lm(Company.Sales~Industry.Sales, data = blaise)
summary(f)
et <- f$residuals
et1 <- Lag(et, shift = 1)
cbind(et, et1) # visually verify lag

d1 <- sum(na.omit(et1*et)) # delete missing points before sum
d2 <- sum(na.omit(et1)^2)
rho <- d1/d2

ytnew <- blaise$Company.Sales - rho*Lag(blaise$Company.Sales, shift = 1)
xtnew <- blaise$Industry.Sales - rho*Lag(blaise$Industry.Sales, shift = 1)

f1 <- lm(ytnew~xtnew)
summary(f1)

dwtest(ytnew~xtnew)

# using library orcutt
coch <- cochrane.orcutt(f)
# no convergence for above

# change criteria to 4th decimal
coch1 <- cochrane.orcutt(f, convergence = 4, max.iter = 100)
summary(coch1)

# Hildreth Lu

prg1 <- function(x, y, rh) {
  # function to run Hildreth-Lu method over grid for rh
  n <- length(rh)
  # define matrix
  out <- matrix(0, nrow = n, ncol = 2)
  # 1st column has rh value
  out[,1] <- rh
  for (i in 1:n){
    d <- anova(hildreth.lu(y = y, x = x, rho = rh[i]))
    # store sse result in 2nd column
    out[i,2] <- d$'Sum Sq'[2]
  }
  out
}

rh <- seq(0.1, 1, by = 0.01)
h1 <- prg1(blaise$Company.Sales, x = blaise$Industry.Sales, rh)
which.min(h1[,2])
# return minimum value of rh and sse
h1[which.min(h1[,2]),]

# First Difference

# assume rho = 1 & calculate lag
rho = 1
ytnew <- blaise$Company.Sales - rho*Lag(blaise$Company.Sales, shift = 1)
xtnew <- blaise$Industry.Sales - rho*Lag(blaise$Industry.Sales, shift = 1)

# perform regression
f1 <- lm(ytnew~xtnew - 1)
summary(f1)

# check using DW
dwtest(f1)

# Prediction
f <- lm(Company.Sales~Industry.Sales, data = blaise)
et <- f$residuals
et1 <- Lag(et, shift = 1)

d1 <- sum(na.omit(et1*et))
d2 <- sum(na.omit(et1)^2)

rho <- d1/d2
rho

ytnew <- blaise$Company.Sales - rho*Lag(blaise$Company.Sales, shift = 1)
xtnew <- blaise$Industry.Sales - rho*Lag(blaise$Industry.Sales, shift = 1)

f1 <- lm(ytnew~xtnew)
summary(f1)
mse <- summary(f1)$sigma^2

 # transforming coefficients back to original form

# get intercept
b0 <- summary(f1)[[4]][1,1]/(1-rho); b0
s_bo <- summary(f1)[[4]][1,2]/(1-rho); s_bo

# get coefficient
b1 <- summary(f1)[[4]][2,1]; b1
s_b1 <- summary(f1)[[4]][2,2]; s_b1

yhat_correct <- b0 + b1*blaise$Company.Sales
yhat_correct

# Point Forecasts
xn_plus1 <- 175.3
xn <- rev(blaise$Industry.Sales)[1]; xn

yhat_nplus1 <- b0+b1*xn_plus1; yhat_nplus1

yn <- rev(blaise$Company.Sales)[1]; yn
en <- yn - (b0+b1*xn); en

yhat_forecast_nplus1 <- yhat_nplus1 + rho*en
yhat_forecast_nplus1

# Forecast Interval
xprime <- xtnew
xbar_prime <- mean(xprime[-1]) # drop 1st value which is NA
xn_plus1_prime <- xn_plus1 - rho*xn

alpha = 0.05
n <- length(blaise$Company.Sales)

spred <- sqrt(mse*(1 + (1/n) + (xn_plus1_prime - xbar_prime)^2/(sum((xprime[-1]-xbar_prime)^2))))
spred

pred_l <- yhat_forecast_nplus1 - qt(1-alpha/2, df = n-3)*spred; pred_l
pred_u <- yhat_forecast_nplus1 + qt(1-alpha/2, df = n-3)*spred; pred_u
