rm(list = ls())

library(readxl)
library(onewaytests)
library(MASS)

prostate <- read_excel('prostate_dat.xls')
head(prostate)
colnames(prostate)
dim(prostate)

# take samples
set.seed(1023)
ind <- sample(c(1:97),68)
samp <- prostate[ind,]

# regression of psa on cavol - cancer volume
# key assumptions to check for 
  # normal linear regression model is appropriate
  # estimated errors (residuals) are independent from X & Y_hat
  # Errors are iid ~ N(0,sigma)
  # Error terms have non-constant variance and mean = 0
y <- samp$psa
x1 <- samp$cavol
plot(x1, y, xlab = 'cancer vol', ylab = 'PSA level')
f <- lm(y~x1)
summary(f)

ei <- f$residuals
yhat <- f$fitted.values

newer.par = par(mfrow = c(2,2))
plot(x1, ei, xlab = 'Cancer Volume', ylab = 'Residual')
plot(f)

plot(x1, y, xlab = 'Cancer Volume', ylab = 'PSA Level')
abline(f)
plot(yhat, ei, xlab = 'Predicted Values', ylab = 'Residuals')
boxplot(ei, horizontal = TRUE, staplewax = 0.5, col = 3, xlab = 'Residual')

error.std = rstandard(f)
qqnorm(error.std, ylab = 'Standardized Residuals', xlab = 'Normal Scores')
newer.par

# remediation 1
boxcox(f, lambda = seq(0.1, 0.7, 0.1))
