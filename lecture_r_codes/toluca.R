rm(list = ls())
library(onewaytests)
library(lmtest)

setwd("~/OneDrive/courses/e106/e106_hes2020/lecture_r_codes")

toluca <- read.csv('toluca_data.csv')
colnames(toluca) <- c('size', 'hrs')
head(toluca)

fitreg <- lm(hrs~size, data = toluca)
summary(fitreg)
res <- fitreg$residuals

# getting confidence interval
confint(fitreg, level = 0.95)

# predict values 
predict(fitreg, data.frame(size = 100), se.fit = TRUE, interval = 'confidence', level = 0.9)
predict(fitreg, data.frame(size = c(10,20), se.fit = TRUE, interval = 'confidence', level = 0.90)

# get prediction interval; will be wider than confidence interval
predict(fitreg, data.frame(size = 100), se.fit = TRUE, interval = 'prediction', level = 0.90)
predict(fitreg, data.frame(size = c(10,20)), se.fit = TRUE, interval = 'prediction', level = 0.90)

# anova
anova(fitreg)

# fitted values
fitted(fitreg)
fitreg$fitted.values

names(fitreg)

# extract R2; ?sumary.lm for more info
summary(fitreg)$r.squared
summary(fitreg)$adj.r.squared


new.par <- par(mfrow = c(2,2))
plot(toluca$size, res, col = 'red', xlab = 'lot size', ylab = 'residual', main = 'Residual plot against X')
plot(res, col = 'blue', type = 'b', xlab = 'Run', ylab = 'Residual', main = 'Sequence Plot')
boxplot(res, horizontal = TRUE, staplewax = 0.5, col = 'cyan', xlab = 'Residual')
qqplot(qnorm(ppoints(length(res)), mean(res), sd(res)), res,
       xlab = 'Expected', ylab = 'Residual', main = 'Normal Prob Plot', col = 'Purple')
qnorm(ppoints(length(res)), mean(res), sd(res))
new.par

# BF Test

bf.data <- data.frame(cbind(toluca, fitreg$residuals, fitreg$fitted.values))

dimnames(bf.data) # all dim names
dimnames(bf.data)[[1]] # row names
dimnames(bf.data)[[2]] # column names 
dimnames(bf.data)[[2]][3] # 3rd column
dimnames(bf.data)[[2]][3:4] # 3rd and 4th column
dim(bf.data)


dimnames(bf.data)[[2]][3:4] <- c('residuals', 'fitted.values')

bf.data1 <- data.frame(cbind(bf.data, ind = as.factor(I(bf.data$size<=median(bf.data$size))*1)))
dim(bf.data1) # Indicator column added at end on basis of medium

bf.test(residuals~ind, data = bf.data1)
# DEcision Rule
# Ho : Difference is not statistically significant; i.e. error variance is constant
# Ha : Difference is statistically significant; 

# Decision Rule 
# p-value > 0.05 ==> Accept Ho

# BP Test
bptest(fitreg)
# Ho : Errors are normally distributed; no heteroskedasticicty
# Ha : Errors are not normally distributed

# Decision Rule: p-value > 0.05 ==> Accept Ho

# Bonferroni Jt CI
confint(fitreg) # standard CI
confint(fitreg, level = 0.95) # standard CI

confint(fitreg, level = 1-0.1/2) # bonferroni CI for 90% family CI; alpha = -0.90 = 10% LOS

# Working-Hotelling Procedure for simultaneous estimation of mean response; 90% family CI
xh<-data.frame(size=c(30,65,100))

W <- sqrt( 2 * qf(0.90,2,23))
CI<-predict(fitreg, xh, se.fit=TRUE, interval="confidence", level=0.90)
cbind(CI$fit[,1]-W*CI$se.fit, CI$fit[,1] + W*CI$se.fit )


xh <- c(30, 65, 100)
pred <- predict.lm(fitreg, data.frame(size = c(xh)), level = 0.90, se.fit = TRUE ) # can use interval = ' ' , se.fit = TRUE
pred
w <- rep(sqrt(2*qf(0.90, 2, nrow(toluca)-2)), length(xh)) # get f-value and repeat 3 times
wh_final <- rbind(pred$fit - w*pred$se.fit, pred$fit + w*pred$se.fit) # yhat + & - (W * SE of prediction)
wh_final

# Bonferroni procedure for simultaneous estimation of mean response; 90% family CI
xh <- c(30, 65,28 ,55,33,47,100)

predict.lm(fitreg, data.frame(size = c(xh)), interval = 'confidence', level = 1-0.1/length(xh))

pred <- predict.lm(fitreg, data.frame(size = c(xh)), level = 0.90, se.fit = TRUE) # can use interval = ' ', se.fit = TRUE
b <- rep(qt(1-0.90/(2*length(xh)), nrow(toluca)-2), length(xh))
bonf_final <- rbind(pred$fit - b*pred$se.fit, pred$fit + b*pred$se.fit)
bonf_final 

# simultaneous prediction intervals for new observations 
xh <- c(80,100)
g <- length(xh)
alpha <- 0.05

ci.new <- predict.lm(fitreg, data.frame(size = c(xh)), se.fit = TRUE, level = 1-alpha)
ci.new
m <- rbind(rep(qt(1-alpha/(2*g), fitreg$df.residual),g), rep(sqrt(g*qf(1-alpha, g, fitreg$df.residual)),g) ) 
# repeat t-value g times; repeat f-value g times
m

spred <- sqrt(ci.new$residual.scale^2 + (ci.new$se.fit)^2)
spred

# put all values in a transpose matrix
pred.new <- t(rbind(
  'Yh' = xh,
  'spred' = spred,
  'fit' = ci.new$fit,
  'lower.B' = ci.new$fit - m[1,] * spred,
  'upper.B' = ci.new$fit + m[1,] * spred,
  'lower.s' = ci.new$fit - m[2,] * spred,
  'upper.s' = ci.new$fit + m[2,] * spred))

pred.new

# plot confidence band of regressions
fit_yh <- predict(fitreg, data.frame(size = toluca$size), interval = 'confidence', level = 0.90)

plot(toluca$size, toluca$hrs)
abline(fitreg)
lines(toluca$size, fit_yh[,2], lty = 'dashed', col = 'red')
lines(toluca$size, fit_yh[,3], lty = 'dashed', col = 'red')

# Residual diagnostics 
library(olsrr)

# standard regression assumptions
# 1. Error has normal distribution - normality assumption
# 2. Errors have mean zero
# 3. Errors have same but unknown variance - heteroskedasticcity assumption
# 4. Errors are independent of each other

# Residual QQ plot to detect violation of normality assumption
ols_plot_resid_qq(fitreg)

# Residual Normality Test
ols_test_normality(fitreg)

# Correlation between observed residuals and expected residuals under normality
ols_test_correlation(fitreg)

# Residual vs fitted values plot

# Scatterplot to detect non-linearity, unequal error variance & outliers

# Characteristics of a well behaved residual vs fitted plot
# 
# 1. Residuals spread randomly around hte 0 line indicating that the relationship is linear 
# 2. Residuals form an approximated horizontal band around the 0 line indicating homogeneity of error variance.
# 3. No 1 residual is visibly away from the random pattern of the residuals indicating that there are no outliers.

ols_plot_resid_fit(fitreg)

# Residual histogram - to detect violation of normality assumption
ols_plot_resid_hist(fitreg)


