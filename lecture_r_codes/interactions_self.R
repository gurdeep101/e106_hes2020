rm(list = ls())
setwd("~/OneDrive/courses/e106/e106_hes2020/lecture_r_codes")

library(ggplot2)

# read in
data(whiteside, package = 'MASS')
head(whiteside)
str(whiteside)
summary(whiteside)

# plot

# before / after scatterplot - scatterplot split on factor insulation
ggplot(aes(x=Temp, y = Gas), data = whiteside) + geom_point() + facet_grid(~ Insul) + geom_smooth(method = 'lm')

# plot shows that less gas is used after insulation
# fit a linear model - interaction of 2 terms

lmod <- lm(Gas~Temp*Insul, data = whiteside)
summary(lmod)

# Center temperature by mean
# centering allows a more natural interpretation
whiteside$ctemp <- whiteside$Temp - mean(whiteside$Temp)
lmodc <- lm(Gas~ctemp, data = whiteside)
summary(lmodc)

# plot factors with more than 2 levels
library(faraway)
data(fruitfly, package = 'faraway')
head(faraway)

# plot with levels of activity

# all plots in a single chart - 2 ways
plot(longevity~thorax, fruitfly, pch = unclass(activity))
plot(fruitfly$thorax, fruitfly$longevity, pch = unclass(fruitfly$activity))

# separate plots
ggplot(aes(x=thorax, y = longevity), data = fruitfly) + geom_point() + facet_wrap(~activity)

# regression with interaction terms - separate interaction for each factor term
lmod <- lm(longevity~thorax*activity, data = fruitfly)
summary(lmod)
# 5 factors give 4 activity levels - intercept includes base / reference level

# since summary above shows that interaction terms are not significant we redo regression
lmodp <- lm(longevity~thorax+activity, data = fruitfly)
summary(lmodp)

# plot resiudals vs fitted based on factors
plot(residuals(lmodp)~fitted(lmodp), pch = unclass(fruitfly$activity), xlab = 'Fitted', ylab = 'Residuals', abline(h=0))

# vif to detect mulitcollinearity
vif(lmodp)
# all values near 1 ==> no MC
# >10 or 5 ==> seious MC
# if MC exists then use domain knowledge to drop

# Polynomial Regression

# Power transform
power <- read.csv('Power Cell.csv')
# centering variables - subtract the mean
x1 <- scale(power$X1, scale = FALSE)
x2 <- scale(power$X2, scale = FALSE)
y <- power$Y

# w/o scaling
f1 <- lm(Y~., data = power)
summary(f1)
vif(f1)

# after scaling
f2 <- lm(y~x1+x2)
summary(f2)
vif(f2)

# polynomial
f3 <- lm(y~x1+I(x1^2), data = power)
summary(f3)
vif(f3)

x11 <- x1^2
x22 <- x2^2
x12=x1*x2

f11 <- lm(y~x1+x11)
summary(f11)
vif(f11)

# polynomial and interaction terms
f <- lm(y~x1+x2+x11+x22+x12)
summary(f)
vif(f)

# No MC but quadratic and interaction terms not significant
# can we drop them??

# use anova comparison test
# build model with only linear effect - reduced model
f22 <- lm(y~x1+x2)
summary(f22)
vif(f22)

anova(f22, f) # reduced model, full model
# from anova test we see that
# For full model SS Inc by 3096
# F-val = 1.1604
# p-val = 0.3993 which is > 0.05 
# Hence reject Ha & conclude that curvature & interaction effects are not needed
# Interpretation of p-value
# p-value : Assuming that Ho = True; 
#         : probability of seeing extreme value 
#         : low p-value ==> low prob of seeing extreme value
#         : Reject Ho

# confidence interval
g <- 2; alpha <- 0.1
confint(lm(Y~X1+X2, power), level = 1-(alpha/g))

#library(alr3) # for lack of fit
#pureErrorAnova(f)

# -1 / +1 encoding
data("sexab", package = 'faraway')
head(sexab)
fs1 <- lm(ptsd~csa, data = sexab) # 0, 1 coding
summary(fs1) # 0, 1 coding

contrasts(sexab$csa) <- contr.sum(2)
fs2 <- lm(ptsd~csa, data = sexab) # +1 -1 coding
summary(fs) # 0, 1 coding

# compare coefficients
fs1$coefficients
fs2$coefficients

# compare model matrix
model.matrix(fs1) # 0, 1 coding
# interpretation
# when not abused value decreases by 7.245
# when abused value in intercept
# makes interpretation of coefficient difficult
# hence better to use +1 -1 coding when interpretation is important

# interpretation 
model.matrix(fs2) # +1 -1 coding
# interpretation 
# since coefficient takes +1 or -1 value; makes comparison easier
# when abused PTSD increases by 3.62; when not abused dec by 3.622

# multi-level factors

cdi_3a <- data.frame(cbind(active_phy = cdi$Number.of.active.physicians,
                           tot_pop = cdi$Total.population,
                           tot_inc = cdi$Total.personal.income,
                           geo = cdi$Geographic.region))

str(cdi_3a) # geo is an integer; need to convert to factor
cdi_3a$geo[1:10]

cdi_3a$geo <- factor(cdi_3a$geo, labels=c('W', 'NC', 'S', 'NE'), ordered = FALSE)
str(cdi_3a)
cdi_3a$geo[1:10]

cdi_3a <- cdi_3a[order(cdi_3a$geo, decreasing = TRUE),]

reg3a <- lm(active_phy~.,data = cdi_3a)
summary(reg3a)


