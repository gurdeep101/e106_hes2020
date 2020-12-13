rm(list = ls())
setwd("~/OneDrive/courses/e106/e106_hes2020/lecture_r_codes")

library(ggplot2) # graphics

# read in
data(whiteside, package = 'MASS')
head(whiteside)
str(whiteside) # before / after is factor with 2 levels
summary(whiteside)

# plot

# before / after scatterplot - scatterplot split on factor insulation; add line
ggplot(aes(x=Temp, y = Gas), data = whiteside) + geom_point() + facet_grid(~ Insul) + geom_smooth(method = 'lm')

# plot shows that less gas is used after insulation
# fit a linear model - interaction of 2 terms

lmod <- lm(Gas~Temp*Insul, data = whiteside)
summary(lmod)

# We would predict that gas consumption would fall by 
# 0.393 for 1°C incr in temperature before insulation. 
# After insulation, fall in consumption per degree is only 0.393 - 0.115 = 0.278.
# But the interpretation for the other two parameter estimates is more problematic
# since these represent predicted consumption when the temperature is zero. 
# This is on the lower edge of the observed range of temperatures and 
# would not represent a typical difference. 
# For other datasets, a continuous predictor value of zero might be far outside 
# the range and so these parameters would have little practical meaning.
# The solution is to center the temperature predictor by its mean and recompute t

# Center temperature by mean
# centering allows a more natural interpretation
whiteside$ctemp <- whiteside$Temp - mean(whiteside$Temp)
lmodc <- lm(Gas ~ ctemp*Insul, whiteside)
summary(lmodc)

# plot factors with more than 2 levels
library(faraway)
data(fruitfly, package = 'faraway')
head(fruitfly)

# plot with levels of activity

# all plots in a single chart - 2 ways of plotting
plot(longevity~thorax, fruitfly, pch = unclass(activity))
plot(fruitfly$thorax, fruitfly$longevity, pch = unclass(fruitfly$activity))

# separate plots
ggplot(aes(x=thorax, y = longevity), data = fruitfly) + geom_point() + facet_wrap(~activity)

# regression with interaction terms - separate interaction for each factor term
lmod <- lm(longevity~thorax*activity, data = fruitfly)
summary(lmod)
# 5 factors give 4 activity levels - intercept includes base / reference level

# check for regression assumptions
par(mfrow = c(2,2))
plot(lmod)
par(mfrow = c(1,1))

# funnel shape of residual vs fitted plot indicates heteroskedasticity
anova(lmod) # shows that interaction term is not significant

# since summary above shows that interaction terms are not significant we redo regression
lmodp <- lm(longevity~thorax+activity, data = fruitfly)
summary(lmodp)

# Interpretation
# activity has 4 levels - isolated, one, low, many, high
# isolated included in base level
# regression for isolated = -50.242 + 136.1268 * thorax
# regression for one
# longevity = intercept + activityOne+ (thorax_coefficient * thorax_activityOne_coefficient) * thorax
# longevity = -50.242 + 6.5172 + (136.1268-4.6771) * thorax

drop1(lmodp, test = 'F') # drop test for significance

# plot resiudals vs fitted based on factors
plot(residuals(lmodp)~fitted(lmodp), pch = unclass(fruitfly$activity), xlab = 'Fitted', ylab = 'Residuals', abline(h=0))

# Perform log transform to remove heteroskedasticity
lmodl <- lm(log(longevity) ~ thorax+activity, fruitfly)
summary(lmodl)

plot(residuals(lmodl) ~ fitted(lmodl),pch=unclass(fruitfly$activity), xlab="Fitted",ylab="Residuals")
abline(h=0)

exp(coef(lmodl)[3:6])

lmodh <- lm(thorax ~ activity, fruitfly)
summary(lmodh)
anova(lmodh)

lmodu <- lm(log(longevity) ~ activity, fruitfly)
summary(lmodu)
anova(lmodu)

# vif to detect mulitcollinearity
vif(lmodp)
# all values near 1 ==> no MC
# >10 or 5 ==> seious MC
# if MC exists then use domain knowledge to drop

#######################
# Polynomial Regression
#######################

# Power transform
power <- read.csv('Power Cell.csv')
head(power)

# centering variables - subtract the mean
x1 <- scale(power$X1, scale = FALSE)
x2 <- scale(power$X2, scale = FALSE)
y <- power$Y

# check
range(x1); range(x2)
cbind(head(x1), head(x2))

# w/o scaling
f1 <- lm(Y~., data = power)
summary(f1)
vif(f1)
# all values near 1 ==> no MC
# >10 or 5 ==> seious MC
# if MC exists then use domain knowledge to drop

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

# Ho : No curvature effects are needed; i.e. only linear model
# Ha : Curvature effects are needed; interaction & polynomial terms needed

# from anova test we see that
# For full model SS Inc by 3096
# F-val = 1.1604 of full model
# p-val = 0.3993 of full model which is > 0.05 
# Hence accept Ho & conclude that curvature & interaction effects are not needed
# Interpretation of p-value
# p-value : Assuming that Ho = True; 
#         : probability of seeing extreme value 
#         : low p-value ==> low prob of seeing extreme value
#         : Reject Ho

# confidence interval
g <- 2; # num coefficients  
alpha <- 0.1
confint(lm(Y~X1+X2, power), level = 1-(alpha/g))

#library(alr3) # for lack of fit
#pureErrorAnova(f)

# -1 / +1 encoding
data("sexab", package = 'faraway')
head(sexab)
colnames(sexab)
str(sexab)

fs1 <- lm(ptsd~csa, data = sexab) # 0, 1 coding
summary(fs1) # 0, 1 coding

contrasts(sexab$csa) <- contr.sum(2)
fs2 <- lm(ptsd~csa, data = sexab) # +1 -1 coding
summary(fs2) # -1, +1 coding

# compare coefficients
fs1$coefficients; fs2$coefficients

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


