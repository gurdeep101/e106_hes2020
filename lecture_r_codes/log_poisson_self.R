rm(list = ls())

library(faraway) # cigs data for log reg
library(MASS) # negative binomial
library(ResourceSelection) # goodness of fit test

data(wcgs, package = 'faraway')
str(wcgs)

# logistic regression
lmod <- glm(chd ~ height + cigs, family = binomial, data = wcgs)
summary(lmod)

# height coef = 0.02521 = 2.6%
# cigs coef = 0.02313 = 2.3%

beta <- coef(lmod); beta # extract coefficient
exp(beta); # <1 ==> decreasing effect; >1 ==> increasing effect
# This is odds ratio

# Conclusion
# Odds of heart disease inc by 2.6% for each additional incr in height 
# By 2.3% for each additional cigarette smoked per day

# Deviance - equivalent to F-Test
# NULL Deviance - Assume no coefficients / variables = 1781
# Devvance for current model using 2 variables = 32 = 1781 - 1749
# residual deviance = 1749

# compute effect of smoking 20 packs a day
exp(beta[3]*20) 
# ans = 1.588; implies 59% increase in odds of heart disease due to smoking

# Test for significance of model; i.e. both variables combined
# Ho : Variables are not important
# Ha : Variables are important
1-pchisq(32.2, 2) # 1.01826e-07

# p-value is small reject Ho; accept Ha
# conclude that there is some relationship between predictors & response

# check if height significant; 
anova(lmod, test = 'Chi')

# can we drop height
# build reduced model
lmodc <- glm(chd ~ cigs, family = binomial, data = wcgs)
summary(lmodc)

# anova of reduced and full model
anova(lmodc, lmod, test = 'Chi')
# Ho : Height can be dropped
# Ha : Height cannot be dropped

# In reduced model (w/o height) deviance incr from 1749 to 1750
# p-value = 0.3374; greater than 5%; i.e. large p-value
# fail to reject Ho
# conclude that height is not significant

# Drop function
# performs full / reduced test for each variable
drop1(lmod, test = 'Chi')

# same conclusion - height can be dropped

# Confidence Intervals
confint(lmod) # of coefficients

# Prediction
test <- data.frame(cbind(height = 72, cigs = 0))
predict(lmod, test, type = 'response', se.fit = TRUE)

#predint(lmod) not present
predict(lmod, test)
exp(predict(lmod,test)) # same as predict output

wcgs$bmi <- with(wcgs, 703*wcgs$weight/(wcgs$height^2))
lmodr <- glm(chd ~ age + height + weight + bmi + sdp + dbp + chol + dibep + cigs + arcus, family=binomial, wcgs)
summary(lmodr)
drop1(lmodr, test = 'Chi')

# arcus, height and weight are not significant.
# we drop them 1 at a time starting with highest p-value
# Hence, arcus is dropped as per drop test
# Keep going till you get all significant results

lmodr1 <- glm(chd ~ age + height + weight + sdp + dbp + chol + dibep + cigs, family=binomial, wcgs)
summary(lmodr1)

# Goodness of Fit Test

# Ho : Model is a good fit
# Ha : Model is not a good fit

# Put y, fitted values and separate data into groups
# test will split data into groups and compare each group
# performs chi square test for each group
# 
hoslem.test(lmod$y, fitted(lmod), g = 5)

# Accept Ho - Fit is good; since p > 0.05

####################################################
# Poisson Regression & Negative Binomial Regression
####################################################
# Both used to count data
# e.g. large count outcomes aka rare events
# neg binom more robust to many zeros
# does not have constraint that mean and variance equal to each other
# Negative binomial more flexible

head(gala) # galpagos in faraway

# predict reln betn number of plant species and geo variables

modp <- glm(Species ~ ., family = poisson, data = gala)
summary(modp)

drop1(modp, test = 'Chi')

modn <- glm(skips ~ ., negative.binomial(1), solder)
modn

drop1(modn, test = 'Chi')
