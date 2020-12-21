rm(list = ls())

library(faraway) # cigs data for log reg
library(MASS) # negative binomial
library(ResourceSelection) # goodness of fit test
library(neuralnet) # neural networks
library(fastDummies) # dummy variables
library(boot) # Inv logit

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
pred <- predict(lmod, test, type = 'response', se.fit = TRUE)
pred

# prediction - under the hood
predict(lmod, test) # wo response we get transformed value
exp(predict(lmod,test)) # same as predict output

#prediction interval
# 95% CI ==> 1.96 
critval <- round(qnorm(1-0.05/2),2); critval

upr <- exp(pred$fit + (critval * pred$se.fit))
lwr <- exp(pred$fit - (critval * pred$se.fit))
cbind(upr, lwr)

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

test <- data.frame(Endemics = 23, Area = 25.09, Elevation = 346,
                   Nearest = 0.6, Scruz = 0.6, Adjacent = 1.84)

pred2 <- predict(modp, test, type = 'link', se.fit = TRUE); pred
# type = link returns actual value

#prediction interval
# 95% CI ==> 1.96 
critval <- round(qnorm(1-0.05/2),2); critval

upr <- exp(pred2$fit + (critval * pred2$se.fit))
lwr <- exp(pred2$fit - (critval * pred2$se.fit))
cbind(upr, lwr)

##################
# Neural Networks
##################

concrete <- read.csv('concrete.csv')
str(concrete)

# Neural networks work best when input data are scaled to a narrow range around zero
# here we have a wide range
# For bell shaped curve - use scale function in R
# For non-normal distribution normalize to 0 - 1 range

normalize <- function(x) {
  return((x-min(x))/(max(x) - min(x)))
}

conc_norm <- as.data.frame(lapply(concrete, normalize))
summary(conc_norm)
colnames(conc_norm)
str(conc_norm)

# sort into 72 - 25 set
ind <- sample(1:nrow(conc_norm), round(0.75*nrow(conc_norm)),0); length(ind)
tr <- conc_norm[ind,]
ts <- conc_norm[-ind,]

tr_og <- concrete[ind,]
ts_og <- concrete[-ind,]

# 1 hidden node
conc_modl <- neuralnet(strength~., data = tr)
plot(conc_modl)

# evaluate model performance
modl_results <- compute(conc_modl, ts[1:8])
# returns 2 values - neurons and net result

# 2nd iteration - 5 hidden nodes
conc_modl2 <- neuralnet(strength~., data = tr, hidden = 5)
plot(conc_modl2)

modl2_results <- compute(conc_modl2, ts)

# custom activation function - SmoothReLU aka SoftPlus
softplus <- function(x) { log(1+exp(x))}

# 2nd iteration - 2 layers of 5 hidden nodes each
conc_modl3 <- neuralnet(strength~., data = tr, hidden = c(5,5), act.fct = softplus)
plot(conc_modl3)

# convert predictions back to non-normalized form

unnormalize <- function(x) {
  return((x * (max(concrete$strength)) -min(concrete$strength)) 
         + min(concrete$strength))}

# create df of actual, predicted and un-normalized predicted
strength <- data.frame(ts_og$strength, ts$strength, modl2_results$net.result)
head(strength)
strength$pred <- unnormalize(strength$modl2_results.net.result)
head(strength)

cor(strength$pred,strength$ts_og.strength)

###########################
# Logistic practice exam P5
###########################

setwd("~/OneDrive/courses/e106/e106_hes2020/HW/practice_final")

p5 <- read.csv('Practice Final Question 5.csv')
str(p5)

# Y is dichotomous ==> logistic regression

for (i in (2:ncol(p5))) {
  print(paste('Column X',i))
  print(table(p5[,i]))
}

# X2 & X3 need dummy vars; X4 & X5 already have 0,1 coding
p5 <- dummy_cols(p5, select_columns = 'X2')
p5 <- dummy_cols(p5, select_columns = 'X3')

str(p5)

# drop original column and 1 level
p5 <- p5[,-c(2,3,6,9)]

# get coefficients for 2 way interaction excluding same term interaction

ff<-lm(Y~.^2,data=p5)
ff$coefficients

glm5a <- glm(Y~X1 + X4 + X2_2 + X2_3 + X3_2 + X1:X4 + X1:X2_2 + X1:X2_3 + X1:X3_2 + X4:X2_2 + X4:X2_3 + X4:X3_2 + X2_2:X3_2 + X2_3:X3_2, data = p5, family = binomial)
summary(glm5a)

# Check if interaction terms can be dropped

# reduced model
glm5b <- glm(Y~., family = binomial, data = p5)

# anova test for dropping of terms
anova(glm5b, glm5a, test = 'Chi') # reduced model, full model

# Ho : Interaction terms can be dropped
# Ha : Interaction terms cannot be dropped

# p-value = 0.9803 > 0.05 ==> fail to reject Ho; interaction can be dropped

# backward elimination to drop predictor variables

glm5c <- step(glm5a, direction = 'backward', trace = 0)
summary(glm5c)

# goodness of fit test - Hosmer Lemeshow test

hoslem.test(glm5c$y, fitted(glm5c), g = 5)

# Accept Ho - Fit is good; since p > 0.05

# dummy for test set
test_glm <- data.frame(X1 = c(33,6), X2 = c(1,1), X3 = c(1,1), X4 = c(0,0))
test_glm

# read original dataset into new df and combine test
p5temp <- read.csv('Practice Final Question 5.csv'); tail(p5temp)
p5temp <- p5temp[,-c(5)]; tail(p5temp)
p5temp <- rbind(p5temp, test_glm); tail(p5temp)

# X2 & X3 need dummy vars; X4 & X5 already have 0,1 coding
p5temp <- dummy_cols(p5temp, select_columns = 'X2')
p5temp <- dummy_cols(p5temp, select_columns = 'X3')
# check
tail(p5temp)

# drop columns not needed
p5temp <- p5temp[,-c(2,3,4,5,8)]; tail(p5temp)
glm5c$coefficients # check if columns same

# get test data into a new df
test_p5 <- p5temp[c(197,198),]; test_p5

# Inv Logit for predictions
pred_invlogit <- predict(glm5c, test_p5, type = 'link', se.fit = TRUE)
inv.logit(pred_invlogit$fit)

# Prediction as per original
pred_og <- predict(glm5c, test_p5, type = 'response', se.fit = TRUE); pred_og

# Confidence Intervals
confint(glm5c) # of coefficients

pred <- pred_invlogit
#prediction interval - Invlogit
# 95% CI ==> 1.96 
critval <- round(qnorm(1-0.05/2),2); critval

upr <- exp(pred$fit + (critval * pred$se.fit))
lwr <- exp(pred$fit - (critval * pred$se.fit))
cbind(upr, lwr)

pred <- pred_og
#prediction interval - OG
# 95% CI ==> 1.96 
critval <- round(qnorm(1-0.05/2),2); critval

upr <- exp(pred$fit + (critval * pred$se.fit))
lwr <- exp(pred$fit - (critval * pred$se.fit))
cbind(upr, lwr)

