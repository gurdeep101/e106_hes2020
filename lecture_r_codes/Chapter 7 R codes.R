#Body Fat Example
rm(list = ls())
setwd("~/OneDrive/courses/e106/e106_hes2020/lecture_r_codes")
CH07TA01 <- read.csv("CH07TA01.csv")
head(CH07TA01)
n<-dim(CH07TA01)[[1]]

f1 <- lm(Y~X1, data = CH07TA01)
f2 <- lm(Y~X2, data = CH07TA01)
f3 <- lm(Y~X3, data = CH07TA01)

summary(f1)
summary(f2)
summary(f3)

# get ssr
anova(f1)
anova(f2)
anova(f3)

max(anova(f1)[1,2],
    anova(f2)[1,2],
    anova(f3)[1,2]
)

# Extra sum of squares
f21 <- lm(Y~X1+X2, data = CH07TA01)
f23 <- lm(Y~X1+X3, data = CH07TA01)

summary(f21)
summary(f23)

#SSR(X2|X1) = SSR(X1,X2) - SSR(X1) - marginal contribution
anova(f21)
anova(f23)

# extract SSR values
sc <- anova(f23)
sc$`Sum Sq`

anova(f21)[1,2]

# get marginal contribution
ssr_x2_x1 <- anova(f21)[1,2] - anova(f1)[1,2]
ssr_x2_x1

frm1 <- lm(Y~X1+X2+X3,data=CH07TA01)
frm2 <- lm(Y~X1+X2,data=CH07TA01)
SSE1 <-deviance(frm1)
SSE2 <-deviance(frm2)
F<-((SSE2-SSE1)/1)/(SSE1/(n-4))
F

anova(frm1)
anova(frm2)
anova(frm2,frm1) # compare effect adding variable to the model; reduced, full


res1<-lm(Y~X2,data=CH07TA01)$residuals
res2<-lm(X1~X2,data=CH07TA01)$residuals
fitres<-summary(lm(res1~res2))
fitres$r.squared

#Dwaine Studios Example
#standartized regreesion
library(QuantPsyc)
CH07TA05 <- read.csv("/cloud/project/Fall 2020/CH07TA05.csv")
fit<-lm(Y~X1+X2,data=CH07TA05)
summary(fit)
lm.beta(fit)

#Crew Productivty Example
CrewProductivity<- read.csv("CrewProductivity.csv")
cor(CrewProductivity)

f0<-lm(Y~X1+X2,data=CrewProductivity)
f1<-lm(Y~X1,data=CrewProductivity)
f2<-lm(Y~X2,data=CrewProductivity)

summary(f0)
summary(f1)
summary(f2)

anova(f0)
anova(f1)
anova(f2)

######################
VIF
#######################
library(faraway)
vif(frm1)
round(cor(BodyFat),2)

#fix the faraway lib issue
#library('devtools')
#install_github("jyypma/nloptr")




