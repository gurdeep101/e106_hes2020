library(knitr)

rm(list = ls())

CDI <- read.csv("~/OneDrive/courses/e106/HW/HW1/CDI Data.csv")
head(CDI)
summary(CDI)

reg=lm(Total.serious.crimes~Total.personal.income,data=CDI)
reg$coefficients

ei2=sum((reg$residuals)^2)
sigma2=ei2/(length(reg$residuals)-2)
sigma2

plot(CDI$Total.personal.income,CDI$Total.serious.crimes,xlab="Personal Income",ylab="Total Crime")
abline(reg)

length(reg$residuals)

# toluca_data <- read.csv("/cloud/project/toluca_data.csv")
toluca_data <- read.csv('~/OneDrive/courses/e106/Section_Quiz/Section1/toluca_data.csv')
head(toluca_data)
attach(toluca_data)
# fitting the regression model 
toluca.reg <- lm(workhrs ~ lotsize)
# getting the summary regression output:
toluca.reg <- lm(workhrs ~ lotsize)
summary(toluca.reg)
# getting the confidence interval
confint(toluca.reg, level = 0.95) # significant since zero is not in the interval
# predict values
predict(toluca.reg,data.frame(lotsize = 10), se.fit = TRUE, interval = 'confidence')
predict(toluca.reg,data.frame(lotsize = c(10,20)), se.fit = TRUE, interval = 'confidence')
predict(toluca.reg,data.frame(lotsize = 65), se.fit = TRUE, interval = 'confidence', level = 0.95)
predict(toluca.reg, data.frame(lotsize = 100), se.fit = TRUE, interval = 'confidence', level = 0.90)

# getting the prediction interval
predict(toluca.reg, data.frame(lotsize = 100), se.fit = TRUE, interval = 'prediction', level = 0.90)
# getting the ANOVA table:
anova(toluca.reg)
# getting the fitted values:
fitted(toluca.reg)
# getting the residuals:
names(toluca.reg)
resid(toluca.reg)
toluca.reg$residuals
plot(lotsize, workhrs)
# overlaying the regression line on this scatter plot:
abline(toluca.reg)


d) Determine the boundary values of the 99 percent confidence band for the regression line when Xh = 2 and when Xh = 4. Is your confidence band wider at these two points than the corresponding confidence intervals in part (a)? Should it be? 
```{r prow2blem 2.15d}
predict(lmFit21,data.frame(x=3),interval="prediction",level=0.99,se.fit=TRUE)
W2= 2*qf(1-0.01,2,8)
W=sqrt(W2)
W
