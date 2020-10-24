x <- c(5,12,10)
y <- c(20, 55, 30)
f <- lm(y~x)
summary(f)
plot(x, y); abline(f)

m1.y <- rep(mean(y),3) # repeat mean fo y 3 times and concatenate into a vector
m2.y <- rep(mean(y[c(1,3)]),3) # index specific values of y, calculate mean and repeat
m3.y <- f$fitted.values

output = cbind(m1.y, m2.y, m3.y) # group by columns
output
typeof(output)
apply(output,2,sum) # total by column of output
apply(output,2, mean) # average by column of output
# other functions inclue abs, output^2,

# Section 2 - Copier Maintenance
setwd("~/OneDrive/courses/e106/e106_hes2020/Section_Quiz/Section2")

df20 <- read.delim('CH01PR20.txt', header = FALSE, sep = '')
colnames(df20) <- c('y', 'x')

lmfit20 <- lm(y~x, data = df20)
summary(lmfit20)

# negative intercept does not have any relevant information here
# verify mean goes through regression - 
  # calculate xbar, ybar
  # predict ybar using xbar - answer should be same

# relation between residuals and sum of squared residuals
resid1 <- df20$y - lmfit20$fitted.values
resid2 <- lmfit20$residuals
sum(resid1 - resid2) # nearly 0
sum(resid1)-sum(resid2)

# calculate SSE
sum(resid1^2)
sum(resid2^2)
sum(resid1^2)-sum(resid2^2) # diff small; values are equal

# calculate MSE - 2 ways
sum(lmfit20$residuals^2)/lmfit20$df.residual # sum(residuals squared) / degrees of freedom
summary(lmfit20)$sigma^2 # sigma^2 from summary = MSE

anova(lmfit20)


