rm(list = ls())

df20 = read.delim('CH01PR20.txt', header = FALSE, sep = '')
head(df20)
index <- c(1:nrow(df20))
df20 <- data.frame(index,df20)
colnames(df20) <- c('i', 'y', 'x') 
# x - num of copiers serviced; y - duration of service calls
head(df20)

# setup basic anova table; which elements are additive?

lm20 <- lm(y~x, df20)
summary(lm20)
names(lm20)
anova(lm20)

# 90% confidence interval
confint(lm20, level = 0.90)

# t-test if beta1 is significant ==> 2 sided test; i.e. = or != 0
qt(1-0.1/2, nrow(df20-2)) #---> returns critical t-value
# H-0 : not significant
# H_a : significant
# Reject H_0 if t* > t-value

# p-test
# t*-value from summary used to get p-value 
2*(1-pt(31.23, nrow(df20))) 
# returns 0 which is less than 0.05 ==> reject H_0

# similar way to do this is to look at absence of 0 in confint

# Mfg states that mean duration of service calls (y) should not 
# increase by more than14 minutes
# # conduct a test; type 1 error = 0.05; p-val and t-val

# H_o : beta1 <= 14; H_a : beta1 > 14 --> 1 sided t-test

# t_star = (b1 - beta1) / std error
t_star = (lm20$coefficients[2] - 14)/summary(lm20)$coefficients[2,2]
t_star # returns 2.142984 

# critical t-value
qt(1-0.05, nrow(df20)-2)
# returns 1.68 which is < t_star ==> reject H_0

# p-value
1-pt(t_star, nrow(df20)-2)
# returns 0.01890766 which is < 0.05
0.05 - (1-pt(t_star, nrow(df20)-2)) 
# returns = 0.03109234 hence proved

# variation on account of regression
y_bar <- sum(df20$y)/nrow(df20)
ybar <- mean(df20$y)
yd <- (lm20$fitted.values - ybar)^2
ssr <- sum(yd)
msr <- ssr
msr; ssr

# variation because of error
sse <- sum(lm20$residuals^2)
df_e <- lm20$df.residual
mse <- sse/df_e

xbar <- mean(df20$x)
ssx <- sum((df20$x - xbar)^2)
b1 <- lm20$coefficients[2]
exp_ms <- mse + ((b1^2)*ssx)

# Total model variation is a sum of SSR and SSE
ssto = ssr + sse
ssto

# Modified anova table includes correction of the mean
cor_f_mean <- nrow(df20)*ybar^2
cor_f_mean
anova(lm20)
# degrees of freedom and sum of squares are additive

# total variation is reduced by ssr/ssto; the part explained by "x"
# this is same as multiple R2 in the summary table

