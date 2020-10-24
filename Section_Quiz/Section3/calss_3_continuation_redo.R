rm(list = ls())

df20 = read.delim('CH01PR20.txt', header = FALSE, sep = '')
index <- c(1:nrow(df20))
df20 <- data.frame(index,df20)
colnames(df20) <- c('i', 'y', 'x') # y - copiers serviced; x- num service calls
head(df20)

# setup basic anova table; which elements are additive?

lm20 <- lm(y~x, df20)
names(lm20)
anova(lm20)

# variation on account of regression
y_bar <- sum(df20$y/nrow(df20))
ybar <- mean(df20$y)
yd <- (lm20$fitted.values - ybar)^2
ssr <- sum(yd)
msr <- ssr
msr

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

anova(lm20)

# total variation is reduced by ssr; the part explained by "x"