#####################
QQ PLOT
#####################
toluca.stdres = rstandard(toluca.reg)
qqnorm(toluca.stdres,ylab="Standardized Residuals",xlab="Normal Scores",main="Toluca") 
qqline(toluca.stdres)

######################
Brown-Forsythe Test
######################
Ho: Error variances are constant
Ha: Error variances are not constant

> f3.4<-lm(Dataset_1.20$V1 ~ Dataset_1.20$V2)
> summary(f3.4)

Call:
lm(formula = Dataset_1.20$V1 ~ Dataset_1.20$V2)

Residuals:
     Min       1Q   Median       3Q      Max 
-22.7723  -3.7371   0.3334   6.3334  15.4039 

Coefficients:
                Estimate Std. Error t value Pr(>|t|)    
(Intercept)      -0.5802     2.8039  -0.207    0.837    
Dataset_1.20$V2  15.0352     0.4831  31.123   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 8.914 on 43 degrees of freedom
Multiple R-squared:  0.9575,	Adjusted R-squared:  0.9565 
F-statistic: 968.7 on 1 and 43 DF,  p-value: < 2.2e-16

> summary(Dataset_1.20$V2)
   Min. 1st Qu.  Median    Mean 3by=0.1rd Qu.    Max. 
  1.000   2.000   5.000   5.111   7.000  10.000 
>

ei<-f3.4$residuals

We will separate the data based on the median value, we first need to create a data frame with X,Y and errors.

pop1.20<-data.frame(cbind(Dataset_1.20$V1, Dataset_1.20$V2,ei))
p1<-pop1.20[pop1.20[,2]< 5,]
p2<-pop1.20[pop1.20[,2]>=5,]

M1<-summary(p1[,3])[3]
M2<-summary(p2[,3])[3]
N1<-dim(p1)[1]
N2<-dim(p2)[1]


d1<-abs(p1[,3]-M1)
d2<-abs(p2[,3]-M2)

s2<-sqrt((var(d1)*(N1-1)+var(d2)*(N2-1))/(N1+N2-2))

Den<- s2*sqrt(1/N1+1/N2)
Num<- mean(d1)-mean(d2)

T= Num/Den
> T
[1] -0.6887365
> 1-pt(T,N1+N2-2)
[1] 0.7526557

Comments on T: accept null, error variances are constant.
######################
Breusch-Pagan Test
######################

> ei2<-ei^2
> g<-lm(ei2~Dataset_1.20$V2)
>> anova(g)
Analysis of Variance Table

Response: ei2
                Df Sum Sq Mean Sq F value Pr(>F)
Dataset_1.20$V2  1  15155   15155  1.3998 0.2433
Residuals       43 465556   10827               
> anova(f3.4)
Analysis of Variance Table

Response: Dataset_1.20$V1
                Df Sum Sq Mean Sq F value    Pr(>F)    
Dataset_1.20$V2  1  76960   76960  968.66 < 2.2e-16 ***
Residuals       43   3416      79                      
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
> 

SSR*=  15155
SSE =  3416

Chi-Square=(15155 / 2) / ((3416/45)^2)
[1] 1.314968
> 1-pchisq(1.314968,1)
[1] 0.2514971
>
Accept the null

#################
Box Cox Plot
#################
library(MASS)
par(mfrow=c(1,1))
f<-lm(Y~X,data=PlasmaLevel)
boxcox(f,lambda=seq(-5,5,by=.1))
boxcox(f,lambda=seq(-1,1,by=.1))
boxcox(f,lambda=seq(-2,1,by=.05))
f1<-lm(LogY~X,data=PlasmaLevel)
f2<-lm(1/sqrt(Y)~X,data=PlasmaLevel)
#################
Lowess Regression
################

par(mfrow=c(1,1))
f3.4<-lm(Dataset_1.20$V1 ~ Dataset_1.20$V2)
plot(Dataset_1.20$V1 ~ Dataset_1.20$V2, main = "lowess")
abline(f3.4,col=1)
lines(lowess(Dataset_1.20$V1 ~ Dataset_1.20$V2,f=0.01), col = 2)
lines(lowess(Dataset_1.20$V1 ~ Dataset_1.20$V2,f=0.1), col = 3)
lines(lowess(Dataset_1.20$V1 ~ Dataset_1.20$V2,f=1/3), col = 4)
lines(lowess(Dataset_1.20$V1 ~ Dataset_1.20$V2,f=2/3), col = 5)
lines(lowess(Dataset_1.20$V1 ~ Dataset_1.20$V2, f=.2), col = 6)
legend(5, 120, c(paste("f = ", c("0.01","0.1","1/3","2/3", ".2"))), lty = 1, col = 2:6)
#################
Correlation Test
################
cor.test(Crime.Rate$X,Crime.Rate$Y, method ="pearson")
cor.test(Crime.Rate$X,Crime.Rate$Y, method ="spearman")

