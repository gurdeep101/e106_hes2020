#####################################
Bonferroni Joint Confidence Intervals
Bo and B1
######################################

fitreg<-lm(workhrs~lotsize,data=toluca_data)
confint(fitreg,level=1-0.1/2)


#####################################
Bonferroni Joint Confidence Intervals
Yhat confidence interval
######################################

Xh<-c(30,65,100)
predict.lm(fitreg,data.frame(lotsize = c(Xh)),interval = "confidence", level = 1-0.1/3)

#####################################
Bonferroni Joint Confidence Intervals
prediction Interval
######################################

predict.lm(fitreg,data.frame(lotsize = c(80,100)),interval = "prediction", level = 1-0.05/2)

######################
Working Hotelling
######################

Xh<-data.frame(lotsize=c(30,65,100))

W <- sqrt( 2 * qf(0.90,2,23))
CI<-predict(toluca.reg,Xh,se.fit=TRUE,interval="confidence",level=0.90)
cbind(CI$fit[,1]-W*CI$se.fit, CI$fit[,1] + W*CI$se.fit )