rm(list = ls())

set.seed(1023)
x_i <- c(4,8,12,16,20)
err_i <- rnorm(5,0,25) # 5 normal random numbers
y_i_est <- 20 + (4*x_i) + err_i # calculate yh
err_i; y_i_est

# fit regression and get summary
reg_q1a <- lm(y_i_est~x_i)
summary(reg_q1a)

# calculate E(Y_h) when X_h = 10
x_h <- 10
#y_h <- 20 + (4*xh)
e_y_h <- predict(reg_q1a, data.frame(x_i=x_h), interval = 'confidence', level = 0.95, se.fit = TRUE)
e_y_h

######## 1b

n_iter <- 200
b0_1b <- vector(mode = 'integer', length = n_iter)
b1_1b <- vector(mode = 'integer', length = n_iter)
yh_1b <- vector(mode = 'integer', length = n_iter)
yh_lwr_1b <- vector(mode = 'integer', length = n_iter)
yh_upr_1b <- vector(mode = 'integer', length = n_iter)

for (i in 1:n_iter) {
  #print(i)
  err_x <- rnorm(length(x_i), 0, 25)
  y_i_iter <- 20 + (4*x_i) + err_x
  reg_iter <- lm(y_i_iter~x_i)
  b0_1b[i] <- reg_iter$coefficients[1]
  b1_1b[i] <- reg_iter$coefficients[2]
  yh_1b_pred <- predict(reg_iter, data.frame(x_i = x_h), level = 0.95, interval = 'confidence')
  yh_1b[i] <- yh_1b_pred[1]
  yh_lwr_1b[i] <- yh_1b_pred[2]
  yh_upr_1b[i] <- yh_1b_pred[3]
}

######## 1c
b1_mean_1c <- mean(b1_1b)
b1_sd_1c <- sd(b1_1b)
b1_mean_1c; b1_sd_1c

par(mfrow = c(1,3))
hist(b0_1b)
hist(b1_1b)
hist(yh_1b)

#### 1d
yh_confint_1d <- data.frame(yh_lwr_1b, yh_upr_1b)
head(yh_confint_1d)

yh_incl_eh <- subset(yh_confint_1d, yh_lwr_1b < 60 & yh_upr_1b > 60)
(dim(yh_incl_eh)[1]/n_iter)*100 # percent of iterations that have E(Y_h)

##### 2
cdi <- read.csv("CDI Data.csv")
head(cdi)

reg_q2_tot_pop <- lm(Number.of.active.physicians~Total.population, data = cdi)
summary(reg_q2_tot_pop)

reg_q2_num_beds <- lm(Number.of.active.physicians~Number.of.hospital.beds, data = cdi)
summary(reg_q2_num_beds)

reg_q2_tot_income <- lm(Number.of.active.physicians~Total.personal.income, data = cdi)
summary(reg_q2_tot_income)

####### 3
table(cdi$Geographic.region) # shows 4 geographies with count of 103, 108, 152 and 77 

geo1 <- subset(cdi, Geographic.region == 1, select = c(Per.capita.income, Percent.bachelor.s.degrees))
# visually verified 103 obs with output of table function
geo1_p3_reg <- lm(Per.capita.income~Percent.bachelor.s.degrees, data = geo1)
confint(geo1_p3_reg, level = 0.9)
geo1_p3_reg$fitted.values[2]

geo2 <- subset(cdi, Geographic.region == 2, select = c(Per.capita.income, Percent.bachelor.s.degrees))
# visually verified 108 obs with output of table function
geo2_p3_reg <- lm(Per.capita.income~Percent.bachelor.s.degrees, data = geo2)
confint(geo2_p3_reg, level = 0.9)
geo2_p3_reg$fitted.values[2]

geo3 <- subset(cdi, Geographic.region == 3, select = c(Per.capita.income, Percent.bachelor.s.degrees))
# visually verified 152 obs with output of table function
geo3_p3_reg <- lm(Per.capita.income~Percent.bachelor.s.degrees, data = geo3)
confint(geo3_p3_reg, level = 0.9)
geo3_p3_reg$fitted.values[2]

geo4 <- subset(cdi, Geographic.region == 4, select = c(Per.capita.income, Percent.bachelor.s.degrees))
# visually verified 77 obs with output of table function
geo4_p3_reg <- lm(Per.capita.income~Percent.bachelor.s.degrees, data = geo4)
confint(geo4_p3_reg, level = 0.9)
geo4_p3_reg$fitted.values[2]

###### 4a
x_4a <- c(1,4,10,11,14)
sigma <- 0.6
beta0_4a <- 5
beta1_4a <- 3

exp_mse_4a <- sigma^2
exp_mse_4a

exp_msr_4a <- exp_mse_4a + (beta1_4a)*sum((x_4a - mean(x_4a))^2)
exp_msr_4a

################# 4b
x_4b <- c(6,7,8,9,10)
xh_4b <- 8

exp_msr_4b <- exp_mse_4a + (beta1_4a)*sum((x_4b -mean(x_4b))^2)
exp_msr_4b
