credit <- read.csv("/cloud/project/credit.csv")
str(credit)
head(credit,10)
table(credit$checking_balance)
table(credit$savings_balance)
summary(credit$months_loan_duration)
summary(credit$amount)
table(credit$default)
#changing the default indicator to Yes and No
credit$default<- factor(credit$default, levels = c("1", "2"),labels = c("No", "Yes"))


#Data preparation – creating random training and test datasets
#We will use 90 percent of the data for training and 10 percent for testing, 
#which will provide us with 100 records to simulate new applicants.
nrow(credit)
set.seed(123)
train_sample <- sample(nrow(credit), 900)
str(train_sample)

credit_train <- credit[train_sample, ]
credit_test  <- credit[-train_sample, ]
#If randomization was done correctly, we should have about 30 percent of loans with default in each of the datasets
prop.table(table(credit_train$default))


#install.packages("C50")
library(C50)
credit_model <- C5.0(credit_train[-17], credit_train$default)
#The credit_model object now contains a C5.0 decision tree.
credit_model
summary(credit_model)

#The first three lines could be represented in plain language as:
#1.If the checking account balance is unknown or greater than 200 DM, then classify as "not likely to default."
#2.Otherwise, if the checking account balance is less than zero DM or between one and 200 DM…
#3… and the credit history is perfect or very good, then classify as "likely to default."

plot(credit_model)
#install.packages("partykit")
#library(partykit)

#evaluating model performance
#To apply our decision tree to the test dataset, we use the predict() function as shown in the following line of code:

credit_pred <- predict(credit_model, credit_test)

#This creates a vector of predicted class values, which we can compare to the actual class values using the CrossTable() function in the gmodels package. Setting the prop.c and prop.r parameters to FALSE removes the column and row percentages from the table. The remaining percentage (prop.t) indicates the proportion of records in the cell out of the total number of records:

library(gmodels)
CrossTable(credit_test$default, credit_pred,prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('actual default', 'predicted default'))

############################
#improving model performance
############################

#Boosting
#The C5.0() function makes it easy to add boosting to our decision tree. We simply need to add an additional trials parameter indicating 
#the number of separate decision trees to use in the boosted team. The trials parameter sets an upper limit; the algorithm will stop adding 
#trees if it recognizes that additional trials do not seem to be improving the accuracy. We'll start with 10 trials, a number that has become 
#the de facto standard, as research suggests that this reduces error rates on test data by about 25 percent. Aside from the new parameter, 
#the command is similar to before:

credit_boost10 <- C5.0(credit_train[-17], credit_train$default,trials = 10)
summary(credit_boost10)

credit_boost_pred10 <- predict(credit_boost10, credit_test)
CrossTable(credit_test$default, credit_boost_pred10,
             prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
             dnn = c('actual default', 'predicted default'))

###########################################
#Making some mistakes cost more than others
############################################
#Giving a loan to an applicant who is likely to default can be an expensive mistake. One solution to reduce the number of false 
#negatives may be to reject a larger number of borderline applicants under the assumption that the interest that the bank would earn 
#from a risky loan is far outweighed by the massive loss it would incur if the money is not paid back at all.

#To begin constructing the cost matrix, we need to start by specifying the dimensions. Since the predicted and actual values can both 
#take two values, yes or no, we need to describe a 2x2 matrix using a list of two vectors, each with two values. At the same time, 
#we'll also name the matrix dimensions to avoid confusion later on:

matrix_dimensions <- list(c("No", "Yes"), c("No", "Yes"))
names(matrix_dimensions) <- c("predicted", "actual")
matrix_dimensions
#Examining the new object shows that our dimensions have been set up correctly:

error_cost <- matrix(c(0, 1, 4, 0), nrow = 2, dimnames = matrix_dimensions)
error_cost


credit_cost <- C5.0(credit_train[-17], credit_train$default,costs = error_cost)
credit_cost_pred <- predict(credit_cost, credit_test)
CrossTable(credit_test$default, credit_cost_pred,
             prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
             dnn = c('actual default', 'predicted default'))

#Compared to our boosted model, this version makes more mistakes overall: 41 percent error here versus 18 percent in the boosted case.
#However, the types of mistakes are very different. Where the previous models classified only 42 and 61 percent of defaults correctly, 
#in this model, 26 / 33 = 79% of the actual defaults were correctly predicted to be defaults. This trade-off resulting in a reduction 
#of false negatives at the expense of increasing false positives may be acceptable if our cost estimates were accurate.



