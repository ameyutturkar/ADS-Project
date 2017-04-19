# install.packages("e1071")
# install.packages("ggplot2")
# install.packages('caret')
# install.packages('rminer')
# install.packages('neuralnet')

library(e1071)
library(rminer)
library(neuralnet)
library(ggplot2)
library(caret)

train <- read.csv("C:/My_Data/Personal Data/Important Documents/MSIS/Course Work/Sem 3 - Fall 16/ADS/Assignments/Final_Project/loan(1).csv")
train

set.seed(1000)

str(train)
View(train)

#Check the total number of accepts and rejects
table(train$Decision)

#Convert to Categorical variables
train$Age <- ifelse(train$Age > 50 , "Aged", "Young")
train$Time_at_add <- ifelse(train$Time_at_add < 8 , "Short", ifelse(train$Time_at_add < 16, "Medium", "Long"))
train$Time_employed <- ifelse(train$Time_employed < 5 , "Short", ifelse(train$Time_employed < 10, "Medium", "Long"))
train$Time_bank <- ifelse(train$Time_bank < 5 , "New", ifelse(train$Time_bank < 10, "Intermediate", "Old"))
train$Home_Expn <- ifelse(train$Home_Expn < 200 , "Low", ifelse(train$Home_Expn < 500, "Medium", "High"))
train$Balance <- ifelse(train$Balance < 1000  , "Low", ifelse(train$Balance < 5000, "Medium", "High"))

#Get all column names
feature.names <- names(train)[2:ncol(train)-1]
str(train)

#Partitioning the data
training_data <- train[1:344,]
testing_data <- train[345:429,]
training_data
testing_data

# ---------------------------------NBC Part---------------------------------------

#NBC Model
nbc_Model <- naiveBayes(Decision ~ Sex + Age + Time_at_add + Res_status + Telephone +
                          Occupation + Job_status + Time_employed + Time_bank + Liab_ref +
                          Acc_ref + Home_Expn + Balance, data = training_data)


nbc_Model
summary(nbc_Model)
str(nbc_Model)

#Predict the output
predict_nbc <- predict(nbc_Model, testing_data)
predict_nbc

#Predict the output by displaying posteriors
predict_nbc_discrete <- predict(nbc_Model, testing_data, type = c("raw"))
predict_nbc_discrete

#Predict the output with the substitution error
pred_nbc_sub <- predict(nbc_Model, testing_data[, -1])
pred_nbc_sub

#Create Confusion matrix and overall accuracy
tab <- confusionMatrix(table(pred_nbc_sub, training_data$Decision))
tab
mmetric(testing_data$Decision, predict_nbc, c("ACC", "PRECISION", "TPR", "F1"))

#NBC With Laplace Smoothing
nbc_Laplace <- naiveBayes(Decision ~ Sex + Age + Time_at_add + Res_status + Telephone +
                            Occupation + Job_status + Time_employed + Time_bank + Liab_ref +
                            Acc_ref + Home_Expn + Balance, data = training_data, 
                            laplace = 3)
nbc_Laplace

#Predict the Laplace output
pred_Laplace <- predict(nbc_Laplace, testing_data)
pred_Laplace

#Predict the Laplace output by displaying posteriors
predict_laplace_discrete <- predict(nbc_Laplace, testing_data, type = c("raw"))
predict_laplace_discrete

#Predict the Laplace output with the substitution error
pred_Laplace_sub <- predict(nbc_Laplace, training_data[, -1])
pred_Laplace_sub

#Create Confusion matrix and overall accuracy
tab1 <- confusionMatrix(table(pred_Laplace_sub, training_data$Decision))
tab1
mmetric(testing_data$Decision, pred_Laplace, c("ACC", "PRECISION", "TPR", "F1"))
