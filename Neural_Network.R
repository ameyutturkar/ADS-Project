# install.packages("e1071")
# install.packages("ggplot2")
# install.packages('caret')
# install.packages('rminer')
# install.packages('neuralnet')

library(ggplot2)
library(e1071)
library(caret)
library(rminer)
library(neuralnet)

train <- read.csv("C:/My_Data/Personal Data/Important Documents/MSIS/Course Work/Sem 3 - Fall 16/ADS/Assignments/Final_Project/loan(1).csv")
train
str(train)
View(train)

set.seed(1000)

dummy <- function(train, Columnname){
  for(level in unique(train[[Columnname]])){
    train[paste(Columnname,sep = "_",level)]<- ifelse(train[[Columnname]] == level,1,0)
  }
  return(subset(train,select = -get(Columnname)))
}

#Splitting the column into different columns(1 to N transformation)
train <- dummy(train, "Occupation")
train <- dummy(train, "Job_status")

train$Sex <- ifelse(train$Sex == "M" , 0, 1)
train$Res_status <- ifelse(train$Res_status == "owner" , 0, 1)
train$Telephone <- ifelse(train$Telephone == "given" , 0, 1)
train$Liab_ref <- ifelse(train$Liab_ref == "f" , 0, 1)
train$Acc_ref <- ifelse(train$Acc_ref == "given" , 0, 1)
train$Decision <- ifelse(train$Decision == "accept", 0, 1)

str(train)
View(train)
m <- model.matrix(~ Sex + Res_status + Telephone + Liab_ref + Acc_ref, data = train)
head(m)
#-----------------------------------------------------------------------------

#Partitioning the data
training_data <- train[1:344,]
testing_data <- train[345:429,]
#testing_data <- testing_data[, -14]
training_data
testing_data
colnames(train)

#Neural Network Full Model
#For classification set linear.output to FALSE otherwise set it to TRUE
nn <- neuralnet(Decision ~ Sex + Age + Time_at_add + Res_status + Telephone + Time_employed +
                  Time_bank + Liab_ref + Acc_ref + Home_Expn + Balance + Occupation_unemploye +
                  Occupation_labourer + Occupation_creative_ + Occupation_driver +
                  Occupation_professio + Occupation_manager + Occupation_guard_etc +
                  Occupation_executive + Occupation_office_st + Occupation_productio +
                  Occupation_semi_pro + Occupation_sales + Job_status_unemploye +
                  Job_status_governmen + Job_status_private_s + Job_status_self_empl +
                  Job_status_retired + Job_status_student + Job_status_military,
                  data = training_data, hidden = 8,  threshold=0.01,
                  linear.output = TRUE)
nn

plot(nn)

#Calculate weight that is connection strength between neurons
nn$weights #Weights are the numbers written near lines in the plot
nn$result.matrix #weights in a different way

nn$covariate
training_data$Decision

#Outcome or prediction of our model
nn$net.result[[1]]

#Check if the above results are more than 50%
#If yes, assign 1 else assign 0
verifyNN <- ifelse(nn$net.result[[1]] > 0.5, 1, 0)
verifyNN

#Calculate misclassification error
mce <- mean(training_data$Decision != verifyNN)
mce

#Compare Output with Prediction
OutputVsPrediction <- cbind(training_data$Decision, verifyNN)
OutputVsPrediction

#Compute The Model with Test Data
results <- compute(nn, testing_data[, -12])
results

print(head(results$net.result))
print(results$net.result)
results$net.result #Percentage of a rejection

#Evaluate and Round results against Test Labels
rounded_results <- sapply(results$net.result, round, digits = 0)
rounded_results

#Display results with respective parameter names
newResults <- data.frame(actual = testing_data$Decision, prediction = results$net.result,
                         rounded = round(results$net.result))
newResults

#Assigning Accept and Reject to values
testing_data$Decision <- ifelse(testing_data$Decision == 0, "accept", "reject")
results$net.result <- ifelse(round(results$net.result) == 0, "accept", "reject")

#Displaying results in the form of Accept and Reject
newResults <- data.frame(actual = testing_data$Decision, prediction = results$net.result)
newResults

#Creating confusion matrix
confusionMatrix(table(testing_data$Decision, results$net.result))
