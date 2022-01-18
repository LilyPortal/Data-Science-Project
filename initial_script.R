#Require packages
require(dplyr)
require(ISLR)
require(boot)
require(caret)

#Read in csv file
data <- read.csv("College.csv")

#Investigate structure of data
str(data)

#remove first two columns of character data
data <- data[,c(-1:-2)]

#set seed to 1
set.seed(1)

#creating data partition for splitting 2/3 of data based on perc.alumni
set <-createDataPartition(data$perc.alumni, p= 2/3, list=F)
nrow(set)

#split test and training data
train <- data[set,]
test <- data[-set,]

#####################################################################################################
################    First MODEL   ###################################################################

#Create ControlParameters for 10 fold cross validation
ControlParameters <-trainControl(method="repeatedcv", 
                                 number=10,
                                 repeats=10)

#Create model on train data using perc.alumni as dependent variable, and Outstate and Grad.Rate
trainModel1 <- (perc.alumni~Outstate+Grad.Rate)

#Run Linear regression on model
trainlm1 <- lm(trainModel1,train)

summary(trainlm1)

#Perform 10 fold cross validation on train data, use ControlParameters
modellm1 <-train(trainModel1, data=train,
                 method="lm",
                 trControl= ControlParameters)

#Print the models
trainlm1
modellm1
#Print out RMSE and save to RMSE_train1
RMSE_train1 <- modellm1$results$RMSE
RMSE_train1

############## Predict on modellm2  ###########################################

(predict1 <- predict(modellm1,test))

#Calculate the error of the model
error1 <- predict1 - test$perc.alumni

#Calculate RMSE on the test data
RMSE_test1 <- sqrt(mean(error1^2))

#Compare RMSE train and test values
RMSE_train1
RMSE_test1


###################### Accuracy ########################

actuals_preds <- data.frame(cbind(actuals=test$perc.alumni, predicteds=predict1)) # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)

head(actuals_preds)

min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))
min_max_accuracy

#####################################################################################################
################    SECOND MODEL   ###################################################################

#Create model on train data using perc.alumni as dependent variable, and Accept, Enroll,  Outstate, Room.Board, Personal, S.F.Ratio and Grad.Rate
trainModel2 <- (perc.alumni~Accept+Enroll+Outstate+Room.Board+Personal+S.F.Ratio+Grad.Rate)

#Run Linear regression on model
trainlm2 <- lm(trainModel2,train)

summary(trainlm2)

#Perform 10 fold cross validation on train data, use ControlParameters
modellm2 <-train(trainModel2, data=train,
                 method="lm",
                 trControl= ControlParameters)

#Print the models
trainlm2
modellm2
#Print out RMSE and save to RMSE_train1
RMSE_train2 <- modellm2$results$RMSE
RMSE_train2

############## Predict on modellm2  ###########################################

(predict2 <- predict(modellm2,test))

#Calculate the error of the model
error2 <- predict2 - test$perc.alumni

#Calculate RMSE on the test data
RMSE_test2 <- sqrt(mean(error2^2))

#Compare RMSE train and test values
RMSE_train2
RMSE_test2

###################### Accuracy ########################

actuals_preds <- data.frame(cbind(actuals=test$perc.alumni, predicteds=predict2)) # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)

head(actuals_preds)

min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))
min_max_accuracy

#####################################################################################################
################    Third MODEL   ###################################################################

#Create model on train data using perc.alumni as dependent variable, and all others as independent
trainModel3 <- (perc.alumni~.)

#Run Linear regression on model
trainlm3 <- lm(trainModel3,train)

summary(trainlm3)

#Perform 10 fold cross validation on train data
modellm3 <-train(trainModel3, data=train,
                 method="lm",
                 trControl= ControlParameters)

#Print the models
trainlm3
modellm3
#Print out RMSE and save to RMSE_train1
RMSE_train3 <- modellm3$results$RMSE
RMSE_train3

############## Predict on modellm1  ###########################################

predict3 <- predict(modellm3,test)

#Calculate the error of the model
error3 <- predict3 - test$perc.alumni

#Calculate RMSE on the test data
RMSE_test3 <- sqrt(mean(error3^2))

#Compare RMSE train and test values
RMSE_train3
RMSE_test3

###################### Accuracy ########################

actuals_preds <- data.frame(cbind(actuals=test$perc.alumni, predicteds=predict3)) # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)

head(actuals_preds)

min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))
min_max_accuracy
