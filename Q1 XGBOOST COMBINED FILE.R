if("pacman" %in% rownames(installed.packages()) == FALSE) {install.packages("pacman")} # Check if you have universal installer package, install if not

pacman::p_load("caret","ROCR","lift","glmnet","MASS","e1071","xgboost")  #Check, and if needed install the necessary packages
library(readxl)
library(tidyverse) # Load Libraries
library(caret)
library(gbm)
library(dplyr)
library(lars)
library(moments)
library(caret)
library(knitr)
library(corrplot)
library(gbm)
library(glmnet)
library(mice)
library(ggplot2)
library(Amelia)
library(readr)
library(dplyr)
library(xgboost)
library(glmnet)

combined_files <- rbind(credit_data, new_credit_data) ### Combine files

###combined_files<- combined_files[-c(1473,6647,6969),] Only apply if not done so

training_final<- combined_files[1:23997,]  #### Split dataset
testing_final<- combined_files[23998:24997,]

combined_files_matrix <- model.matrix( default_0 ~ ., data = combined_files)###Createmodelmatrix

x_train <- combined_files_matrix[1:23997,]
x_test <- combined_files_matrix[23998:24997,] ###ERRROR:subscript out of bounds#### 

y_train <-training_final$default_0
y_test <- testing_final$default_0

model_XGboost<-xgboost(data = data.matrix(x_train), 
                       label = as.numeric(as.character(y_train)),
                       eta = 0.1,       # hyperparameter: learning rate
                       max_depth = 11,  # hyperparameter: size of a tree in each boosting iteration
                       nround=30,       # hyperparameter: number of boosting iterations
                       lambda=1,        # The next limes of code are to tune other hyperparameters
                       gamma=8,
                       min_child_weight=16,
                       subsample=0.80,
                       colsample_bytree=0.5,
                       objective = "binary:logistic")

XGboost_final_prediction<-predict(model_XGboost,newdata=x_test, type="response") #The following lines of code make the predictions and export the csv 
XGboost_classification<-rep("1",1000)
XGboost_classification[XGboost_final_prediction<0.221083]="0"
XGboost_classification<-as.factor(XGboost_classification)

predictions<- ifelse(XGboost_classification==0,1,0)

file1<-data.frame(default_0 = predictions)
write.csv(file1, file = " Q1XGBoost.csv", row.names=F)


