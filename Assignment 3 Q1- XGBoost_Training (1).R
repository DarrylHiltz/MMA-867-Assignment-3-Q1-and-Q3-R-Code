if("pacman" %in% rownames(installed.packages()) == FALSE) {install.packages("pacman")} # Check if you have universal installer package, install if not

pacman::p_load("caret","ROCR","lift","xgboost") #Check, and if needed install the necessary packages

###############################################################################################
inTrain <- createDataPartition(y = credit_data$default_0,
                               p = 22996/23997, list = FALSE)

training_T <- credit_data[ inTrain,]
testing_T <- credit_data[ -inTrain,]

credit_data_matrix_T<- model.matrix( default_0 ~ ., data = credit_data)[,-1]

x_train_T <- credit_data_matrix_T[ inTrain,]
x_test_T <- credit_data_matrix_T[ -inTrain,]

y_train_T <-training_T$default_0
y_test_T <-testing_T$default_0

model_XGboost_T<-xgboost(data = data.matrix(x_train_T), 
                       label = as.numeric(as.character(y_train_T)), 
                       eta = 0.1,       # hyperparameter: learning rate
                       max_depth = 11,  # hyperparameter: size of a tree in each boosting iteration
                       nround=30,       # hyperparameter: number of boosting iterations
                       lambda=1,        # The next limes of code are to tune other hyperparameters
                       gamma=8,
                       min_child_weight=16,
                       subsample=0.80,
                       colsample_bytree=0.5,
                       objective = "binary:logistic"
)

XGboost_prediction<-predict(model_XGboost_T,newdata=x_test_T, type="response") #Predict classification (for confusion matrix)
confusionMatrix(as.factor(ifelse(XGboost_prediction>0.221083,1,0)),y_test_T,positive="1") #Display confusion matrix

####ROC Curve
XGboost_ROC_prediction <- prediction(XGboost_prediction, y_test_T) #Calculate errors
XGboost_ROC_testing <- performance(XGboost_ROC_prediction,"tpr","fpr") #Create ROC curve data
plot(XGboost_ROC_testing) #Plot ROC curve

####AUC
auc.tmp <- performance(XGboost_ROC_prediction,"auc") #Create AUC data
XGboost_auc_testing <- as.numeric(auc.tmp@y.values) #Calculate AUC
XGboost_auc_testing #Display AUC value: 90+% - excellent, 80-90% - very good, 70-80% - good, 60-70% - so so, below 60% - not much value

#### Lift chart
plotLift(XGboost_prediction, y_test, cumulative = TRUE, n.buckets = 10) # Plot Lift chart

### An alternative way is to plot a Lift curve not by buckets, but on all data points
Lift_XGboost <- performance(XGboost_ROC_prediction,"lift","rpp")
plot(Lift_XGboost)
