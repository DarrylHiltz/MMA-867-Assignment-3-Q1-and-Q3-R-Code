credit_data.orig <- read_excel(choose.files()) # Import credit data file

credit_data <- credit_data.orig

credit_data$default_0 <- as.numeric(credit_data$default_0)


ages <- c(paste(seq(20, 95, by = 5), seq(20 + 5 - 1, 100 - 1, by = 5),
                sep = "-"), paste(100, "+", sep = ""))
credit_data$AGE <- cut(credit_data$AGE, breaks = c(seq(20, 100, by = 5), Inf), labels = ages, right = FALSE)

credit_data[sapply(credit_data, is.numeric)] <- lapply(credit_data[sapply(credit_data, is.numeric)], as.factor)
names <- c('ID' ,'LIMIT_BAL','PAY_AMT1','PAY_AMT2','PAY_AMT3','PAY_AMT4','PAY_AMT5','PAY_AMT6','BILL_AMT1','BILL_AMT2','BILL_AMT3','BILL_AMT4','BILL_AMT5','BILL_AMT6','Bill_1_Overall','Bill_amount_1','bill_amount_2','bill_amount_3','bill_amount_4','bill_amount_5','bill_amount_6','Debt_ratio')
credit_data[,names] <- lapply(credit_data[,names] , as.numeric)
credit_data$Bal_group <- as.factor(credit_data$Bal_group)
credit_data$PAY_1  <- as.factor(credit_data$PAY_1)
credit_data$PAY_2  <- as.factor(credit_data$PAY_2)
credit_data$PAY_3  <- as.factor(credit_data$PAY_3)
credit_data$PAY_4  <- as.factor(credit_data$PAY_4)
credit_data$PAY_5  <- as.factor(credit_data$PAY_5)
credit_data$PAY_6  <- as.factor(credit_data$PAY_6)


fixNAs<-function(data_frame){
  # Define reactions to NAs
  integer_reac<-0
  factor_reac<-"FIXED_NA"
  character_reac<-"FIXED_NA"
  date_reac<-as.Date("1900-01-01")
  # Loop through columns in the data frame and depending on which class the variable is, apply the defined reaction and create a surrogate
  
  for (i in 1 : ncol(data_frame)){
    if (class(data_frame[,i]) %in% c("numeric","integer")) {
      if (any(is.na(data_frame[,i]))){
        data_frame[,paste0(colnames(data_frame)[i],"_surrogate")]<-
          as.factor(ifelse(is.na(data_frame[,i]),"1","0"))
        data_frame[is.na(data_frame[,i]),i]<-integer_reac
      }
    } else
      if (class(data_frame[,i]) %in% c("factor")) {
        if (any(is.na(data_frame[,i]))){
          data_frame[,i]<-as.character(data_frame[,i])
          data_frame[,paste0(colnames(data_frame)[i],"_surrogate")]<-
            as.factor(ifelse(is.na(data_frame[,i]),"1","0"))
          data_frame[is.na(data_frame[,i]),i]<-factor_reac
          data_frame[,i]<-as.factor(data_frame[,i])
          
        } 
      } else {
        if (class(data_frame[,i]) %in% c("character")) {
          if (any(is.na(data_frame[,i]))){
            data_frame[,paste0(colnames(data_frame)[i],"_surrogate")]<-
              as.factor(ifelse(is.na(data_frame[,i]),"1","0"))
            data_frame[is.na(data_frame[,i]),i]<-character_reac
          }  
        } else {
          if (class(data_frame[,i]) %in% c("Date")) {
            if (any(is.na(data_frame[,i]))){
              data_frame[,paste0(colnames(data_frame)[i],"_surrogate")]<-
                as.factor(ifelse(is.na(data_frame[,i]),"1","0"))
              data_frame[is.na(data_frame[,i]),i]<-date_reac
            }
          }  
        }       
      }
  } 
  return(data_frame) 
}



credit_data<-fixNAs(credit_data)




#################################################################
# XGBoost Model
#################################################################



credit_data<- credit_data[-c(1473,6647,6969),]

set.seed(77850)

inTrain <- createDataPartition(y = credit_data$default_0,
                               p = 22996/23997, list = FALSE)


train.T <- credit_data[inTrain,]
test.T <- credit_data[-inTrain,]

credit_data_matrix <- model.matrix( default_0 ~ ., data = credit_data)[,-1]

train.T.matrix <- credit_data_matrix[ inTrain,]
test.T.matrix <- credit_data_matrix[ -inTrain,]

y_train_T <-train.T$default_0
y_test_T <-test.T$default_0

model_XGboost_T<-xgboost(data = data.matrix(train.T.matrix), 
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

XGboost_prediction<-predict(model_XGboost_T,newdata=test.T.matrix, type="response") #Predict classification (for confusion matrix)
confusionMatrix(as.factor(ifelse(XGboost_prediction>0.221083,1,0)),y_test_T,positive="1") 

####ROC Curve
XGboost_ROC_prediction <- prediction(XGboost_prediction, y_test_T) #Calculate errors
XGboost_ROC_testing <- performance(XGboost_ROC_prediction,"tpr","fpr") #Create ROC curve data
plot(XGboost_ROC_testing) 

####AUC
auc.tmp <- performance(XGboost_ROC_prediction,"auc") #Create AUC data
XGboost_auc_testing <- as.numeric(auc.tmp@y.values) #Calculate AUC
XGboost_auc_testing



#### Lift chart
plotLift(XGboost_prediction, y_test_T, cumulative = TRUE, n.buckets = 10) # Plot Lift chart



## Hold out predicted profit for holdout





#################################################################
## Predict XGBoost Model
#################################################################

new_credit_data.orig  <- read_excel(file.choose(), sheet=1) # Import new applications file

new_credit_data <- new_credit_data.orig

new_credit_data$default_0<- as.numeric(0)

combined_files.orig <- rbind(credit_data.orig, new_credit_data) ### Combine files

combined_files <- combined_files.orig

combined_files$AGE <- cut(combined_files$AGE, breaks = c(seq(20, 100, by = 5), Inf), labels = ages, right = FALSE)

combined_files[sapply(combined_files, is.numeric)] <- lapply(combined_files[sapply(combined_files, is.numeric)], as.factor)
names <- c('ID' ,'LIMIT_BAL','PAY_AMT1','PAY_AMT2','PAY_AMT3','PAY_AMT4','PAY_AMT5','PAY_AMT6','BILL_AMT1','BILL_AMT2','BILL_AMT3','BILL_AMT4','BILL_AMT5','BILL_AMT6','Bill_1_Overall','Bill_amount_1','bill_amount_2','bill_amount_3','bill_amount_4','bill_amount_5','bill_amount_6','Debt_ratio')
combined_files[,names] <- lapply(combined_files[,names] , as.numeric)
combined_files$Bal_group <- as.factor(combined_files$Bal_group)
combined_files$PAY_1  <- as.factor(combined_files$PAY_1)
combined_files$PAY_2  <- as.factor(combined_files$PAY_2)
combined_files$PAY_3  <- as.factor(combined_files$PAY_3)
combined_files$PAY_4  <- as.factor(combined_files$PAY_4)
combined_files$PAY_5  <- as.factor(combined_files$PAY_5)
combined_files$PAY_6  <- as.factor(combined_files$PAY_6)

combined_files_matrix <- model.matrix( default_0 ~ ., data = combined_files)[,-1]

new.application.matrix <- combined_files_matrix[24001:25000,]

new.application.prediction<-predict(model_XGboost_T,newdata=new.application.matrix, type="response")

new.application.prediction[new.application.prediction<0.221083]="0"
new.application.prediction[new.application.prediction>=0.221083]="1"


write.csv(new.application.prediction, file = "XGBoost with New Data.csv", row.names=F)







###########################################################################################
####  Question 3
###########################################################################################

## without gender

train.T.WO.Gender <- train.T[-3]
test.T.WO.Gender <- test.T[-3]

train.T.WO.Gender.Matrix <- model.matrix( default_0 ~ ., data = train.T.WO.Gender)[,-1]
test.T.WO.Gender.Matrix <- model.matrix( default_0 ~ ., data = test.T.WO.Gender)[,-1]

model_XGboost_T.WO.Gender <-xgboost(data = data.matrix(train.T.WO.Gender.Matrix), 
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


XGboost_prediction.WO.Gender <- predict(model_XGboost_T.WO.Gender,newdata=test.T.WO.Gender.Matrix, type="response") #Predict classification (for confusion matrix)
confusionMatrix(as.factor(ifelse(XGboost_prediction.WO.Gender>0.221083,1,0)),y_test_T,positive="1")

####ROC Curve
XGboost_ROC_prediction.WO.Gender <- prediction(XGboost_prediction.WO.Gender, y_test_T) #Calculate errors
XGboost_ROC_testing.WO.Gender <- performance(XGboost_ROC_prediction.WO.Gender,"tpr","fpr") #Create ROC curve data
plot(XGboost_ROC_testing.WO.Gender) #Plot ROC curve

####AUC
auc.tmp.WO.Gender <- performance(XGboost_ROC_prediction.WO.Gender,"auc") #Create AUC data
XGboost_auc_testing.WO.Gender <- as.numeric(auc.tmp.WO.Gender@y.values) #Calculate AUC
XGboost_auc_testing.WO.Gender



#### Lift chart
plotLift(XGboost_prediction, y_test_T, cumulative = TRUE, n.buckets = 10) # Plot Lift chart





#########################################################################################
## Male Only
#########################################################################################


test.T.Male <- filter(test.T, SEX == '1')  ## 1 = Male; 2 = Female
test.T.WO.Gender.Male <- filter(test.T, SEX == '1')[-3]


y.test.Male <- test.T.Male$default_0

test.T.Male.matrix <- model.matrix( default_0 ~ ., data = test.T.Male)[,-1]
test.T.WO.Gender.Male.matrix <- model.matrix( default_0 ~ ., data = test.T.WO.Gender.Male)[,-1]



## using model with gender ##########################################################################
XGboost_prediction.Male <- predict(model_XGboost_T,newdata=test.T.Male.matrix, type="response") #Predict classification (for confusion matrix)
confusionMatrix(as.factor(ifelse(XGboost_prediction.Male>0.221083,1,0)),y.test.Male,positive="1") 

####ROC Curve
XGboost_ROC_prediction.Male <- prediction(XGboost_prediction.Male, y.test.Male) #Calculate errors
XGboost_ROC_testing.Male <- performance(XGboost_ROC_prediction.Male,"tpr","fpr") #Create ROC curve data
plot(XGboost_ROC_testing.Male) #Plot ROC curve

####AUC
auc.tmp.Male <- performance(XGboost_ROC_prediction.Male,"auc") #Create AUC data
XGboost_auc_testing.Male <- as.numeric(auc.tmp.Male@y.values) #Calculate AUC
XGboost_auc_testing.Male

#### Lift chart
plotLift(XGboost_prediction.Male, y.test.Male, cumulative = TRUE, n.buckets = 10) # Plot Lift chart


XGboost_classification.Male<-rep("1",1000)
XGboost_classification.Male[XGboost_prediction.Male<0.221083]="0"
XGboost_classification.Male<-as.factor(XGboost_classification.Male)
predictions.W.Gender.Male<- ifelse(XGboost_classification.Male==0,1,0)
predictin.W.Gender.Male <- data.frame(default_0 = predictions.W.Gender.Male)
write.csv(predictin.W.Gender.Male, file = "predictin.W.Gender.Male.csv", row.names=F)


## using model without gender #####################################################################
XGboost_prediction.WO.Gender.Male <- predict(model_XGboost_T.WO.Gender,newdata=test.T.WO.Gender.Male.matrix, type="response") #Predict classification (for confusion matrix)
confusionMatrix(as.factor(ifelse(XGboost_prediction.WO.Gender.Male>0.221083,1,0)),y.test.Male,positive="1")

####ROC Curve
XGboost_ROC_prediction.WO.Gender.Male <- prediction(XGboost_prediction.WO.Gender.Male, y.test.Male) #Calculate errors
XGboost_ROC_testing.WO.Gender.Male <- performance(XGboost_ROC_prediction.WO.Gender.Male,"tpr","fpr") #Create ROC curve data
plot(XGboost_ROC_testing.WO.Gender.Male) #Plot ROC curve

####AUC
auc.tmp.WO.Gender.Male <- performance(XGboost_ROC_prediction.WO.Gender.Male,"auc") #Create AUC data
XGboost_auc_testing.WO.Gender.Male <- as.numeric(auc.tmp.WO.Gender.Male@y.values) #Calculate AUC
XGboost_auc_testing.WO.Gender.Male

#### Lift chart
plotLift(XGboost_ROC_prediction.WO.Gender.Male, y.test.Male, cumulative = TRUE, n.buckets = 10) # Plot Lift chart


XGboost_classification.WO.Gender.Male<-rep("1",1000)
XGboost_classification.WO.Gender.Male[XGboost_prediction.WO.Gender.Male<0.221083]="0"
XGboost_classification.WO.Gender.Male<-as.factor(XGboost_classification.WO.Gender.Male)
predictions.WO.Gender.Male<- ifelse(XGboost_classification.WO.Gender.Male==0,1,0)
predictin.WO.Gender.Male <- data.frame(default_0 = predictions.WO.Gender.Male)
write.csv(predictin.WO.Gender.Male, file = "predictin.WO.Gender.Male.csv", row.names=F)





#########################################################################################
## Female Only
#########################################################################################


test.T.Female <- filter(test.T, SEX == '2')  ## 1 = Male; 2 = Female
test.T.WO.Gender.Female <- filter(test.T, SEX == '2')[-3]


y.test.Female <- test.T.Female$default_0

test.T.Female.matrix <- model.matrix( default_0 ~ ., data = test.T.Female)[,-1]
test.T.WO.Gender.Female.matrix <- model.matrix( default_0 ~ ., data = test.T.WO.Gender.Female)[,-1]



## using model with gender ##########################################################################
XGboost_prediction.Female <- predict(model_XGboost_T,newdata=test.T.Female.matrix, type="response") #Predict classification (for confusion matrix)
confusionMatrix(as.factor(ifelse(XGboost_prediction.Female>0.221083,1,0)),y.test.Female,positive="1")

####ROC Curve
XGboost_ROC_prediction.Female <- prediction(XGboost_prediction.Female, y.test.Female) #Calculate errors
XGboost_ROC_testing.Female <- performance(XGboost_ROC_prediction.Female,"tpr","fpr") #Create ROC curve data
plot(XGboost_ROC_testing.Female) #Plot ROC curve

####AUC
auc.tmp.Female <- performance(XGboost_ROC_prediction.Female,"auc") #Create AUC data
XGboost_auc_testing.Female <- as.numeric(auc.tmp.Female@y.values) #Calculate AUC
XGboost_auc_testing.Female

#### Lift chart
plotLift(XGboost_prediction.Female, y.test.Female, cumulative = TRUE, n.buckets = 10) # Plot Lift chart


XGboost_classification.Female<-rep("1",1000)
XGboost_classification.Female[XGboost_prediction.Female<0.221083]="0"
XGboost_classification.Female<-as.factor(XGboost_classification.Female)
predictions.W.Gender.Female<- ifelse(XGboost_classification.Female==0,1,0)
predictin.W.Gender.Female <- data.frame(default_0 = predictions.W.Gender.Female)
write.csv(predictin.W.Gender.Female, file = "predictin.W.Gender.Female.csv", row.names=F)


## using model without gender #####################################################################
XGboost_prediction.WO.Gender.Female <- predict(model_XGboost_T.WO.Gender,newdata=test.T.WO.Gender.Female.matrix, type="response") #Predict classification (for confusion matrix)
confusionMatrix(as.factor(ifelse(XGboost_prediction.WO.Gender.Female>0.221083,1,0)),y.test.Female,positive="1")

####ROC Curve
XGboost_ROC_prediction.WO.Gender.Female <- prediction(XGboost_prediction.WO.Gender.Female, y.test.Female) #Calculate errors
XGboost_ROC_testing.WO.Gender.Female <- performance(XGboost_ROC_prediction.WO.Gender.Female,"tpr","fpr") #Create ROC curve data
plot(XGboost_ROC_testing.WO.Gender.Female) #Plot ROC curve

####AUC
auc.tmp.WO.Gender.Female <- performance(XGboost_ROC_prediction.WO.Gender.Female,"auc") #Create AUC data
XGboost_auc_testing.WO.Gender.Female <- as.numeric(auc.tmp.WO.Gender.Female@y.values) #Calculate AUC
XGboost_auc_testing.WO.Gender.Female 

#### Lift chart
plotLift(XGboost_ROC_prediction.WO.Gender.Female, y.test.Female, cumulative = TRUE, n.buckets = 10) # Plot Lift chart


XGboost_classification.WO.Gender.Female<-rep("1",1000)
XGboost_classification.WO.Gender.Female[XGboost_prediction.WO.Gender.Female<0.221083]="0"
XGboost_classification.WO.Gender.Female<-as.factor(XGboost_classification.WO.Gender.Female)
predictions.WO.Gender.Female<- ifelse(XGboost_classification.WO.Gender.Female==0,1,0)
prediction.WO.Gender.Female <- data.frame(default_0 = predictions.WO.Gender.Female)
write.csv(prediction.WO.Gender.Female, file = "predictin.WO.Gender.Female.csv", row.names=F)

















