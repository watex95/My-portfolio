
setwd("D:/KAZI/UPWORK/KAGGLE COMPETITION/FEATURES/clean_features")

transformed_train=read.csv("transformed_train.csv",header = T)
transformed_test=read.csv("transformed_test.csv",header = T)

transformed_train$Dropout=as.factor(transformed_train$Dropout)

# Load required packages
library(class)
library(caret)
library(rpart)
library(dplyr)
library(gbm)

# Split the transformed_train dataset into training and validation datasets
set.seed(123)
## 80% of the sample size
smp_size <- floor(0.80 * nrow(transformed_train))
train_ind <- sample(seq_len(nrow(transformed_train)), size = smp_size)
train.set <- transformed_train[train_ind, ]
validation.set <- transformed_train[-train_ind, ]




# //////////////////////////////////////////////////////////////////////////////////////////

# Stochastic Gradient Boosting GBM model
library(caret)

# for reproducibility
set.seed(123)
fit.c50<-train(Dropout~., data=train.set, method = "C5.0", metric="Accuracy",
          trControl=trainControl(method="repeatedcv", number=10, repeats=3))
fit.c50


# Evaluation of model accuracy
predict_c50<-predict.train(object=fit.c50,validation.set,type="raw")
confusionMatrix(predict_c50,validation.set$Dropout)

# Predict new data
pred.c50<-predict.train(object=fit.c50,transformed_test,type="raw")
pred.c50=as.data.frame(pred.c50)
# save the prediction 
write.csv(pred.c50,"D:/KAZI/UPWORK/KAGGLE COMPETITION/FEATURES/my_new_submission/submission_c50.csv")



# ////////////////////////////////////////////////////////////////////////////////////////////////

library(gbm)
set.seed(123)
fit.gbm<-train(Dropout~., data=train.set, method="gbm", metric="Accuracy",
          trControl=trainControl(method="repeatedcv", number=10,
                                 repeats=3), verbose=FALSE)
fit.gbm


# Evaluation of model accuracy
predict_gbm<-predict.train(object=fit.gbm,validation.set,type="raw")

confusionMatrix(predict_gbm,validation.set$Dropout)

# Predict new data
pred.gbm<-predict.train(object=fit.gbm,transformed_test,type="raw")
pred.gbm=as.data.frame(pred.gbm)
# save the prediction 
write.csv(pred.gbm,"D:/KAZI/UPWORK/KAGGLE COMPETITION/FEATURES/my_new_submission/submission_gbm.csv")


