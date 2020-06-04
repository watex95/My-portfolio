library(e1071)
library(foreach)
library(doParallel)



# In this problem, you will use support vector approaches in order to predict
# whether a given car gets high or low gas mileage based on the Auto data set
# in the ISLR library.
# --------------------------------------------------------------------------------

#Load libraries and dataset
library(ISLR)
library(dplyr)
library(mlr)
library(caret)
library(ROCR)
library(kernlab)

Auto=as.data.frame(Auto)

# (a) Create a binary variable that takes on a 1 for cars with gas mileage
# above the median, and a 0 for cars with gas mileage below the median.
med=median(Auto$mpg)
Auto$target=ifelse(Auto$mpg>med,'1','0')

#convert target to factor and drop the string variable for classification
# purposes  
Auto$target=as.factor(as.character(Auto$target)) #convert to factor
class(Auto$target)
Auto=select(Auto,-starts_with('name')) #drop name

# Change to fatcor and perform Variable encoding on the origin variable because its a categorical variable
Auto$origin=as.factor(as.character(Auto$origin))
Auto=createDummyFeatures(Auto, cols = "origin")

str(Auto)

# (b) Fit a support vector classifier to the data with various values of cost,
# in order to predict whether a car gets high or low gas mileage. Report
# the cross-validated training dataset error associated with different
# values of this parameter. Comment on your results.

# split the dataset
set.seed(123)
smp_size<- floor(0.85 * nrow(Auto))
train_ind<-sample(seq_len(nrow(Auto)), size = smp_size)
train.Auto<-Auto[train_ind, ]
test.Auto<-Auto[-train_ind, ]


# Fit the model
SVM1=tune(svm,target~.,data = train.Auto,ranges=list(cost=c(.01,.02,.05,
                .1,.2,.5,1,2,5,10)),kernel='linear',
            trControl = trainControl("cv", number = 5))

SVM1$best.model
summary(SVM1) #The error reduces as the model cross validates towards 10 folds



# (c) Now repeat (b), this time using SVMs with radial and polynomial basis
# kernels, with different values of gamma (sigma) and degree and cost.
# Comment on your results.


# RADIAL KERNEL
SVM2 = tune(svm ,target~.,data=train.Auto,ranges=list(
  cost=c(.01,.02,.05,.1,.2,.5,1,2,5,10),gamma=c(.001,.002,.005,
        .01,.02,.05,.1,.2,.5,1,2,5,10)),kernel='radial',
        trControl = trainControl("cv", number = 10),
        preProcess = c("center","scale"))

SVM2$best.model
summary(SVM2)



# POLYNOMIAL
SVM3 <- train(target ~., data = train.Auto, method = "svmPoly",
               trControl = trainControl("cv", number = 10),
               tuneGrid = expand.grid(C=c(.01,.02,.05,.1,.2,.5,1,2,5,10)
              ,degree=c(1:5),scale=c(0.01:1)),
               preProcess = c("center","scale"),
               tuneLength = 4)


SVM3$finalModel #displays cost , error, degree and scale of the model
SVM3$results #displays the accuracy of the model crossvalidated 


# ---------------------------------------------------------------------------------

# This problem involves the OJ data set which is part of the ISLR package.
library(ISLR)
library(caret)
library(kernlab)
library(ROCR)
library(e1071)
library(tidyverse)

OJ=as.data.frame(OJ)
str(OJ)

# (a) Create a training set containing a random sample of 60% of the observations,
# and a test set containing the remaining observations.
set.seed(123)
smp_size <-floor(0.60 * nrow(OJ))
train_ind <-sample(seq_len(nrow(OJ)), size = smp_size)
train.OJ = OJ[train_ind,]
test.OJ = OJ[-train_ind,]
dim(train.OJ)
dim(test.OJ)

# (b) Fit a support vector classifier with a linear kernel to the training
# data using cost=0.01, with Purchase as the response and the other variables
# as predictors. Describe the results obtained.

library(kernlab)
svm1 = train(Purchase~.,data=train.OJ,method='svmLinear' ,cost=0.01)
svm1$results #accuracy
svm1$finalModel #error 


# (c) What are the training and test error rates?
# Training error rates
svm1.train.pred = predict(svm1,newdata=train.OJ)
confusion.mat=table(obs=train.OJ$Purchase,pred=svm1.train.pred)
confusion.mat
# The above object is a confusion matrix thus grabbing the false positive and negative elements will give the error of the prediction
error_rate=(confusion.mat[1,2]+confusion.mat[2,1])/nrow(train.OJ)
error_rate #confirmed its equal to the value gotten earlier from the model

# Test error rates
svm1.test.pred = predict(svm1,newdata=test.OJ)
confusion.Mat=table(obs=test.OJ$Purchase,pred=svm1.test.pred)
confusion.Mat
# The above object is a confusion matrix thus grabbing the false positive and negative elements will give the error of the prediction
error_rate=(confusion.Mat[1,2]+confusion.Mat[2,1])/nrow(test.OJ)
error_rate



# (d) Use a tuning grid in caret to select an optimal cost. Consider values
# in the range 0.01 to 10.
svm1.tune = tune(svm,Purchase~.,data=train.OJ,
    ranges=list(cost=c(.01,.02,.05,.1,.2,.5,1,2,5,10)),kernel='linear')
summary(svm1.tune)



# (e) Compute the training and test error rates using this new value for cost.
# Train error rates
svm1.best.train.pred = predict(svm1.tune$best.model,newdata=train.OJ)
confusion.Mat=table(obs=train.OJ$Purchase,pred=svm1.best.train.pred)
confusion.Mat
# The above object is a confusion matrix thus grabbing the false positive and negative elements will give the error of the prediction
error_rate=(confusion.Mat[1,2]+confusion.Mat[2,1])/nrow(train.OJ)
error_rate

# Test error rates
svm1.best.test.pred=predict(svm1.tune$best.model,newdata=test.OJ)
confusion.Mat=table(obs=test.OJ$Purchase,pred=svm1.best.test.pred)
# The above object is a confusion matrix thus grabbing the false positive and negative elements will give the error of the prediction
error_rate=(confusion.Mat[1,2]+confusion.Mat[2,1])/nrow(test.OJ)
error_rate



# (f) Repeat parts (b) through (e) using a support vector machine with a
# radial kernel. Use a grid of values for gamma but use default for cost
svm2.tune = tune(svm , Purchase~.,data=train.OJ,ranges=list(
  cost=c(.01,.02,.05,.1,.2,.5,1,2,5,10),gamma=c(.001,.002,.005,
  .01,.02,.05,.1,.2,.5,1,2,5,10)),kernel='radial')
summary(svm2.tune)
svm2.tune$best.performance

# (i) Compute the training and test error rates using this new value for cost.
# Train error rates
svm2.best.train.pred = predict(svm2.tune$best.model,newdata=train.OJ)
confusion.Mat=table(obs=train.OJ$Purchase,pred=svm2.best.train.pred)
confusion.Mat
# The above object is a confusion matrix thus grabbing the false positive and negative elements will give the error of the prediction
error_rate=(confusion.Mat[1,2]+confusion.Mat[2,1])/nrow(train.OJ)
error_rate


# Test error rates
svm2.best.test.pred = predict(svm2.tune$best.model,newdata=test.OJ)
confusion.Mat=table(obs=test.OJ$Purchase,pred=svm2.best.test.pred)
confusion.Mat
# The above object is a confusion matrix thus grabbing the false positive and negative elements will give the error of the prediction
error_rate=(confusion.Mat[1,2]+confusion.Mat[2,1])/nrow(test.OJ)
error_rate




# (g) Repeat parts (b) through (e) using a support vector machine with a
# polynomial kernel.  Tune degree and cost using a grid.

# Fit the model on the training set
set.seed(123)
model <- train(Purchase ~., data = train.OJ, method = "svmPoly",
  trControl = trainControl("cv", number = 10),
  tuneGrid = expand.grid(C=c(.01,.02,.05,.1,.2,.5,1,2,5,10)
          ,degree=c(1:5),scale=c(0.01:1)),
  preProcess = c("center","scale"),
  tuneLength = 4)

# Print the best tuning parameter sigma and C that
# maximizes model accuracy
model$bestTune
model$finalModel #training error

# (i) Compute the training and test error rates using this new value for cost.
# Train error rates
svm3.best.train.pred <- model %>% predict(train.OJ)
confusion.Mat=table(obs=train.OJ$Purchase,pred=svm3.best.train.pred)
confusion.Mat
# The above object is a confusion matrix thus grabbing the false positive and negative elements will give the error of the prediction
error_rate=(confusion.Mat[1,2]+confusion.Mat[2,1])/nrow(train.OJ)
error_rate #training error after tuning



# Test error rates
# Make predictions on the test data
svm3.best.test.pred <- model %>% predict(test.OJ)
confusion.Mat=table(obs=test.OJ$Purchase,pred=svm3.best.test.pred)
confusion.Mat
# The above object is a confusion matrix thus grabbing the false positive and negative elements will give the error of the prediction
error_rate=(confusion.Mat[1,2]+confusion.Mat[2,1])/nrow(test.OJ)
error_rate #test error


# (h) Overall, which approach seems to give the best results on this data?
  



















































