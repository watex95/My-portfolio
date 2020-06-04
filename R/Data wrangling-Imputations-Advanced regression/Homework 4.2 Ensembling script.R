
# ---------------------------------------------------------------------------

# Use the Carseats data set in ISLR package
library(ISLR)
library(tree)


carseats=as.data.frame(Carseats)
str(carseats)
dim(carseats)

# (a) Split the data set into a training set and a test set.
set.seed(123)
smp_size <-floor(0.80 * nrow(carseats))
smp_size

train_ind <-sample(seq_len(nrow(carseats)), size = smp_size)
train.carseats = carseats[train_ind,]
dim(train.carseats)
test.carseats = carseats[-train_ind,]
dim(test.carseats)


# (b) Fit a regression tree to the training set by selecting an appropriate
# target variable to predict. Plot the tree, and interpret the results.
# What test MSE do you obtain?

tree.carseats = tree(Sales~.,data=train.carseats)
summary.carseats = summary(tree.carseats)
summary.carseats

plot(tree.carseats)
text(tree.carseats,pretty=0)

tree.pred = predict(tree.carseats, newdata=test.carseats)
mean((tree.pred-test.carseats$Sales)^2)

# (c) Use cross-validation in order to determine the optimal level of tree
# complexity. 

cv.carseats = cv.tree(tree.carseats)
names(cv.carseats)
cv.carseats

#plot the deviance against the size of the tree model

plot(cv.carseats$size, cv.carseats$dev,type="b")
points(which.min(cv.carseats$dev),cv.carseats$dev[which.min(cv.carseats$dev)],col="red",pch=19,cex=1.25)

prune.carseats = prune.tree(tree.carseats,best = which.min(cv.carseats$dev))
plot(prune.carseats)
text(prune.carseats,pretty=0)

prune.pred = predict(prune.carseats, test.carseats)
prune.pred
mean((prune.pred-test.carseats$Sales)^2)

# (d) Use the bagging approach to do the prediction. What test MSE do you 
# obtain? Which variables are most important? Plot the variable importance.

library(randomForest)
set.seed(1)

#Bagging 
bag.carseats = randomForest(Sales~.,data=train.carseats,mtry=10,importance=T)
bag.carseats
#Prediction and mean squared error
yhat.bag = predict(bag.carseats,newdata=test.carseats)
mean((yhat.bag - test.carseats$Sales)^2)

importance(bag.carseats)

varImpPlot(bag.carseats)


# (e) Use random forest to do the prediction. What test MSE do you obtain? 
# Which variables are most important. Plot the variable importance. 
# Try various values of m, the number of variables considered at each
# split and describe the effect of m on the error rate.

rf5.carseats = randomForest(Sales~.,data=train.carseats,mtry=5,importance=T)

rf3.carseats = randomForest(Sales~.,data=train.carseats,mtry=3,importance=T)
rf5.carseats
rf3.carseats
yhat5.rf = predict(rf5.carseats,newdata=test.carseats)
yhat3.rf = predict(rf3.carseats,newdata=test.carseats)

mean((yhat5.rf-test.carseats$Sales)^2)
mean((yhat3.rf-test.carseats$Sales)^2)
par(mfrow=c(2,2))
varImpPlot(rf5.carseats)
varImpPlot(rf3.carseats)




# -----------------------------------------------------------------------------------

# We now use boosting to predict Salary in the Hitters data set in the ISLR package.

library(ISLR)
Hitters=as.data.frame(Hitters)
dim(Hitters)
str(Hitters)

# (a) Remove the observations for whom the salary information is unknown, and
# then log-transform the salaries.

# remove rows with missing values 
Hitters = Hitters[!is.na(Hitters$Salary),]

#log transform salary
Hitters$Salary = log(Hitters$Salary)

# (b) Create a training set consisting of 65% of the observations, and a test
# set consisting of the remaining observations.
set.seed(123)
smp_size <-floor(0.65 * nrow(Hitters))
train_ind <-sample(seq_len(nrow(Hitters)), size = smp_size)
train.Hitters = Hitters[train_ind,]
dim(train.Hitters)
test.Hitters = Hitters[-train_ind,]
dim(test.Hitters)



# (c) Perform boosting using XGBoost on the training set with 1,000 trees 
# for a range of values of the shrinkage parameter ??. Produce a plot with 
# different shrinkage values on the x-axis and the corresponding training set
# MSE on the y-axis.


library(xgboost)
library(caret) 
library(gbm)

# Ensure the R-studio can handle plots with large margins
par("mar")
par(mar=c(1,1,1,1))


set.seed(1)
shrinkGrid = seq(.1,.001,by=-.001)
shrinkGrid
MSE = matrix(NA,nrow=length(shrinkGrid),ncol=2)


for(i in 1:length(shrinkGrid)){
  lambda = shrinkGrid[i]
  boost.hitters = gbm(Salary~.,data=train.Hitters,distribution="gaussian",n.trees=1000,shrinkage=lambda)
  boost.pred.train = predict(boost.hitters,train.Hitters,n.trees=1000)
  boost.pred.test = predict(boost.hitters,test.Hitters,n.trees=1000)
  train.MSE = mean((boost.pred.train - train.Hitters$Salary)^2)
  test.MSE = mean((boost.pred.test - test.Hitters$Salary)^2)
  MSE[i,1] = train.MSE
  MSE[i,2] = test.MSE
}
best.lambda = shrinkGrid[which.min(MSE[,2])]
best.lambda

# The plot
matplot(x=shrinkGrid,y=MSE,type="l",xlab="lambda",ylab="MSE")
legend("topright",legend = c("Train","Test"),col=c("black","red"),lty=c(1,2))
abline(v=best.lambda,col="blue",lty=2,lwd=.5)
print(paste0("Best Test MSE: ", MSE[which.min(MSE[,2]),2]))




# (d) Compare the test MSE of boosting to the test MSE that results from 
# applying a linear regression approach.

# We shall apply two regression approaches; (ridge and lasso) 
library(glmnet)

x.train.Hitters = model.matrix(Salary~.,train.Hitters)[,-1]
x.test.Hitters = model.matrix(Salary~.,test.Hitters)[,-1]

y.train.Hitters = train.Hitters$Salary
y.test.Hitters = test.Hitters$Salary

lambdaGrid = 10^seq(10,-2,length=100)

cv.ridge = cv.glmnet(x.train.Hitters,y.train.Hitters,alpha=0)
bestlam.ridge = cv.ridge$lambda.min

cv.lasso = cv.glmnet(x.train.Hitters,y.train.Hitters,alpha=1)
bestlam.lasso = cv.lasso$lambda.min

ridge.hitters = glmnet(x.train.Hitters,y.train.Hitters,alpha=0,lambda=lambdaGrid)
lasso.hitters = glmnet(x.train.Hitters,y.train.Hitters,alpha=1,lambda=lambdaGrid)

ridge.pred = predict(ridge.hitters,s=bestlam.ridge,newx=x.test.Hitters)
lasso.pred = predict(lasso.hitters,s=bestlam.lasso,newx=x.test.Hitters)

ridge.MSE.test = mean((ridge.pred - y.test.Hitters)^2)
lasso.MSE.test = mean((lasso.pred - y.test.Hitters)^2)

print(paste0("Boost Test MSE: ", MSE[which.min(MSE[,2]),2]))
print(paste0("Ridge Test MSE: ", ridge.MSE.test))
print(paste0("Lasso Test MSE: ", lasso.MSE.test))

# (e) Which variables appear to be the most important predictors in the 
# boosted model?

summary(boost.hitters)

# (f) Now apply bagging to the training set. What is the test set MSE for 
# this approach?
  
library(randomForest)
bag.hitters = randomForest(Salary~.,data=train.Hitters,ntree=1000,mtry=19,importance=TRUE)
bag.pred = predict(bag.hitters,newdata=test.Hitters)
bag.MSE.test = mean((bag.pred - test.Hitters$Salary)^2)
print(paste0("Bagging Test MSE: ",bag.MSE.test))
# Bagging appears to have slightly outperformed boosting, ridge, and lasso.
  
  



# -------------------------------------------------------------------------------   
# This question uses the Caravan data set in the ISLR package.
# (a) Create a training set consisting of 75% of the observations, and a 
# test set consisting of the remaining observations.

# load libraries and dataset
library(ISLR)
Caravan=as.data.frame(Caravan)


# Ensure the response variable is a factor
Caravan$Purchase=as.factor(as.numeric(Caravan$Purchase))
class(Caravan$Purchase) 
 
 set.seed(123)
 smp_size <-floor(0.75 * nrow(Caravan))
 train_ind <-sample(seq_len(nrow(Caravan)), size = smp_size)
 train.Caravan = Caravan[train_ind,]
 dim(train.Caravan)
 test.Caravan= Caravan[-train_ind,]
 dim(test.Caravan) 
 table(Caravan$Purchase)
 
 # (b) Fit a boosting model to the training set with Purchase as the response
 # and the other variables as predictors. Use 1,000 trees, and a shrinkage 
 # value of 0.01. Which predictors appear to be the most important?

caravan.boost=gbm(formula=Purchase ~ .,data=train.Caravan, n.trees=1000,
                     shrinkage=.01,distribution='gaussian')

summary(caravan.boost)


probFromLogit = function(z){
   exp(z)/(1+exp(z))
 }
 
   
# (c) Use the boosting model to predict the response on the test data.
 # Form a confusion matrix. What fraction of the people predicted to make
 # a purchase do in fact make one? How does this compare with the results 
 # obtained from applying logistic regression to this data set?
 
 caravan.probs = predict(caravan.boost, newdata=test.Caravan,n.trees=1000,type="response")
 caravan.pred = ifelse(caravan.probs>.2,"Yes","No")
 test.y = ifelse(test.Caravan$Purchase==1,"Yes","No")
 table(caravan.pred,test.y)
 
 
 # Logistic model
 caravan.logit = predict(caravan.boost, newdata=test.Caravan,n.trees=1000)
 caravan.probs.manual = sapply(caravan.logit,probFromLogit)
 
 
 
 
# (d) Try a stacking ensemble with at least two different models.  What is 
# the accuracy you get?
   


 
 caravan.logit = predict(caravan.boost, newdata=test.Caravan,n.trees=1000)
 caravan.probs.manual = sapply(caravan.logit,probFromLogit)
 
 caravan.probs = predict(caravan.boost, newdata=test.Caravan,n.trees=1000,type="response")
 #mean(round(caravan.probs.manual,5) != round(caravan.probs,5))
 
 caravan.pred = ifelse(caravan.probs>.2,"Yes","No")
 test.y = ifelse(test.Caravan$Purchase==1,"Yes","No")
 
 table(caravan.pred,test.y)
 
 mean(caravan.pred!=test.y)
 