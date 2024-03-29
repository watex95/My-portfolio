
# ---------------------------------------------------------------------------

# Use the Carseats data set in ISLR package
library(ISLR)
Carseats=as.data.frame(Carseats)

# (a) Split the data set into a training set and a test set.

set.seed(123)
smp_size <-floor(0.80 * nrow(Carseats))
train_ind <-sample(seq_len(nrow(Carseats)), size = smp_size)
train.carseats=train_ind[,Carseats]
test.carseats=train_ind[,-Carseats]
dim(train.carseats)
dim(test.carseats)


# (b) Fit a regression tree to the training set by selecting an appropriate
# target variable to predict. Plot the tree, and interpret the results.
# What test MSE do you obtain?


# (c) Use cross-validation in order to determine the optimal level of tree complexity. 


# (d) Use the bagging approach to do the prediction. What test MSE do you 
# obtain? Which variables are most important? Plot the variable importance.


# (e) Use random forest to do the prediction. What test MSE do you obtain?  Which variables are most important. Plot the variable importance. Try various values of m, the number of variables considered at each split and describe the effect of m on the error rate.



We now use boosting to predict Salary in the Hitters data set in the ISLR package.
(a) Remove the observations for whom the salary information is unknown, and then log-transform the salaries.
(b) Create a training set consisting of 65% of the observations, and a test set consisting of the remaining observations.
(c) Perform boosting using XGBoost on the training set with 1,000 trees for a range of values of the shrinkage parameter ??. Produce a plot with different shrinkage values on the x-axis and the corresponding training set MSE on the y-axis.
(d) Compare the test MSE of boosting to the test MSE that results from applying an linear regression approach.
(e) Which variables appear to be the most important predictors in the boosted model?
(f) Now apply bagging to the training set. What is the test set MSE for this approach?
  
  
  
  
  
  
#This question uses the Caravan data set in the ISLR package.
#(a) Create a training set consisting of 75% of the observations and a test set
#consisting of the remaining observations.

  
#(b) Fit a boosting model to the training set with Purchase as the response and
#the other variables as predictors. Use 1,000 trees, and a shrinkage value
#of 0.01. Which predictors appear to be the most important?



#(c) Use the boosting model to predict the response on the test data.
#Form a confusion matrix. What fraction of the people predicted to make a purchase
#do in fact make one?



#How does this compare with the results obtained from applying logistic regression
#to this data set?


#(d) Try a stacking ensemble with at least two different models.


#What is the accuracy you get?
  