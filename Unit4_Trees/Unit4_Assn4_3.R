# PREDICTING EARNINGS FROM CENSUS DATA
# 
# The United States government periodically collects demographic information by conducting 
# a census.
# 
# In this problem, we are going to use census information about an individual to predict how 
# much a person earns -- in particular, whether the person earns more than $50,000 per year. 
# This data comes from the UCI Machine Learning Repository.
# 
# The file census.csv contains 1994 census data for 31,978 individuals in the United States.
# 
# The dataset includes the following 13 variables:
#   
#   age = the age of the individual in years
#   workclass = the classification of the individual's working status (does the person work for 
#     the federal government, work for the local government, work without pay, and so on)
#   education = the level of education of the individual (e.g., 5th-6th grade, high school 
#     graduate, PhD, so on)
#   maritalstatus = the marital status of the individual
#   occupation = the type of work the individual does (e.g., administrative/clerical work, 
#     farming/fishing, sales and so on)
#   relationship = relationship of individual to his/her household
#   race = the individual's race
#   sex = the individual's sex
#   capitalgain = the capital gains of the individual in 1994 (from selling an asset such as a stock 
#     or bond for more than the original purchase price)
#   capitalloss = the capital losses of the individual in 1994 (from selling an asset such as a stock 
#     or bond for less than the original purchase price)
#   hoursperweek = the number of hours the individual works per week
#   nativecountry = the native country of the individual
#   over50k = whether or not the individual earned more than $50,000 in 1994
# 
#
# ################################################################################
# Problem 1.1 - A Logistic Regression Model
# Let's begin by building a logistic regression model to predict whether an individual's 
# earnings are above $50,000 (the variable "over50k") using all of the other variables as 
# independent variables. First, read the dataset census.csv into R.
census = read.csv("census.csv")
# Then, split the data randomly into a training set and a testing set, setting the seed to 2000 
# before creating the split. 
# Split the data so that the training set contains 60% of the observations, while the testing set 
# contains 40% of the observations.
library(caTools)
set.seed(2000) 
spl = sample.split(census$over50k, SplitRatio = 0.6)
train = subset(census, spl==TRUE)
test = subset(census, spl==FALSE)
# 
# Next, build a logistic regression model to predict the dependent variable "over50k", using all 
# of the other variables in the dataset as independent variables. 
# Use the training set to build the model.
logModel = glm(over50k ~ ., data=train, family="binomial")
# 
# Which variables are significant, or have factors that are significant? (Use 0.1 as your 
# significance threshold, so variables with a period or dot in the stars column should be 
# counted too. You might see a warning message here - you can ignore it and proceed. 
# This message is a warning that we might be overfitting our model to the training set.) 
# Select all that apply.
summary(logModel)
# o age  
# o workclass  
# o education  
# o maritalstatus  
# o occupation  
# o relationship  
# x race  
# o sex  
# o capitalgain  
# o capitalloss  
# o hoursperweek  
# x nativecountry
# 
# 
# ################################################################################
# Problem 1.2 - A Logistic Regression Model
# What is the accuracy of the model on the testing set? Use a threshold of 0.5. 
# (You might see a warning message when you make predictions on the test set - you can 
# safely ignore it.)
predictLog = predict(logModel, newdata = test, type="response")
sum(table(test$over50k, predictLog>0.5)*diag(x=1,2,2)) / nrow(test) #0.8552107
# 
# 
# ################################################################################
# Problem 1.3 - A Logistic Regression Model
# What is the baseline accuracy for the testing set?
table(test$over50k)
9713 / nrow(test) #0.7593621
# 
# 
# ################################################################################
# Problem 1.4 - A Logistic Regression Model
# What is the area-under-the-curve (AUC) for this model on the test set?
library(ROCR)
ROCRLog = prediction(predictLog, test$over50k)
plot(performance(ROCRLog, "tpr", "fpr"), colorize=TRUE, 
     print.cutoffs.at=seq(0,1,by=0.2), text.adj=c(-0.2,1.7))
as.numeric(performance(ROCRLog, "auc")@y.values) #0.9061598
# 
# 
# ################################################################################
# Problem 2.1 - A CART Model
# We have just seen how the logistic regression model for this data achieves a high accuracy. 
# Moreover, the significances of the variables give us a way to gauge which variables are 
# relevant for this prediction task. 
# However, it is not immediately clear which variables are more important than the others, 
# especially due to the large number of factor variables in this problem.
# 
# Let us now build a classification tree to predict "over50k". 
# Use the training set to build the model, and all of the other variables as independent 
# variables. 
# Use the default parameters, so don't set a value for minbucket or cp. 
# Remember to specify method="class" as an argument to rpart, since this is a classification 
# problem. 
# After you are done building the model, plot the resulting tree.
CARTb = rpart(over50k ~ ., data=train, method="class")
prp(CARTb)
# How many splits does the tree have in total?
# 4
# 
#
# ################################################################################
# Problem 2.2 - A CART Model
# Which variable does the tree split on at the first level (the very first split of the tree)?
# 
# relationship  
# 
#
# ################################################################################
# Problem 2.3 - A CART Model
# Which variables does the tree split on at the second level (immediately after the first split of 
# the tree)? 
# Select all that apply.
# 
# education  
# capitalgain  
# 
#
# ################################################################################
# Problem 2.4 - A CART Model
# What is the accuracy of the model on the testing set? Use a threshold of 0.5. 
# (You can either add the argument type="class", or generate probabilities and use a 
# threshold of 0.5 like in logistic regression.)
predictCART = predict(CARTb, newdata=test, type="class")
sum(table(test$over50k, predictCART)*diag(x=1,2,2))/nrow(test) #0.8473927
# 
# 
# This highlights a very regular phenomenon when comparing CART and logistic regression. 
# CART often performs a little worse than logistic regression in out-of-sample accuracy. 
# However, as is the case here, the CART model is often much simpler to describe and 
# understand.
# 
#
# ################################################################################
# Problem 2.5 - A CART Model
# Let us now consider the ROC curve and AUC for the CART model on the test set. 
# You will need to get predicted probabilities for the observations in the test set to build the 
# ROC curve and compute the AUC. 
# Remember that you can do this by removing the type="class" argument when making 
# predictions, and taking the second column of the resulting object.
# Plot the ROC curve for the CART model you have estimated. 
# Observe that compared to the logistic regression ROC curve, the CART ROC curve is less 
# smooth than the logistic regression ROC curve. 
# Which of the following explanations for this behavior is most correct? 
# (HINT: Think about what the ROC curve is plotting and what changing the threshold does.)

## ROC for Logistic
ROCRLog = prediction(predictLog, test$over50k)
plot(performance(ROCRLog, "tpr", "fpr"), colorize=TRUE, 
     print.cutoffs.at=seq(0,1,by=0.2), text.adj=c(-0.2,1.7))
as.numeric(performance(ROCRLog, "auc")@y.values) #0.9061598

## ROC for CART
predictCART = predict(CARTb, newdata=test)[,2]
ROCRCART = prediction(predictCART, test$over50k)
plot(performance(ROCRCART, "tpr", "fpr"), colorize=TRUE, 
     print.cutoffs.at=seq(0,1,by=0.2), text.adj=c(-0.2,1.7))
as.numeric(performance(ROCRCART, "auc")@y.values) #0.8470256
# 
# x The number of variables that the logistic regression model is based on is larger than the 
#   number of variables used by the CART model, so the ROC curve for the logistic regression 
#   model will be smoother.  
# x CART models require a higher number of observations in the testing set to produce a 
#   smoother/more continuous ROC curve; there is simply not enough data.  
# o The probabilities from the CART model take only a handful of values (five, one for each end 
#   bucket/leaf of the tree); the changes in the ROC curve correspond to setting the threshold 
#   to one of those values.  
# x The CART model uses fewer continuous variables than the logistic regression model 
#   (capitalgain for CART versus age, capitalgain, capitallosses, hoursperweek), which is why the 
#   CART ROC curve is less smooth than the logistic regression one.
# 
# Choice 1 is on the right track, but is incorrect, because the number of variables that you use in a 
#   model does not determine how the ROC curve looks. 
#   In particular, try fitting logistic regression with hourperweek as the only variable; 
#   you will see that the ROC curve is very smooth.
# 
# Choice 2 is not correct. The smoothness of the ROC curve will generally depend on the number of 
#   data points, but in the case of the particular CART model we have estimated, varying the amount of
#   testing set data will not change the qualitative behavior of the ROC curve.
# 
# Choice 3 is the correct answer. The breakpoints of the curve correspond to the false and 
#   true positive rates when the threshold is set to the five possible probability values.
# 
# Choice 4 is also not correct. In logistic regression, the continuity of an independent variable 
#   means that you will have a large range of predicted class probabilities in your test set data; 
#   this, in turn, means that you will see a large range of true and false positive rates as you 
#   change the threshold for generating predictions. 
#   In CART, the continuity of the variables does not at all affect the continuity of the predicted 
#   class probabilities; for our CART tree, there are only five possible probability values.
# 
# 
# ################################################################################
# Problem 2.6 - A CART Model
# What is the AUC of the CART model on the test set?
as.numeric(performance(ROCRCART, "auc")@y.values) #0.8470256
# 
# 
# ################################################################################
# Problem 3.1 - A Random Forest Model
# Before building a random forest model, we'll down-sample our training set. 
# While some modern personal computers can build a random forest model on the entire 
# training set, others might run out of memory when trying to train the model since random 
# forests is much more computationally intensive than CART or Logistic Regression. 
# For this reason, before continuing we will define a new training set to be used when building 
# our random forest model, that contains 2000 randomly selected obervations from the 
# original training set. 
# Do this by running the following commands in your R console 
# (assuming your training set is called "train"):
 set.seed(1)
 trainSmall = train[sample(nrow(train), 2000), ]
# 
# Let us now build a random forest model to predict "over50k", using the dataset "trainSmall" 
# as the data used to build the model. 
# Set the seed to 1 again right before building the model, and use all of the other variables in 
# the dataset as independent variables. (If you get an error that random forest "can not 
# handle categorical predictors with more than 32 categories", re-build the model without the 
# nativecountry variable as one of the independent variables.)
# 
# Then, make predictions using this model on the entire test set. 
# What is the accuracy of the model on the test set, using a threshold of 0.5? 
# (Remember that you don't need a "type" argument when making predictions with a random 
# forest model if you want to use a threshold of 0.5. Also, note that your accuracy might be 
# different from the one reported here, since random forest models can still differ depending 
# on your operating system, even when the random seed is set. )
library(randomForest)
randomForest50k = randomForest(over50k ~ ., data = trainSmall)
predictRF = predict(randomForest50k, newdata=test)
sum(table(test$over50k, predictRF)*diag(x=1,2,2)) / nrow(test)
# 
# ################################################################################
# Problem 3.2 - A Random Forest Model
# As we discussed in lecture, random forest models work by building a large collection of 
# trees. 
# As a result, we lose some of the interpretability that comes with CART in terms of seeing 
# how predictions are made and which variables are important. 
# However, we can still compute metrics that give us insight into which variables are 
# important.
# 
# One metric that we can look at is the number of times, aggregated over all of the trees in 
# the random forest model, that a certain variable is selected for a split. 
# To view this metric, run the following lines of R code (replace "MODEL" with the name of 
# your random forest model):
 vu = varUsed(randomForest50k, count=TRUE)
 vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
 dotchart(vusorted$x, names(randomForest50k$forest$xlevels[vusorted$ix]))
# 
# This code produces a chart that for each variable measures the number of times that 
# variable was selected for splitting (the value on the x-axis). 
# Which of the following variables is the most important in terms of the number of splits?
# 
# age  
# 
#
# ################################################################################
# Problem 3.3 - A Random Forest Model
# A different metric we can look at is related to "impurity", which measures how homogenous 
# each bucket or leaf of the tree is. In each tree in the forest, whenever we select a variable 
# and perform a split, the impurity is decreased. 
# Therefore, one way to measure the importance of a variable is to average the reduction in 
# impurity, taken over all the times that variable is selected for splitting in all of the trees in 
# the forest. 
# To compute this metric, run the following command in R (replace "MODEL" with the name 
# of your random forest model):
   varImpPlot(randomForest50k)
# Which one of the following variables is the most important in terms of mean reduction in 
# impurity?
# 
# occupation  
# 
#
# ################################################################################
# Problem 4.1 - Selecting cp by Cross-Validation
# We now conclude our study of this data set by looking at how CART behaves with different 
# choices of its parameters.
# 
# Let us select the cp parameter for our CART model using k-fold cross validation, 
# with k = 10 folds. 
# Do this by using the train function. 
# Set the seed beforehand to 2. 
# Test cp values from 0.002 to 0.1 in 0.002 increments, by using the following command:
   cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002))
# 
# Also, remember to use the entire training set "train" when building this model. 
# The train function might take some time to run.
library(rpart)
CARTb = rpart(over50k ~ ., data=train, method="class")
prp(CARTb)
set.seed(2)
library(caret)
# Number of folds
tr.control = trainControl(method = "cv", number = 10)
# cp values
cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002))
tr = train(over50k ~ ., data = train, method = "rpart", 
           trControl = tr.control, tuneGrid = cartGrid)
# Which value of cp does the train function recommend?
summary(tr) #0.002000000
# 
# The final value used for the model was cp = 0.002.
# 
# In other words, the best value was cp = 0.002, corresponding to the lowest cp value. 
# If we look more closely at the accuracy at different cp values, we can see that it seems to be
# decreasing steadily as the cp value increases. 
# Often, the cp value needs to become quite low before the accuracy begins to deteriorate.
# 
# ################################################################################
# Problem 4.2 - Selecting cp by Cross-Validation
# Fit a CART model to the training data using this value of cp. 
# What is the prediction accuracy on the test set?
CART50k = rpart(over50k ~ ., data=train, method="class", cp=0.002)
predictCART = predict(CART50k, newdata=test, type="class")
sum(table(test$over50k, predictCART)*diag(x=1,2,2))/nrow(test) #0.8612306
# 
# ################################################################################
# Problem 4.3 - Selecting cp by Cross-Validation
# Compared to the original accuracy using the default value of cp, this new CART model is an 
# improvement, and so we should clearly favor this new model over the old one -- or should 
# we? 
# Plot the CART tree for this model. How many splits are there?
prp(CART50k) #18!!
# 
# This highlights one important tradeoff in building predictive models. 
# By tuning cp, we improved our accuracy by over 1%, but our tree became significantly more 
# complicated. 
# In some applications, such an improvement in accuracy would be worth the loss in 
# interpretability. 
# In others, we may prefer a less accurate model that is simpler to understand and describe 
# over a more accurate -- but more complicated -- model.
# ################################################################################