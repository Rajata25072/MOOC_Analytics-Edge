# Unit 4 - "Judge, Jury, and Classifier" Lecture


# VIDEO 4

# Read in the data
stevens = read.csv("stevens.csv")
str(stevens)

# Split the data
library(caTools)
set.seed(3000)
spl = sample.split(stevens$Reverse, SplitRatio = 0.7)
Train = subset(stevens, spl==TRUE)
Test = subset(stevens, spl==FALSE)

# Install rpart library
install.packages("rpart")
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)

# CART model
StevensTree = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt 
                    + Unconst, data = Train, method="class", minbucket=25)

prp(StevensTree)

################################################################################
# Quick Question - Video 4
# 
# Compute the AUC of the CART model from the previous video, using the following 
# command in your R console:

as.numeric(performance(pred, "auc")@y.values)

# What is the AUC?
# 0.6927105 


# Now, recall that in Video 4, our tree had 7 splits. 
# Let's see how this changes if we change the value of minbucket.

# First build a CART model that is similar to the one we built in Video 4, 
# except change the minbucket parameter to 5. 
# Plot the tree.

# How many splits does the tree have?
StevensTree = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt 
                    + Unconst, data = Train, method="class", minbucket=5)
prp(StevensTree) #16

# Now build a CART model that is similar to the one we built in Video 4, 
# except change the minbucket parameter to 100. Plot the tree.

# How many splits does the tree have?
StevensTree = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt 
                    + Unconst, data = Train, method="class", minbucket=100)
prp(StevensTree) #1
################################################################################

# CART model
StevensTree = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt 
                    + Unconst, data = Train, method="class", minbucket=25)

prp(StevensTree)

# Make predictions
PredictCART = predict(StevensTree, newdata = Test, type = "class")
table(Test$Reverse, PredictCART)
(41+71)/(41+36+22+71)

# ROC curve
library(ROCR)

PredictROC = predict(StevensTree, newdata = Test)
PredictROC

pred = prediction(PredictROC[,2], Test$Reverse)
perf = performance(pred, "tpr", "fpr")
plot(perf)



# VIDEO 5 - Random Forests

# Install randomForest package
install.packages("randomForest")
library(randomForest)

# Build random forest model
StevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, ntree=200, nodesize=25 )

# Convert outcome to factor
Train$Reverse = as.factor(Train$Reverse)
Test$Reverse = as.factor(Test$Reverse)

# Try again
StevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, ntree=200, nodesize=25 )

# Make predictions
PredictForest = predict(StevensForest, newdata = Test)
table(Test$Reverse, PredictForest)
(40+74)/(40+37+19+74)


################################################################################
# Quick Question - Video 5
# 
# IMPORTANT NOTE: When creating random forest models, you might still get different 
# answers from the ones you see here even if you set the random seed. 
# This has to do with different operating systems and the random forest implementation.
# 
# Let's see what happens if we set the seed to two different values and create 
# two different random forest models.
# 
# First, set the seed to 100, and the re-build the random forest model, 
# exactly like we did in the previous video (Video 5). 
# Then make predictions on the test set. What is the accuracy of the model on 
# the test set?
set.seed(100)

# Convert outcome to factor
Train$Reverse = as.factor(Train$Reverse)
Test$Reverse = as.factor(Test$Reverse)

# Build random forest model
StevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, ntree=200, nodesize=25 )

# Make predictions
PredictForest = predict(StevensForest, newdata = Test)
table(Test$Reverse, PredictForest)
(41+75)/(41+36+18+75) #0.6823529

#  
# Now, set the seed to 200, and then re-build the random forest model, 
# exactly like we did in the previous video (Video 5). 
# Then make predictions on the test set. What is the accuracy of this model on 
# the test set?

# the test set?
set.seed(200)

# Convert outcome to factor
Train$Reverse = as.factor(Train$Reverse)
Test$Reverse = as.factor(Test$Reverse)

# Build random forest model
StevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, ntree=200, nodesize=25 )

# Make predictions
PredictForest = predict(StevensForest, newdata = Test)
table(Test$Reverse, PredictForest)
(41+75)/(41+36+18+75) #0.6823529

# As we see here, the random component of the random forest method can change 
# the accuracy. The accuracy for a more stable dataset will not change very much, 
# but a noisy dataset can be significantly affected by the random samples.

################################################################################

# VIDEO 6

# Install cross-validation packages
install.packages("caret")
library(caret)
install.packages("e1071")
library(e1071)

# Define cross-validation experiment
numFolds = trainControl( method = "cv", number = 10 )
cpGrid = expand.grid( .cp = seq(0.01,0.5,0.01)) 

# Perform the cross validation
train(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method = "rpart", trControl = numFolds, tuneGrid = cpGrid )

# Create a new CART model
StevensTreeCV = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method="class", cp = 0.18)

prp(StevensTreeCV)

# Make predictions
PredictCV = predict(StevensTreeCV, newdata = Test, type = "class")

table(Test$Reverse, PredictCV)
(59+64)/(59+18+29+64)

# The tree with the best accuracy only has one split! 
# When we were picking different minbucket parameters before, i
# t seemed like this tree was probably not doing a good job of fitting the data. 
# However, this tree with one split gives us the best out-of-sample accuracy. 
# This reminds us that sometimes the simplest models are the best!