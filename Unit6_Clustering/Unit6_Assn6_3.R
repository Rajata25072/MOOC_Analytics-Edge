# PREDICTING STOCK RETURNS WITH CLUSTER-THEN-PREDICT
# 
# In the second lecture sequence this week, we heard about cluster-then-predict, 
# a methodology in which you first cluster observations and then build cluster-specific 
# prediction models. 
# In the lecture sequence, we saw how this methodology helped improve the 
# prediction of heart attack risk. 
# In this assignment, we'll use cluster-then-predict to predict future stock prices using 
# historical stock data.
# 
# When selecting which stocks to invest in, investors seek to obtain good future returns. 
# In this problem, we will first use clustering to identify clusters of stocks that have 
# similar returns over time. 
# Then, we'll use logistic regression to predict whether or not the stocks will have 
# positive future returns.
# 
# For this problem, we'll use StocksCluster.csv, which contains monthly stock returns 
# from the NASDAQ stock exchange. 
# The NASDAQ is the second-largest stock exchange in the world, and it lists many 
# technology companies. 
# The stock price data used in this problem was obtained from infochimps, a website 
# providing access to many datasets.
# 
# Each observation in the dataset is the monthly returns of a particular company in a particular year. 
# The years included are 2000-2009. The companies are limited to tickers that were 
# listed on the exchange for the entire period 2000-2009, and whose stock price never fell below $1. 
# So, for example, one observation is for Yahoo in 2000, and another observation is for Yahoo in 2001. 
# Our goal will be to predict whether or not the stock return in December will be positive, 
# using the stock returns for the first 11 months of the year.
# 
# This dataset contains the following variables:
#   ReturnJan = the return for the company's stock during January (in the year of the observation). 
#   ReturnFeb = the return for the company's stock during February (in the year of the observation). 
#   ReturnMar = the return for the company's stock during March (in the year of the observation). 
#   ReturnApr = the return for the company's stock during April (in the year of the observation). 
#   ReturnMay = the return for the company's stock during May (in the year of the observation). 
#   ReturnJune = the return for the company's stock during June (in the year of the observation). 
#   ReturnJuly = the return for the company's stock during July (in the year of the observation). 
#   ReturnAug = the return for the company's stock during August (in the year of the observation). 
#   ReturnSep = the return for the company's stock during September (in the year of the observation). 
#   ReturnOct = the return for the company's stock during October (in the year of the observation). 
#   ReturnNov = the return for the company's stock during November (in the year of the observation). 
#   PositiveDec = whether or not the company's stock had a positive return in December 
#     (in the year of the observation). 
#     This variable takes value 1 if the return was positive, and value 0 if the return was not positive.
# 
#
# ################################################################################
# For the first 11 variables, the value stored is a proportional change in stock value during that month. 
# For instance, a value of 0.05 means the stock increased in value 5% during the month, 
# while a value of -0.02 means the stock decreased in value 2% during the month.
# 
# Problem 1.1 - Exploring the Dataset
# Load StocksCluster.csv into a data frame called "stocks". 
# How many observations are in the dataset?
stocks = read.csv("StocksCluster.csv")
nrow(stocks) #11580
# 
# 
# ################################################################################
# Problem 1.2 - Exploring the Dataset
# What proportion of the observations have positive returns in December?
table(stocks$PositiveDec)
table(stocks$PositiveDec)[2]/ nrow(stocks) #0.546114
mean(stocks$PositiveDec) #0.546114
# 
# 
# ################################################################################
# Problem 1.3 - Exploring the Dataset
# What is the maximum correlation between any two return variables in the dataset? 
# You should look at the pairwise correlations between ReturnJan, ReturnFeb, 
# ReturnMar, ReturnApr, ReturnMay, ReturnJune, ReturnJuly, ReturnAug, ReturnSep, 
# ReturnOct, and ReturnNov.
for (i in 1:11){
  print(sort(cor(stocks)[i,])[11])
}
# 0.1916728
# 
# 
# ################################################################################
# Problem 1.4 - Exploring the Dataset
# Which month (from January through November) has the largest mean return across |
# all observations in the dataset?
sort(colMeans(stocks[,1:11])) #April
# 
# 
# Which month (from January through November) has the smallest mean return across 
# all observations in the dataset?
sort(colMeans(stocks[,1:11])) #September
# 
# 
# ################################################################################
# Problem 2.1 - Initial Logistic Regression Model
# Run the following commands to split the data into a training set and testing set, 
# putting 70% of the data in the training set and 30% of the data in the testing set:
# 
set.seed(144)
library(caTools)
spl = sample.split(stocks$PositiveDec, SplitRatio = 0.7)
stocksTrain = subset(stocks, spl == TRUE)
stocksTest = subset(stocks, spl == FALSE)
# 
# Then, use the stocksTrain data frame to train a logistic regression model (name it StocksModel) 
# to predict PositiveDec using all the other variables as independent variables. 
# Don't forget to add the argument family=binomial to your glm command.
StocksModel = glm(PositiveDec ~ ., data=stocksTrain, family="binomial")
# 
# What is the overall accuracy on the training set, using a threshold of 0.5?
predictTrain = predict(StocksModel, type="response")
sum(table(stocksTrain$PositiveDec, predictTrain>0.5) * diag(x=1,2,2)) / nrow(stocksTrain) # 0.5711818
# 
# 
# ################################################################################
# Problem 2.2 - Initial Logistic Regression Model
# Now obtain test set predictions from StocksModel. 
# What is the overall accuracy of the model on the test, again using a threshold of 0.5?
predictTest = predict(StocksModel, newdata=stocksTest, type="response")
sum(table(stocksTest$PositiveDec, predictTest>0.5) * diag(x=1,2,2)) / nrow(stocksTest) # 0.5670697
# 
# 
# ################################################################################
# Problem 2.3 - Initial Logistic Regression Model
# What is the accuracy on the test set of a baseline model that always predicts 
# the most common outcome (PositiveDec = 1)?
table(stocksTest$PositiveDec)
table(stocksTest$PositiveDec)[2] / nrow(stocksTest) #0.5460564
# 
# 
# ################################################################################
# Problem 3.1 - Clustering Stocks
# Now, let's cluster the stocks. 
# The first step in this process is to remove the dependent variable using the following 
# commands:
# 
limitedTrain = stocksTrain
limitedTrain$PositiveDec = NULL
limitedTest = stocksTest
limitedTest$PositiveDec = NULL
# 
# Why do we need to remove the dependent variable in the clustering phase of 
# the cluster-then-predict methodology?
# 
# x Leaving in the dependent variable might lead to unbalanced clusters  
# x Removing the dependent variable decreases the computational effort needed to cluster  
# o Needing to know the dependent variable value to assign an observation to a cluster 
#    defeats the purpose of the methodology
# 
# 
# In cluster-then-predict, our final goal is to predict the dependent variable, which is unknown 
# to us at the time of prediction. 
# Therefore, if we need to know the outcome value to perform the clustering, the methodology is 
# no longer useful for prediction of an unknown outcome value.
# 
# This is an important point that is sometimes mistakenly overlooked. 
# If you use the outcome value to cluster, you might conclude your method strongly outperforms 
# a non-clustering alternative. 
# However, this is because it is using the outcome to determine the clusters, which is not valid.
# ################################################################################
# Problem 3.2 - Clustering Stocks
# In the market segmentation assignment in this week's homework, you were 
# introduced to the preProcess command from the caret package, which normalizes 
# variables by subtracting by the mean and dividing by the standard deviation.
# 
# In cases where we have a training and testing set, we'll want to normalize by the 
# mean and standard deviation of the variables in the training set. 
# We can do this by passing just the training set to the preProcess function:
# 
library(caret)
preproc = preProcess(limitedTrain)
normTrain = predict(preproc, limitedTrain)
normTest = predict(preproc, limitedTest)
# 
# What is the mean of the ReturnJan variable in normTrain?
mean(normTrain$ReturnJan) #2.100586e-17
# 
# 
# What is the mean of the ReturnJan variable in normTest?
mean(normTest$ReturnJan) #-0.0004185886
# 
# 
# ################################################################################
# Problem 3.3 - Clustering Stocks
# Why is the mean ReturnJan variable much closer to 0 in normTrain than in normTest?
# 
# x Small rounding errors exist in the normalization procedure  
# o The distribution of the ReturnJan variable is different in the training and testing set  
# x The distribution of the dependent variable is different in the training and testing set
# 
# 
# ################################################################################
# Problem 3.4 - Clustering Stocks
# Set the random seed to 144 (it is important to do this again, even though we did it earlier). 
# Run k-means clustering with 3 clusters on normTrain, storing the result in an object called km.
# 
set.seed(144)
km = kmeans(normTrain, centers = 3)
str(km)
# Which cluster has the largest number of observations?
table(km$cluster) # Cluster 2
# 
# 
# ################################################################################
# Problem 3.5 - Clustering Stocks
# Recall from the recitation that we can use the flexclust package to obtain training set 
# and testing set cluster assignments for our observations 
# (note that the call to as.kcca may take a while to complete):
# 
library(flexclust)
km.kcca = as.kcca(km, normTrain)
clusterTrain = predict(km.kcca)
clusterTest = predict(km.kcca, newdata=normTest)
# 
# How many test-set observations were assigned to Cluster 2?
table(clusterTest) #2080
# 
# 
# ################################################################################
# Problem 4.1 - Cluster-Specific Predictions
# Using the subset function, build data frames stocksTrain1, stocksTrain2, and 
# stocksTrain3, containing the elements in the stocksTrain data frame assigned to 
# clusters 1, 2, and 3, respectively (be careful to take subsets of stocksTrain, not of normTrain). 
# Similarly build stocksTest1, stocksTest2, and stocksTest3 from the stocksTest data frame.
# 
head(stocksTrain)
stocksTrain$clusterTrain = clusterTrain
stocksTrain1 = subset(stocksTrain[,1:12], clusterTrain==1)
stocksTrain2 = subset(stocksTrain[,1:12], clusterTrain==2)
stocksTrain3 = subset(stocksTrain[,1:12], clusterTrain==3)

stocksTest$clusterTest = clusterTest
stocksTest1 = subset(stocksTest[,1:12], clusterTest==1)
stocksTest2 = subset(stocksTest[,1:12], clusterTest==2)
stocksTest3 = subset(stocksTest[,1:12], clusterTest==3)
# Which training set data frame has the highest average value of the dependent variable?
mean(stocksTrain1$PositiveDec) #0.602
mean(stocksTrain2$PositiveDec) #0.514
mean(stocksTrain3$PositiveDec) #0.439
# stocksTrain1  
# 
#
# ################################################################################
# Problem 4.2 - Cluster-Specific Predictions
# Build logistic regression models StocksModel1, StocksModel2, and StocksModel3, 
# which predict PositiveDec using all the other variables as independent variables. 
# StocksModel1 should be trained on stocksTrain1, StocksModel2 should be trained on stocksTrain2, 
# and StocksModel3 should be trained on stocksTrain3.
StocksModel1 = glm(PositiveDec ~ ., data=stocksTrain1, family="binomial")
StocksModel2 = glm(PositiveDec ~ ., data=stocksTrain2, family="binomial")
StocksModel3 = glm(PositiveDec ~ ., data=stocksTrain3, family="binomial")
# 
# Which variables have a positive sign for the coefficient in at least one of 
# StocksModel1, StocksModel2, and StocksModel3 and a negative sign for the 
# coefficient in at least one of StocksModel1, StocksModel2, and StocksModel3? 
# Select all that apply.
StockModelCoeff = rbind(StocksModel1$coefficients, StocksModel2$coefficients, StocksModel3$coefficients)
rownames(StockModelCoeff) = c("Model1", "Model2", "Model3")
# 
# o ReturnJan  
# o ReturnFeb  
# o ReturnMar  
# x ReturnApr  
# x ReturnMay  
# o ReturnJune  
# x ReturnJuly  
# o ReturnAug  
# x ReturnSep  
# o ReturnOct  
# x ReturnNov
# 
#
# ################################################################################
# Problem 4.3 - Cluster-Specific Predictions
# Using StocksModel1, make test-set predictions called PredictTest1 on the data frame stocksTest1. 
# Using StocksModel2, make test-set predictions called PredictTest2 on the data frame stocksTest2. 
# Using StocksModel3, make test-set predictions called PredictTest3 on the data frame stocksTest3.
PredictTest1 = predict(StocksModel1, newdata=stocksTest1, type="response")
PredictTest2 = predict(StocksModel2, newdata=stocksTest2, type="response")
PredictTest3 = predict(StocksModel3, newdata=stocksTest3, type="response")
# 
# What is the overall accuracy of StocksModel1 on the test set stocksTest1, using a threshold of 0.5?
sum(table(stocksTest1$PositiveDec, PredictTest1>0.5)*diag(x=1,2,2)) / nrow(stocksTest1) #0.6194145
# 
# 
# What is the overall accuracy of StocksModel2 on the test set stocksTest2, using a threshold of 0.5?
sum(table(stocksTest2$PositiveDec, PredictTest2>0.5)*diag(x=1,2,2)) / nrow(stocksTest2) #0.5504808
# 
# 
# What is the overall accuracy of StocksModel3 on the test set stocksTest3, using a threshold of 0.5?
sum(table(stocksTest3$PositiveDec, PredictTest3>0.5)*diag(x=1,2,2)) / nrow(stocksTest3) #0.6458333
# 
# 
# ################################################################################
# Problem 4.4 - Cluster-Specific Predictions
# To compute the overall test-set accuracy of the cluster-then-predict approach, 
# we can combine all the test-set predictions into a single vector and all the true 
# outcomes into a single vector:
# 
AllPredictions = c(PredictTest1, PredictTest2, PredictTest3)
AllOutcomes = c(stocksTest1$PositiveDec, stocksTest2$PositiveDec, stocksTest3$PositiveDec)
# 
# What is the overall test-set accuracy of the cluster-then-predict approach, again 
# using a threshold of 0.5?
table(AllOutcomes)
sum(table(AllOutcomes, AllPredictions>0.5)*diag(x=1,2,2)) / length(AllPredictions) #0.5788716
# 
# 
# We see a modest improvement over the original logistic regression model. 
# Since predicting stock returns is a notoriously hard problem, this is a good increase in accuracy. 
# By investing in stocks for which we are more confident that they will have positive 
# returns (by selecting the ones with higher predicted probabilities), this cluster-then-predict model
# can give us an edge over the original logistic regression model.
#
#
# ################################################################################