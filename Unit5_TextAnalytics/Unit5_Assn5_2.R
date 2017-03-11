# AUTOMATING REVIEWS IN MEDICINE
# 
# The medical literature is enormous. Pubmed, a database of medical publications 
# maintained by the U.S. National Library of Medicine, has indexed over 23 million 
# medical publications. 
# Further, the rate of medical publication has increased over time, and now there are 
# nearly 1 million new publications in the field each year, or more than one per 
# minute.
# 
# The large size and fast-changing nature of the medical literature has increased the 
# need for reviews, which search databases like Pubmed for papers on a particular 
# topic and then report results from the papers found. 
# While such reviews are often performed manually, with multiple people reviewing 
# each search result, this is tedious and time consuming. 
# In this problem, we will see how text analytics can be used to automate the process 
# of information retrieval.
# 
# The dataset consists of the titles (variable title) and abstracts (variable abstract) of 
# papers retrieved in a Pubmed search. 
# Each search result is labeled with whether the paper is a clinical trial testing a drug 
# therapy for cancer (variable trial). 
# These labels were obtained by two people reviewing each search result and 
# accessing the actual paper if necessary, as part of a literature review of clinical trials 
# testing drug therapies for advanced and metastatic breast cancer.
# 
# 
# ################################################################################
# Problem 1.1 - Loading the Data
# Load clinical_trial.csv into a data frame called trials (remembering to add the 
# argument stringsAsFactors=FALSE), and investigate the data frame with summary() 
# and str().
 trials = read.csv("clinical_trial.csv", stringsAsFactors=FALSE)

# IMPORTANT NOTE: Some students have been getting errors like "invalid multibyte 
# string" when performing certain parts of this homework question. 
# If this is happening to you, use the argument fileEncoding="latin1" when reading in 
# the file with read.csv. This should cause those errors to go away.
# 
# We can use R's string functions to learn more about the titles and abstracts of the 
# located papers. 
# The nchar() function counts the number of characters in a piece of text. 
# Using the nchar() function on the variables in the data frame, answer the following 
# questions:
str(trials)
# How many characters are there in the longest abstract? 
# (Longest here is defined as the abstract with the largest number of characters.)
max(nchar(trials$abstract)) #3708
# 
# 
# ################################################################################
# Problem 1.2 - Loading the Data
# How many search results provided no abstract? 
# (HINT: A search result provided no abstract if the number of characters in the 
# abstract field is zero.)
sum(nchar(trials$abstract)==0) #112
# 
# 
# ################################################################################
# Problem 1.3 - Loading the Data
# Find the observation with the minimum number of characters in the title 
# (the variable "title") out of all of the observations in this dataset. 
# What is the text of the title of this article? Include capitalization and punctuation in 
# your response, but don't include the quotes.
summary(trials$title)
which.min(nchar(trials$title)) #1258
trials[which.min(nchar(trials$title)),1] #A decade of letrozole: FACE.
# 
# 
# ################################################################################
# Problem 2.1 - Preparing the Corpus
# Because we have both title and abstract information for trials, we need to build two 
# corpera instead of one. 
# Name them corpusTitle and corpusAbstract.
# Following the commands from lecture, perform the following tasks 
# (you might need to load the "tm" package first if it isn't already loaded). 
# Make sure to perform them in this order.
# 1) Convert the title variable to corpusTitle and the abstract variable to corpusAbstract.
library(tm)
library(SnowballC)
str(trials)
corpusTitle = Corpus(VectorSource(trials$title))
corpusAbstract = Corpus(VectorSource(trials$abstract))
# 2) Convert corpusTitle and corpusAbstract to lowercase. After performing this step, 
#   remember to run the lines:
#      corpusTitle = tm_map(corpusTitle, PlainTextDocument)
#      corpusAbstract = tm_map(corpusAbstract, PlainTextDocument)
corpusTitle = tm_map(corpusTitle, tolower)
corpusAbstract = tm_map(corpusAbstract, tolower)
corpusTitle = tm_map(corpusTitle, PlainTextDocument)
corpusAbstract = tm_map(corpusAbstract, PlainTextDocument)
# 3) Remove the punctuation in corpusTitle and corpusAbstract.
corpusTitle = tm_map(corpusTitle, removePunctuation)
corpusAbstract = tm_map(corpusAbstract, removePunctuation)
# 4) Remove the English language stop words from corpusTitle and corpusAbstract.
corpusTitle = tm_map(corpusTitle, removeWords, stopwords("english"))
corpusAbstract = tm_map(corpusAbstract, removeWords, stopwords("english"))
# 5) Stem the words in corpusTitle and corpusAbstract (each stemming might take a 
#     few minutes).
corpusTitle = tm_map(corpusTitle, stemDocument)
corpusAbstract = tm_map(corpusAbstract, stemDocument)
# 6) Build a document term matrix called dtmTitle from corpusTitle and dtmAbstract 
#     from corpusAbstract.
dtmTitle = DocumentTermMatrix(corpusTitle) #2835
dtmAbstract = DocumentTermMatrix(corpusAbstract)
# 7) Limit dtmTitle and dtmAbstract to terms with sparseness of at most 95% 
#     (aka terms that appear in at least 5% of documents).
dtmTitle = removeSparseTerms(dtmTitle, 0.95) #31
dtmAbstract = removeSparseTerms(dtmAbstract, 0.95)
# 8) Convert dtmTitle and dtmAbstract to data frames (keep the names dtmTitle and 
#     dtmAbstract).
dtmTitle = as.data.frame(as.matrix(dtmTitle))
dtmAbstract = as.data.frame(as.matrix(dtmAbstract))
# If the code length(stopwords("english")) does not return 174 for you, then please run 
# the line of code in this file, which will store the standard stop words in a variable 
# called sw. 
# When removing stop words, use tm_map(corpusTitle, removeWords, sw) and 
# tm_map(corpusAbstract, removeWords, sw) instead of tm_map(corpusTitle, 
# removeWords, stopwords("english")) and tm_map(corpusAbstract, removeWords, 
# stopwords("english")).
# 
# How many terms remain in dtmTitle after removing sparse terms 
# (aka how many columns does it have)?
ncol(dtmTitle) #31
# 
# How many terms remain in dtmAbstract?
ncol(dtmAbstract) #335
# 
# 
# ################################################################################
# Problem 2.2 - Preparing the Corpus
# What is the most likely reason why dtmAbstract has so many more terms than 
# dtmTitle?
# 
# o Abstracts tend to have many more words than titles  
# x Abstracts tend to have a much wider vocabulary than titles  
# x More papers have abstracts than titles
# 
# 
# ################################################################################
# Problem 2.3 - Preparing the Corpus
# What is the most frequent word stem across all the abstracts? 
# Hint: you can use colSums() to compute the frequency of a word across all the 
# abstracts.
head(sort(colSums(dtmAbstract), decreasing=TRUE)) #patient
# 
# 
# ################################################################################
# Problem 3.1 - Building a model
# We want to combine dtmTitle and dtmAbstract into a single data frame to make 
# predictions. 
# However, some of the variables in these data frames have the same names. 
# To fix this issue, run the following commands:
   colnames(dtmTitle) = paste0("T", colnames(dtmTitle))
   colnames(dtmAbstract) = paste0("A", colnames(dtmAbstract))
# What was the effect of these functions?
# 
# x Removing the words that are in common between the titles and the abstracts.  
# o Adding the letter T in front of all the title variable names and adding the letter A in 
#     front of all the abstract variable names.  
# x Adding the letter T in front of all the title variable names that also appear in the 
#     abstract data frame, and adding an A in front of all the abstract variable names that 
#     appear in the title data frame.
# 
# 
# ################################################################################
# Problem 3.2 - Building a Model
# Using cbind(), combine dtmTitle and dtmAbstract into a single data frame called dtm:
   dtm = cbind(dtmTitle, dtmAbstract)
# As we did in class, add the dependent variable "trial" to dtm, copying it from the 
# original data frame called trials. 
# How many columns are in this combined data frame?
dtm$trial = trials$trial
ncol(dtm) #367
# 
# 
# ################################################################################
# Problem 3.3 - Building a Model
# Now that we have prepared our data frame, it's time to split it into a training and 
# testing set and to build regression models. 
# Set the random seed to 144 and use the sample.split function from the caTools 
# package to split dtm into data frames named "train" and "test", putting 70% of the 
# data in the training set.
library(caTools)
set.seed(144) 
spl = sample.split(dtm$trial, SplitRatio = 0.7)
train = subset(dtm, spl==TRUE)
test = subset(dtm, spl==FALSE)
# What is the accuracy of the baseline model on the training set? 
# (Remember that the baseline model predicts the most frequent outcome in the 
# training set for all observations.)
table(train$trial)
730 / nrow(train) #0.5606759
# 
# 
# ################################################################################
# Problem 3.4 - Building a Model
# Build a CART model called trialCART, using all the independent variables in the 
# training set to train the model, and then plot the CART model. 
# Just use the default parameters to build the model (don't add a minbucket or cp value). 
# Remember to add the method="class" argument, since this is a classification problem.
# 
# What is the name of the first variable the model split on?
library(rpart)
library(rpart.plot)
trialCART = rpart(trial ~ ., data=train, method="class")
prp(trialCART) #Tphase
# sum(table(wikiTest$Vandal, predictCART) * diag(x=1,2,2)) / nrow(wikiTest) #0.5417025
# 
# ################################################################################
# Problem 3.5 - Building a Model
# Obtain the training set predictions for the model (do not yet predict on the test set). 
# Extract the predicted probability of a result being a trial (recall that this involves not 
# setting a type argument, and keeping only the second column of the predict output). 
# What is the maximum predicted probability for any result?
predTrain = predict(trialCART)
max(predTrain[,2]) #0.8718861
# 
# 
# ################################################################################
# Problem 3.6 - Building a Model
# Without running the analysis, how do you expect the maximum predicted probability 
# to differ in the testing set?
# 
# x The maximum predicted probability will likely be smaller in the testing set.  
# o The maximum predicted probability will likely be exactly the same in the testing set.  
# x The maximum predicted probability will likely be larger in the testing set.
# 
# 
# Because the CART tree assigns the same predicted probability to each leaf node and 
# there are a small number of leaf nodes compared to data points, we expect exactly 
# the same maximum predicted probability.
# ################################################################################
# Problem 3.7 - Building a Model
# For these questions, use a threshold probability of 0.5 to predict that an observation 
# is a clinical trial.
predTrain2 = predict(trialCART, type="class")
# What is the training set accuracy of the CART model?
sum(table(train$trial, predTrain2) * diag(x=1,2,2)) / nrow(train) #0.8233487
# 
# 
# What is the training set sensitivity of the CART model?
table(train$trial, predTrain2)
441 / (131+441) #0.770979
# 
# 
# What is the training set specificity of the CART model?
631 / (631+99) #0.8643836
# 
# 
# ################################################################################
# Problem 4.1 - Evaluating the model on the testing set
# Evaluate the CART model on the testing set using the predict function and creating a 
# vector of predicted probabilities predTest.
predTest = predict(trialCART, newdata=test)[,2]
# 
#
# What is the testing set accuracy, assuming a probability threshold of 0.5 for 
# predicting that a result is a clinical trial?
sum(table(test$trial, predTest>=0.5) * diag(x=1,2,2)) / nrow(test) #0.7580645
# 
# 
# Problem 4.2 - Evaluating the Model on the Testing Set
# Using the ROCR package, what is the testing set AUC of the prediction model?
library(ROCR)
ROCRpred = prediction(predTest, test$trial)
plot(performance(ROCRpred, "tpr", "fpr"), colorize=TRUE, 
     print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
performance(ROCRpred, "auc")@y.values #0.8371063
# 
# 
# ################################################################################
# PART 5: DECISION-MAKER TRADEOFFS
# 
# The decision maker for this problem, a researcher performing a review of the 
# medical literature, would use a model (like the CART one we built here) in the 
# following workflow:
# 
# 1) For all of the papers retreived in the PubMed Search, predict which papers are 
#   clinical trials using the model. 
#   This yields some initial Set A of papers predicted to be trials, and some Set B of 
#   papers predicted not to be trials. (See the figure below.)
# 
# 2) Then, the decision maker manually reviews all papers in Set A, verifying that each 
#   paper meets the study's detailed inclusion criteria (for the purposes of this analysis, 
#   we assume this manual review is 100% accurate at identifying whether a paper in Set 
#   A is relevant to the study). 
#   This yields a more limited set of papers to be included in the study, which would 
#   ideally be all papers in the medical literature meeting the detailed inclusion criteria 
#   for the study.
# 
# 3) Perform the study-specific analysis, using data extracted from the limited set of 
#   papers identified in step 2.
# 
# This process is shown in the figure below.
# 
# 
# ################################################################################
# Problem 5.1 - Decision-Maker Tradeoffs
# What is the cost associated with the model in Step 1 making a false negative 
# prediction?
# 
# x A paper will be mistakenly added to Set A, yielding additional work in Step 2 of the 
#     process but not affecting the quality of the results of Step 3.  
# x A paper will be mistakenly added to Set A, definitely affecting the quality of the 
#     results of Step 3. 
# o A paper that should have been included in Set A will be missed, affecting the quality 
#     of the results of Step 3.  
# x There is no cost associated with a false negative prediction.
# 
# 
# By definition, a false negative is a paper that should have been included in Set A 
# but was missed by the model. 
# This means a study that should have been included in Step 3 was missed, affecting the results.
# ################################################################################
# Problem 5.2 - Decision-Maker Tradeoffs
# What is the cost associated with the model in Step 1 making a false positive 
# prediction?
# 
# o A paper will be mistakenly added to Set A, yielding additional work in Step 2 of the 
#     process but not affecting the quality of the results of Step 3.  
# x A paper will be mistakenly added to Set A, definitely affecting the quality of the 
#     results of Step 3. 
# x A paper that should have been included in Set A will be missed, affecting the quality 
#     of the results of Step 3. 
# x There is no cost associated with a false positive prediction.
# 
#
# By definition, a false positive is a paper that should not have been included in Set A 
# but that was actually included. 
# However, because the manual review in Step 2 is assumed to be 100% effective, this extra paper
# will not make it into the more limited set of papers, and therefore this mistake will not 
# affect the analysis in Step 3.
# ################################################################################
# Problem 5.3 - Decision-Maker Tradeoffs
# Given the costs associated with false positives and false negatives, which of the 
# following is most accurate?
# 
# x A false positive is more costly than a false negative; the decision maker should use a 
#     probability threshold greater than 0.5 for the machine learning model.  
# x A false positive is more costly than a false negative; the decision maker should use a 
#     probability threshold less than 0.5 for the machine learning model.  
# x A false negative is more costly than a false positive; the decision maker should use 
#     a probability threshold greater than 0.5 for the machine learning model.  
# o A false negative is more costly than a false positive; the decision maker should use a 
#     probability threshold less than 0.5 for the machine learning model.
#  
# 
# A false negative might negatively affect the results of the literature review and analysis, 
# while a false positive is a nuisance (one additional paper that needs to be manually checked). 
# As a result, the cost of a false negative is much higher than the cost of a false positive, 
# so much so that many studies actually use no machine learning (aka no Step 1) and have 
# two people manually review each search result in Step 2. 
# As always, we prefer a lower threshold in cases where false negatives are more costly than 
# false positives, since we will make fewer negative predictions.
# ################################################################################