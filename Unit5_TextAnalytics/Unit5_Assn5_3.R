# SEPARATING SPAM FROM HAM (PART 1)
# 
# Nearly every email user has at some point encountered a "spam" email, which is an 
# unsolicited message often advertising a product, containing links to malware, or 
# attempting to scam the recipient. 
# Roughly 80-90% of more than 100 billion emails sent each day are spam emails, 
# most being sent from botnets of malware-infected computers. 
# The remainder of emails are called "ham" emails.
# 
# As a result of the huge number of spam emails being sent across the Internet each 
# day, most email providers offer a spam filter that automatically flags likely spam 
# messages and separates them from the ham. 
# Though these filters use a number of techniques 
# (e.g. looking up the sender in a so-called "Blackhole List" that contains IP addresses 
# of likely spammers), most rely heavily on the analysis of the contents of an email via 
# text analytics.
# 
# In this homework problem, we will build and evaluate a spam filter using a publicly 
# available dataset first described in the 2006 conference paper "Spam Filtering with 
# Naive Bayes -- Which Naive Bayes?" by V. Metsis, I. Androutsopoulos, and 
# G. Paliouras. 
# The "ham" messages in this dataset come from the inbox of former Enron Managing 
# Director for Research Vincent Kaminski, one of the inboxes in the Enron Corpus. 
# One source of spam messages in this dataset is the SpamAssassin corpus, 
# which contains hand-labeled spam messages contributed by Internet users. 
# The remaining spam was collected by Project Honey Pot, a project that collects spam 
# messages and identifies spammers by publishing email address that humans would 
# know not to contact but that bots might target with spam. 
# The full dataset we will use was constructed as roughly a 75/25 mix of the ham and 
# spam messages.
# 
# The dataset contains just two fields:
#   text: The text of the email.
#   spam: A binary variable indicating if the email was spam.
# 
# IMPORTANT NOTE: This problem (Separating Spam from Ham) continues on the next page with 
# additional exercises. 
# The second page is optional, but if you want to try it out, remember to save your work 
# so you can start the next page where you left off here.
# 
# 
# ################################################################################
# Problem 1.1 - Loading the Dataset
# Begin by loading the dataset emails.csv into a data frame called emails. 
# Remember to pass the stringsAsFactors=FALSE option when loading the data.
emails = read.csv("emails.csv", stringsAsFactors=FALSE)
# How many emails are in the dataset?
nrow(emails) #5728
# 
# 
# ################################################################################
# Problem 1.2 - Loading the Dataset
# How many of the emails are spam?
sum(emails$spam) #1368
# 
# 
# ################################################################################
# Problem 1.3 - Loading the Dataset
# Which word appears at the beginning of every email in the dataset? 
# Respond as a lower-case word with punctuation removed.
head(emails) #subject
# 
# 
# ################################################################################
# Problem 1.4 - Loading the Dataset
# Could a spam classifier potentially benefit from including the frequency of the word 
# that appears in every email?
# 
# x  No -- the word appears in every email so this variable would not help us differentiate 
#     spam from ham.  
# o  Yes -- the number of times the word appears might help us differentiate spam from 
#     ham.
# 
# 
# We know that each email has the word "subject" appear at least once, but the frequency with 
# which it appears might help us differentiate spam from ham. For instance, a long email chain 
# would have the word "subject" appear a number of times, and this higher frequency might be 
# indicative of a ham message.
# ################################################################################
# Problem 1.5 - Loading the Dataset
# The nchar() function counts the number of characters in a piece of text. 
# How many characters are in the longest email in the dataset 
# (where longest is measured in terms of the maximum number of characters)?
max(nchar(emails$text)) #43952
# 
# 
# ################################################################################
# Problem 1.6 - Loading the Dataset
# Which row contains the shortest email in the dataset? 
# (Just like in the previous problem, shortest is measured in terms of the fewest 
# number of characters.)
which.min(nchar(emails$text)) #1992
# 
# 
# ################################################################################
# Problem 2.1 - Preparing the Corpus
# Follow the standard steps to build and pre-process the corpus:
#   1) Build a new corpus variable called corpus.
library(tm)
library(SnowballC)
corpus = Corpus(VectorSource(emails$text))
#   2) Using tm_map, convert the text to lowercase.
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
#   3) Using tm_map, remove all punctuation from the corpus.
corpus = tm_map(corpus, removePunctuation)
#   4) Using tm_map, remove all English stopwords from the corpus.
corpus = tm_map(corpus, removeWords, stopwords("english"))
#   5) Using tm_map, stem the words in the corpus.
corpus = tm_map(corpus, stemDocument)
#   6) Build a document term matrix from the corpus, called dtm.
dtm = DocumentTermMatrix(corpus)
# 
# If the code length(stopwords("english")) does not return 174 for you, then please run 
# the line of code in this file, which will store the standard stop words in a variable called sw. 
# When removing stop words, use tm_map(corpus, removeWords, sw) instead of 
# tm_map(corpus, removeWords, stopwords("english")).
# 
# How many terms are in dtm?
dtm #28687
# 
# 
# ################################################################################
# Problem 2.2 - Preparing the Corpus
# To obtain a more reasonable number of terms, limit dtm to contain terms appearing 
# in at least 5% of documents, and store this result as spdtm 
# (don't overwrite dtm, because we will use it in a later step of this homework). 
# How many terms are in spdtm?
spdtm = removeSparseTerms(dtm, 0.95)
spdtm #330
# 
# 
# ################################################################################
# Problem 2.3 - Preparing the Corpus
# Build a data frame called emailsSparse from spdtm, and use the make.names 
# function to make the variable names of emailsSparse valid.
# 
emailsSparse = as.data.frame(as.matrix(spdtm))
colnames(emailsSparse) = make.names(colnames(emailsSparse))
# colSums() is an R function that returns the sum of values for each variable in our data frame. 
# Our data frame contains the number of times each word stem (columns) appeared in each email (rows). 
# Therefore, colSums(emailsSparse) returns the number of times a word stem 
# appeared across all the emails in the dataset. 
# 
# What is the word stem that shows up most frequently across all the emails in the 
# dataset? 
# Hint: think about how you can use sort() or which.max() to pick out the maximum frequency.
head(sort(colSums(emailsSparse),decreasing=TRUE)) #enron
# 
# 
# ################################################################################
# Problem 2.4 - Preparing the Corpus
# 
# Add a variable called "spam" to emailsSparse containing the email spam labels. 
# You can do this by copying over the "spam" variable from the original data frame 
# (remember how we did this in the Twitter lecture).
# 
# How many word stems appear at least 5000 times in the ham emails in the dataset? 
# Hint: in this and the next question, remember not to count the dependent variable we just added.
emailsSparse$spam = emails$spam
#Method 1
hamSparse = subset(emailsSparse, spam==0)
table(colSums(hamSparse[,1:330])>5000) #6
#Method 2
head(sort(colSums(subset(emailsSparse, spam == 0)[,1:330]), decreasing = TRUE),10) #6
# 
# 
# ################################################################################
# Problem 2.5 - Preparing the Corpus
# How many word stems appear at least 1000 times in the spam emails in the dataset?
head(sort(colSums(subset(emailsSparse, spam == 1)[,1:330]), decreasing = TRUE),10) #3
# 
# 
# ################################################################################
# Problem 2.6 - Preparing the Corpus
# The lists of most common words are significantly different between the spam and ham emails. 
# What does this likely imply?
# 
#   x The frequencies of these most common words are unlikely to help differentiate 
#       between spam and ham.  
#   o The frequencies of these most common words are likely to help differentiate 
#       between spam and ham.
# 
# 
# ################################################################################
# Problem 2.7 - Preparing the Corpus
# Several of the most common word stems from the ham documents, such as "enron", 
# "hou" (short for Houston), "vinc" (the word stem of "Vince") and "kaminski", 
# are likely specific to Vincent Kaminski's inbox. 
# What does this mean about the applicability of the text analytics models we will train 
# for the spam filtering problem?
# 
#   x The models we build are still very general, and are likely to perform well as a spam 
#       filter for nearly any other person.  
#   o The models we build are personalized, and would need to be further tested before 
#       being used as a spam filter for another person.
# 
# 
# ################################################################################
# Problem 3.1 - Building machine learning models
# First, convert the dependent variable to a factor with 
emailsSparse$spam = as.factor(emailsSparse$spam)
# 
# Next, set the random seed to 123 and use the sample.split function to split 
# emailsSparse 70/30 into a training set called "train" and a testing set called "test". 
# Make sure to perform this step on emailsSparse instead of emails.
library(caTools)
set.seed(123) 
spl = sample.split(emailsSparse$spam, SplitRatio = 0.7)
train = subset(emailsSparse, spl==TRUE)
test = subset(emailsSparse, spl==FALSE)
# 
# Using the training set, train the following three machine learning models. 
# The models should predict the dependent variable "spam", using all other available 
# variables as independent variables. 
# Please be patient, as these models may take a few minutes to train.
#   1) A logistic regression model called spamLog. You may see a warning message here - 
#     we'll discuss this more later.
spamLog = glm(spam ~ ., data=train, family=binomial)
#   2) A CART model called spamCART, using the default parameters to train the model 
#     (don't worry about adding minbucket or cp). Remember to add the argument 
#     method="class" since this is a binary classification problem.
library(rpart)
library(rpart.plot)
spamCART = rpart(spam ~ ., data=train, method="class")
#   3) A random forest model called spamRF, using the default parameters to train the 
#     model (don't worry about specifying ntree or nodesize). Directly before training the 
#     random forest model, set the random seed to 123 (even though we've already done 
#     this earlier in the problem, it's important to set the seed right before training the 
#     model so we all obtain the same results. Keep in mind though that on certain 
#     operating systems, your results might still be slightly different).
library(randomForest)
set.seed(123)
spamRF = randomForest(spam ~ ., data=train)
# 
# For each model, obtain the predicted spam probabilities for the training set. 
# Be careful to obtain probabilities instead of predicted classes, because we will be 
# using these values to compute training set AUC values. 
# Recall that you can obtain probabilities for CART models by not passing any type 
# parameter to the predict() function, and you can obtain probabilities from a random 
# forest by adding the argument type="prob". 
# For CART and random forest, you need to select the second column of the output of 
# the predict() function, corresponding to the probability of a message being spam.
# 
# You may have noticed that training the logistic regression model yielded the 
# messages "algorithm did not converge" and "fitted probabilities numerically 0 or 1 occurred". 
# Both of these messages often indicate overfitting and the first indicates particularly 
# severe overfitting, often to the point that the training set observations are fit 
# perfectly by the model. 
# Let's investigate the predicted probabilities from the logistic regression model.
# 
# How many of the training set predicted probabilities from spamLog are less than 0.00001?
sum(predict(spamLog) < 0.00001) #3056
# 
# 
# How many of the training set predicted probabilities from spamLog are more than 0.99999?
sum(predict(spamLog) > 0.99999) #954
hist(predict(spamLog))
# 
# 
# How many of the training set predicted probabilities from spamLog are between 0.00001 and 0.99999?
sum(predict(spamLog) >= 0.00001 & predict(spamLog)<= 0.99999) #0
# 
# 
# ################################################################################
# Problem 3.2 - Building Machine Learning Models
# How many variables are labeled as significant (at the p=0.05 level) in the logistic 
# regression summary output?
summary(spamLog) #0
# 
# 
# ################################################################################
# Problem 3.3 - Building Machine Learning Models
# How many of the word stems "enron", "hou", "vinc", and "kaminski" appear in the 
# CART tree? 
# Recall that we suspect these word stems are specific to Vincent Kaminski and might 
# affect the generalizability of a spam filter built with his ham data.
prp(spamCART) #2 - "vinc" & "enron"
# 
# 
# ################################################################################
# Problem 3.4 - Building Machine Learning Models
# What is the training set accuracy of spamLog, using a threshold of 0.5 for 
# predictions?
sum(table(train$spam,predict(spamLog)>0.5) * diag(x=1,2,2)) / nrow(train) #0.9990025
# 
# 
# ################################################################################
# Problem 3.5 - Building Machine Learning Models
# What is the training set AUC of spamLog?
library(ROCR)
ROCRpred = prediction(predict(spamLog), train$spam)
plot(performance(ROCRpred, "tpr", "fpr"), colorize=TRUE, 
     print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
performance(ROCRpred, "auc")@y.values #0.9999959
# 
# 
# ################################################################################
# Problem 3.6 - Building Machine Learning Models
# What is the training set accuracy of spamCART, using a threshold of 0.5 for 
# predictions? 
# (Remember that if you used the type="class" argument when making predictions, 
# you automatically used a threshold of 0.5. If you did not add in the type argument to 
# the predict function, the probabilities are in the second column of the predict 
# output.)
sum(table(train$spam,predict(spamCART, type="class")) * diag(x=1,2,2)) / nrow(train) #0.942394
# 
# 
# ################################################################################
# Problem 3.7 - Building Machine Learning Models
# What is the training set AUC of spamCART? 
# (Remember that you have to pass the prediction function predicted probabilities, 
# so don't include the type argument when making predictions for your CART model.)
ROCRpred = prediction(predict(spamCART)[,2], train$spam)
plot(performance(ROCRpred, "tpr", "fpr"), colorize=TRUE, 
     print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
performance(ROCRpred, "auc")@y.values #0.9696044
# 
# 
# ################################################################################
# Problem 3.8 - Building Machine Learning Models
# What is the training set accuracy of spamRF, using a threshold of 0.5 for predictions? 
# (Remember that your answer might not match ours exactly, due to random behavior 
# in the random forest algorithm on different operating systems.)
sum(table(train$spam,predict(spamRF, type="class")) * diag(x=1,2,2)) / nrow(train) #0.9802993
# 
# 
# ################################################################################
# Problem 3.9 - Building Machine Learning Models
# What is the training set AUC of spamRF? 
# (Remember to pass the argument type="prob" to the predict function to get 
# predicted probabilities for a random forest model. The probabilities will be the 
# second column of the output.)
ROCRpred = prediction(predict(spamRF, type="prob")[,2], train$spam)
plot(performance(ROCRpred, "tpr", "fpr"), colorize=TRUE, 
     print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
performance(ROCRpred, "auc")@y.values #0.9979831
# 
# 
# ################################################################################
# Problem 3.10 - Building Machine Learning Models
# Which model had the best training set performance, in terms of accuracy and AUC?
# 
#   o Logistic regression   - 0.9990025 | 0.9999959
#   x CART                  - 0.942394 | 0.9696044
#   x Random forest         - 0.9802993 | 0.9979831
# 
# 
# ################################################################################ 
# Problem 4.1 - Evaluating on the Test Set
# Obtain predicted probabilities for the testing set for each of the models, 
# again ensuring that probabilities instead of classes are obtained.
# 
# What is the testing set accuracy of spamLog, using a threshold of 0.5 for predictions?
sum(table(test$spam,predict(spamLog, newdata=test)>0.5) * diag(x=1,2,2)) / nrow(test) #0.9511059
# 
# 
# ################################################################################
# Problem 4.2 - Evaluating on the Test Set
# What is the testing set AUC of spamLog?
ROCRpred = prediction(predict(spamLog, newdata=test), test$spam)
plot(performance(ROCRpred, "tpr", "fpr"), colorize=TRUE, 
     print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
performance(ROCRpred, "auc")@y.values #0.9767994
# 
# 
# ################################################################################
# Problem 4.3 - Evaluating on the Test Set
# What is the testing set accuracy of spamCART, using a threshold of 0.5 for 
# predictions?
sum(table(test$spam,predict(spamCART, newdata=test, type="class")) * diag(x=1,2,2)) / nrow(test) 
# 0.9394645
# 
# 
# ################################################################################
# Problem 4.4 - Evaluating on the Test Set
# What is the testing set AUC of spamCART?
ROCRpred = prediction(predict(spamCART, newdata=test)[,2], test$spam)
plot(performance(ROCRpred, "tpr", "fpr"), colorize=TRUE, 
     print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
performance(ROCRpred, "auc")@y.values #0.963176
# 
# 
# ################################################################################
# Problem 4.5 - Evaluating on the Test Set
# What is the testing set accuracy of spamRF, using a threshold of 0.5 for predictions?
sum(table(test$spam,predict(spamRF, newdata=test, type="class")) * diag(x=1,2,2)) / nrow(test) #0.975553
# 
# 
# ################################################################################
# Problem 4.6 - Evaluating on the Test Set
# What is the testing set AUC of spamRF?
ROCRpred = prediction(predict(spamRF, newdata=test, type="prob")[,2], test$spam)
plot(performance(ROCRpred, "tpr", "fpr"), colorize=TRUE, 
     print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
performance(ROCRpred, "auc")@y.values #0.9975628
# 
# 
# ################################################################################
# Problem 4.7 - Evaluating on the Test Set
# Which model had the best testing set performance, in terms of accuracy and AUC?
# 
#   x Logistic regression 0.9511059| 0.9767994
#   x CART                0.9394645| 0.963176
#   o Random forest       0.975553 | 0.9975628
# 
# 
# ################################################################################
# Problem 4.8 - Evaluating on the Test Set
# Which model demonstrated the greatest degree of overfitting?
# 
#   o Logistic regression   - 0.9990025 | 0.9999959 -> 0.9511059| 0.9767994
#   x CART                  - 0.942394 | 0.9696044 -> 0.9394645| 0.963176
#   x Random forest         - 0.9802993 | 0.9979831 -> 0.975553 | 0.9975628
# 
# ################################################################################