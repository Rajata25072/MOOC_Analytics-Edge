# Quick Question
# 
# In the previous video, we used CART and Random Forest to predict sentiment. 
# Let's see how well logistic regression does. 
# Build a logistic regression model (using the training set) to predict "Negative" using all 
# of the independent variables. 
# You may get a warning message after building your model - don't worry 
# (we explain what it means in the explanation).
# 
# Now, make predictions using the logistic regression model:
#   
#   predictions = predict(tweetLog, newdata=testSparse, type="response")
# 
# where "tweetLog" should be the name of your logistic regression model. 
# You might also get a warning message after this command, but don't worry - 
# it is due to the same problem as the previous warning message.
# 
# Build a confusion matrix (with a threshold of 0.5) and compute the accuracy of the model. 
# What is the accuracy?

# Read in the data
tweets = read.csv("tweets.csv", stringsAsFactors=FALSE)
str(tweets)


# Create dependent variable
tweets$Negative = as.factor(tweets$Avg <= -1)
table(tweets$Negative)


# Install new packages
# install.packages("tm")
library(tm)
# install.packages("SnowballC")
library(SnowballC)


# Create corpus
corpus = Corpus(VectorSource(tweets$Tweet))

# Look at corpus
corpus
corpus[[1]]


# Convert to lower-case
corpus = tm_map(corpus, tolower)
corpus[[1]]

# IMPORTANT NOTE: If you are using the latest version of the tm package, you will need to run 
# the following line before continuing (it converts corpus to a Plain Text Document). 
# This is a recent change having to do with the tolower function that occurred after this video 
# was recorded.

corpus = tm_map(corpus, PlainTextDocument)


# Remove punctuation
corpus = tm_map(corpus, removePunctuation)
corpus[[1]]


# Look at stop words 
stopwords("english")[1:10]


# Remove stopwords and apple
corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))
corpus[[1]]

# Stem document 
corpus = tm_map(corpus, stemDocument)
corpus[[1]]


# Create matrix
frequencies = DocumentTermMatrix(corpus)
frequencies


# Look at matrix 
inspect(frequencies[1000:1005,505:515])


# Check for sparsity
findFreqTerms(frequencies, lowfreq=20)
findFreqTerms(frequencies, lowfreq=100)


# Remove sparse terms
sparse = removeSparseTerms(frequencies, 0.995)
sparse


# Convert to a data frame
tweetsSparse = as.data.frame(as.matrix(sparse))


# Make all variable names R-friendly
colnames(tweetsSparse) = make.names(colnames(tweetsSparse))

# Add dependent variable
tweetsSparse$Negative = tweets$Negative

# Split the data
# library(caTools)
set.seed(123)

split = sample.split(tweetsSparse$Negative, SplitRatio = 0.7)
trainSparse = subset(tweetsSparse, split==TRUE)
testSparse = subset(tweetsSparse, split==FALSE)


# Build a CART model
# library(rpart)
# library(rpart.plot)
tweetCART = rpart(Negative ~ ., data=trainSparse, method="class")
prp(tweetCART)


# Evaluate the performance of the model
predictCART = predict(tweetCART, newdata=testSparse, type="class")
table(testSparse$Negative, predictCART)

# Compute accuracy
(294+18)/(294+6+37+18) #0.8788732

# Baseline accuracy 
table(testSparse$Negative)
300/(300+55) #0.8450704


# Random forest model
# library(randomForest)
set.seed(123)
tweetRF = randomForest(Negative ~ ., data=trainSparse)


# Make predictions:
predictRF = predict(tweetRF, newdata=testSparse)
table(testSparse$Negative, predictRF)

# Accuracy:
(293+21)/(293+7+34+21) #0.884507

# Logistic regression model
tweetLog = glm(Negative ~ ., data=trainSparse, family=binomial)
predictions = predict(tweetLog, newdata=testSparse, type="response")
table(testSparse$Negative,predictions>0.5)

(245+34)/ nrow(testSparse) #0.7859155

# The accuracy is (254+37)/(254+46+18+37) = 0.8197183, which is worse than the baseline. 
# If you were to compute the accuracy on the training set instead, you would see that the model 
# does really well on the training set - this is an example of over-fitting. 
# The model fits the training set really well, but does not perform well on the test set. 
# A logistic regression model with a large number of variables is particularly at risk for overfitting.
# 
# Note that you might have gotten a different answer than us, because the glm function struggles 
# with this many variables. The warning messages that you might have seen in this problem have to do 
# with the number of variables, and the fact that the model is overfitting to the training set. 
# We'll discuss this in more detail in the Homework Assignment.