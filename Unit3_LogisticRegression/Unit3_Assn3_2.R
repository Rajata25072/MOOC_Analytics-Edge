# PREDICTING PAROLE VIOLATORS
# 
# In many criminal justice systems around the world, inmates deemed not to be a threat to society 
# are released from prison under the parole system prior to completing their sentence. 
# They are still considered to be serving their sentence while on parole, and they can be returned 
# to prison if they violate the terms of their parole.
# 
# Parole boards are charged with identifying which inmates are good candidates for release on parole.
# They seek to release inmates who will not commit additional crimes after release. 
# In this problem, we will build and validate a model that predicts if an inmate will violate 
# the terms of his or her parole. Such a model could be useful to a parole board when deciding 
# to approve or deny an application for parole.
# 
# For this prediction task, we will use data from the United States 2004 National Corrections 
# Reporting Program, a nationwide census of parole releases that occurred during 2004. 
# We limited our focus to parolees who served no more than 6 months in prison and whose 
# maximum sentence for all charges did not exceed 18 months. 
# The dataset contains all such parolees who either successfully completed their term of parole
# during 2004 or those who violated the terms of their parole during that year. 
# The dataset contains the following variables:
#   
#   male: 1 if the parolee is male, 0 if female
#   race: 1 if the parolee is white, 2 otherwise
#   age: the parolee's age (in years) when he or she was released from prison
#   state: a code for the parolee's state. 2 is Kentucky, 3 is Louisiana, 4 is Virginia, 
#     and 1 is any other state. 
#     The three states were selected due to having a high representation in the dataset.
#   time.served: the number of months the parolee served in prison 
#     (limited by the inclusion criteria to not exceed 6 months).
#   max.sentence: the maximum sentence length for all charges, in months 
#     (limited by the inclusion criteria to not exceed 18 months).
#   multiple.offenses: 1 if the parolee was incarcerated for multiple offenses, 0 otherwise.
#   crime: a code for the parolee's main crime leading to incarceration. 2 is larceny, 
#     3 is drug-related crime, 4 is driving-related crime, and 1 is any other crime.
#   violator: 1 if the parolee violated the parole, and 0 if the parolee completed the parole 
#     without violation.
# 
# 
# ##################################################################################    
# Problem 1.1 - Loading the Dataset
# Load the dataset parole.csv into a data frame called parole, and investigate it using the str() 
# and summary() functions.
parole = read.csv("parole.csv") 
# How many parolees are contained in the dataset?
# 675
# 
# ##################################################################################    
# Problem 1.2 - Loading the Dataset
# How many of the parolees in the dataset violated the terms of their parole?
table(parole$violator) #78
# 
# ##################################################################################    
# Problem 2.1 - Preparing the Dataset
# You should be familiar with unordered factors (if not, review the Week 2 homework problem 
# "Reading Test Scores"). 
# Which variables in this dataset are unordered factors with at least three levels? 
# Select all that apply.
summary(parole) 
# xmale  xrace  xage  ostate  xtime.served  xmax.sentence  xmultiple.offenses  ocrime  xviolator
# 
# 
# ##################################################################################    
# Problem 2.2 - Preparing the Dataset
# In the last subproblem, we identified variables that are unordered factors with at least 3 levels,
# so we need to convert them to factors for our prediction problem 
# (we introduced this idea in the "Reading Test Scores" problem last week). 
# Using the as.factor() function, convert these variables to factors. 
# Keep in mind that we are not changing the values, just the way R understands them 
# (the values are still numbers).
parole$state = as.factor(parole$state)
parole$crime = as.factor(parole$crime)

# How does the output of summary() change for a factor variable as compared to a numerical variable?
summary(parole)
# 
# The output becomes similar to that of the table() function applied to that variable  
# The output becomes similar to that of the str() function applied to that variable  
# There is no change
# 
# 
# ##################################################################################    
# Problem 3.1 - Splitting into a Training and Testing Set
# To ensure consistent training/testing set splits, run the following 5 lines of code 
# (do not include the line numbers at the beginning):
  set.seed(144)
  library(caTools)
  split = sample.split(parole$violator, SplitRatio = 0.7)
  train = subset(parole, split == TRUE)
  test = subset(parole, split == FALSE)
# 
# Roughly what proportion of parolees have been allocated to the training and testing sets?
# 
#   70% to the training set, 30% to the testing set  
# 
# 
# ##################################################################################    
# Problem 3.2 - Splitting into a Training and Testing Set
# Now, suppose you re-ran lines [1]-[5] of Problem 3.1. What would you expect?  
#   oThe exact same training/testing set split as the first execution of [1]-[5]  
#   A different training/testing set split from the first execution of [1]-[5]
# 
# 
# If you instead ONLY re-ran lines [3]-[5], what would you expect?
#   The exact same training/testing set split as the first execution of [1]-[5]  
#   oA different training/testing set split from the first execution of [1]-[5]
# 
# 
# If you instead called set.seed() with a different number and then re-ran lines [3]-[5] of 
# Problem 3.1, what would you expect?
#   The exact same training/testing set split as the first execution of [1]-[5]  
#   oA different training/testing set split from the first execution of [1]-[5]
# 
# 
# ##################################################################################    
# Problem 4.1 - Building a Logistic Regression Model
# If you tested other training/testing set splits in the previous section, 
# please re-run the original 5 lines of code to obtain the original split.
# 
# Using glm (and remembering the parameter family="binomial"), train a logistic regression model on 
# the training set. 
# Your dependent variable is "violator", and you should use all of the other variables 
# as independent variables.
violate1 = glm(violator ~ ., data=train, family="binomial")
# 
# What variables are significant in this model? 
# Significant variables should have a least one star, or should have a probability less than 0.05 
# (the column Pr(>|z|) in the summary output). Select all that apply.
summary(violate1) 
#   race  state4  multiple.offenses  
# 
# 
# ##################################################################################    
# Problem 4.2 - Building a Logistic Regression Model
# What can we say based on the coefficient of the multiple.offenses variable?
# 
# The following two properties might be useful to you when answering this question:
# 1) If we have a coefficient c for a variable, then that means the log odds (or Logit) are 
#   increased by c for a unit increase in the variable.
# 2) If we have a coefficient c for a variable, then that means the odds are multiplied by e^c for 
#   a unit increase in the variable.
# 
exp(1.6119919) #5.01
#   xOur model predicts that parolees who committed multiple offenses have 1.61 times higher odds 
#     of being a violator than the average parolee.  
#   xOur model predicts that a parolee who committed multiple offenses has 1.61 times higher odds 
#     of being a violator than a parolee who did not commit multiple offenses but is otherwise 
#     identical.  
#   xur model predicts that parolees who committed multiple offenses have 5.01 times higher odds 
#     of being a violator than the average parolee.  
#   oOur model predicts that a parolee who committed multiple offenses has 5.01 times higher odds 
#     of being a violator than a parolee who did not commit multiple offenses but is otherwise i
#     dentical.
# 
# 
# ##################################################################################    
# Problem 4.3 - Building a Logistic Regression Model
# Consider a parolee who is male, of white race, aged 50 years at prison release, from the state of
# Maryland, served 3 months, had a maximum sentence of 12 months, did not commit multiple offenses, 
# and committed a larceny. 
# Answer the following questions based on the model's predictions for this individual. 
# (HINT: You should use the coefficients of your model, the Logistic Response Function, 
#  and the Odds equation to solve this problem.)
# 
# According to the model, what are the odds this individual is a violator?
log_odd = -4.2411574 + 0.3869904*1 + 0.8867192*1 - 0.0001756*50 -0.1238867*3 + 0.0802954*12 + 0.6837143*1
odd = exp(log_odd); odd  #0.1825687
# 
# According to the model, what is the probability this individual is a violator?
odd / (1+odd) #0.1543832
1 / (1+exp(-log_odd)) #0.1543832
# 
# ##################################################################################    
# Problem 5.1 - Evaluating the Model on the Testing Set
# Use the predict() function to obtain the model's predicted probabilities for parolees in 
# the testing set, remembering to pass type="response".
predicttest = predict(violate1, newdata=test, type="response") 
# What is the maximum predicted probability of a violation?
max(predicttest)
# 
# ##################################################################################    
# Problem 5.2 - Evaluating the Model on the Testing Set
# In the following questions, evaluate the model's predictions on the test set using 
# a threshold of 0.5.
table(test$violator, predicttest>0.5) 
# What is the model's sensitivity?
12 / (12+11) #0.5217391
# 
# What is the model's specificity?
167 / (167+12) #0.9329609
# 
# What is the model's accuracy?
(167+12)  / nrow(test) #0.8861386
# 
# ##################################################################################    
# Problem 5.3 - Evaluating the Model on the Testing Set
# What is the accuracy of a simple model that predicts that every parolee is a non-violator?
table(test$violator) 
sum(test$violator==0)/nrow(test) #0.8861386
# 
# ##################################################################################    
# Problem 5.4 - Evaluating the Model on the Testing Set
# Consider a parole board using the model to predict whether parolees will be violators or not. 
# The job of a parole board is to make sure that a prisoner is ready to be released into 
# free society, and therefore parole boards tend to be particularily concerned about releasing 
# prisoners who will violate their parole. 
# Which of the following most likely describes their preferences and best course of action?
# 
#   xThe board assigns more cost to a false negative than a false positive, and should therefore use 
#     a logistic regression cutoff higher than 0.5.  
#   oThe board assigns more cost to a false negative than a false positive, and should therefore use
#     a logistic regression cutoff less than 0.5.  
#   xThe board assigns equal cost to a false positive and a false negative, and should therefore use 
#     a logistic regression cutoff equal to 0.5.  
#   xThe board assigns more cost to a false positive than a false negative, and should therefore use 
#     a logistic regression cutoff higher than 0.5.  
#   xThe board assigns more cost to a false positive than a false negative, and should therefore use 
#     a logistic regression cutoff less than 0.5.
# 
# 
# Problem 5.5 - Evaluating the Model on the Testing Set
# Which of the following is the most accurate assessment of the value of the logistic regression 
# model with a cutoff 0.5 to a parole board, based on the model's accuracy as compared to 
# the simple baseline model?

cf_table = table(test$violator, predicttest>0.6)
(cf_table[1,1]+cf_table[2,2]) / 202

# xThe model is of limited value to the board because it cannot outperform a simple baseline, 
#   and using a different logistic regression cutoff is unlikely to improve the model's value.  
# xThe model is of limited value to the board because it cannot outperform a simple baseline, 
#   and using a different logistic regression cutoff is likely to improve the model's value.  
# xThe model is likely of value to the board, and using a different logistic regression cutoff 
#   is unlikely to improve the model's value.  
# oThe model is likely of value to the board, and using a different logistic regression cutoff 
#   is likely to improve the model's value.
# 
# The model at cutoff 0.5 has 12 false positives and 11 false negatives, while the baseline model 
# has 0 false positives and 23 false negatives. 
# Because a parole board is likely to assign more cost to a false negative, the model at cutoff 0.5 
# is likely of value to the board.
# 
# From the previous question, the parole board would likely benefit from decreasing the 
# logistic regression cutoffs, which decreases the false negative rate while increasing 
# the false positive rate.
# 
# 
# ##################################################################################    
# Problem 5.6 - Evaluating the Model on the Testing Set
# Using the ROCR package, what is the AUC value for the model?
library(ROCR)
ROCRpred = prediction(predicttest, test$violator)
plot(performance(ROCRpred, "tpr", "fpr"), colorize=TRUE, 
     print.cutoffs.at=seq(0,1,by=0.2), text.adj=c(-0.2,1.7))
performance(ROCRpred, "auc")@y.values #0.8945834
# 
# ##################################################################################    
# Problem 5.7 - Evaluating the Model on the Testing Set
# Describe the meaning of AUC in this context.
# 
# The probability the model can correctly differentiate between a randomly selected parole violator 
#   and a randomly selected parole non-violator.  
# 
# 
# ##################################################################################    
# Problem 6.1 - Identifying Bias in Observational Data
# Our goal has been to predict the outcome of a parole decision, and we used a publicly available 
# dataset of parole releases for predictions. 
# In this final problem, we'll evaluate a potential source of bias associated with our analysis. 
# It is always important to evaluate a dataset for possible sources of bias.
# 
# The dataset contains all individuals released from parole in 2004, either due to completing 
# their parole term or violating the terms of their parole. 
# However, it does not contain parolees who neither violated their parole nor completed their 
# term in 2004, causing non-violators to be underrepresented. 
# This is called "selection bias" or "selecting on the dependent variable," because only a subset 
# of all relevant parolees were included in our analysis, based on our dependent variable in 
# this analysis (parole violation). How could we improve our dataset to best address selection bias?
# 
# xThere is no way to address this form of biasing.  
# xWe should use the current dataset, expanded to include the missing parolees. 
#   Each added parolee should be labeled with violator=0, because they have not yet had a violation.  
# xWe should use the current dataset, expanded to include the missing parolees. 
#   Each added parolee should be labeled with violator=NA, because the true outcome has not been 
#   observed for these individuals.  
# oWe should use a dataset tracking a group of parolees from the start of their parole until 
#   either they violated parole or they completed their term.