# Quick Question
# 
# Suppose the coefficients of a logistic regression model with two independent variables
# are as follows:
#  B0 = -1.5; B1 = 3; B2 = -0.5
   B = c(-1.5, 3, -0.5)
# 
# And we have an observation with the following values for the independent variables:
#  X1 = 1; X2 = 5
   X = c(1,5)
#   
#   
# What is the value of the Logit for this observation? Recall that the Logit is log(Odds).
   log_odd = B[1] + sum(B[2:3]*X); log_odd #-1
# 
# What is the value of the Odds for this observation? Note that you can compute e^x, 
# for some number x, in your R console by typing exp(x). 
# The function exp() computes the exponential of its argument.
   odd = exp(log_odd); odd #0.3678794
# 
# What is the value of P(y = 1) for this observation?
#   p / 1-p = odd
#   p = (1-p)*odd = odd-p*odd
#   p = odd / (1+odd)
  p_y1 = odd/(1+odd); p_y1 #0.2689414

# ##################################################################################    
# Quick Question
#    
# In R, create a logistic regression model to predict "PoorCare" using the independent 
# variables "StartedOnCombination" and "ProviderCount". 
# Use the training set we created in the previous video to build the model.
#    
# Note: If you haven't already loaded and split the data in R, please run these commands
# in your R console to load and split the data set. 
# Remember to first navigate to the directory where you have saved "quality.csv".    
   quality = read.csv("quality.csv")
   install.packages("caTools")
   library(caTools)
   set.seed(88)
   split = sample.split(quality$PoorCare, SplitRatio = 0.75)
   qualityTrain = subset(quality, split == TRUE)
   qualityTest = subset(quality, split == FALSE)
# 
# Then recall that we built a logistic regression model to predict PoorCare using 
# the R command:
#     QualityLog = glm(PoorCare ~ OfficeVisits + Narcotics, 
#                      data=qualityTrain, family=binomial)
    QualityLog = glm(PoorCare ~ StartedOnCombination + ProviderCount, 
                     data=qualityTrain, family=binomial)   
# You will need to adjust this command to answer this question, and then look at 
# the summary(QualityLog) output.
   summary(QualityLog)
   
# What is the coefficient for "StartedOnCombination"?
#    1.95230
   
# Quick Question
# 
# StartedOnCombination is a binary variable, which equals 1 if the patient is started
# on a combination of drugs to treat their diabetes, and equals 0 if the patient is 
# not started on a combination of drugs. 
# All else being equal, does this model imply that starting a patient on a 
# combination of drugs is indicative of poor care, or good care?
#    Poor care
   
# Quick Question
#    
#  IMPORTANT NOTE: This question uses the original model with the independent variables 
#  "OfficeVisits" and "Narcotics". Be sure to use this model, instead of the model you built in
#   Quick Question 4.
   QualityLog = glm(PoorCare ~ OfficeVisits + Narcotics, 
                    data=qualityTrain, family=binomial)  
   
# Compute the test set predictions in R by running the command:
   predictTest = predict(QualityLog, type="response", newdata=qualityTest)
#    
# You can compute the test set AUC by running the following two commands in R:
#  install.packages("ROCR")
   library(ROCR)
   ROCRpredTest = prediction(predictTest, qualityTest$PoorCare)
   auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
   plot(performance(ROCRpredTest, "tpr", "fpr"))
   plot(performance(ROCRpredTest, "tpr", "fpr"), colorize = TRUE)
   plot(performance(ROCRpredTest, "tpr", "fpr"), colorize = TRUE,
        print.cutoffs.at=seq(0,1,by=0.2), text.adj=c(0,1.7))
   auc
#    
# What is the AUC of this model on the test set?
   auc #0.7994792
   
# The AUC of a model has the following nice interpretation: 
#    given a random patient from the dataset who actually received poor care, 
#    and a random patient from the dataset who actually received good care, 
#    the AUC is the perecentage of time that our model will classify which is which correctly.