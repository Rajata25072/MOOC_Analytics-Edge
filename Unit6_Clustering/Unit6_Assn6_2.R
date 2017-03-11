# MARKET SEGMENTATION FOR AIRLINES
# 
# Market segmentation is a strategy that divides a broad target market of customers 
# into smaller, more similar groups, and then designs a marketing strategy specifically for each group. 
# Clustering is a common technique for market segmentation since it automatically 
# finds similar groups given a data set. 
# 
# In this problem, we'll see how clustering can be used to find similar groups of 
# customers who belong to an airline's frequent flyer program. 
# The airline is trying to learn more about its customers so that it can target different 
# customer segments with different types of mileage offers. 
# 
# The file AirlinesCluster.csv contains information on 3,999 members of the frequent flyer program. 
# This data comes from the textbook "Data Mining for Business Intelligence," by 
# Galit Shmueli, Nitin R. Patel, and Peter C. Bruce. For more information, 
# see the website for the book.
# 
# There are seven different variables in the dataset, described below:
#   
#   Balance = number of miles eligible for award travel
#   QualMiles = number of miles qualifying for TopFlight status
#   BonusMiles = number of miles earned from non-flight bonus transactions in the past 
#     12 months
#   BonusTrans = number of non-flight bonus transactions in the past 12 months
#   FlightMiles = number of flight miles in the past 12 months
#   FlightTrans = number of flight transactions in the past 12 months
#   DaysSinceEnroll = number of days since enrolled in the frequent flyer program
# 
#
# ################################################################################
# Problem 1.1 - Normalizing the Data
# Read the dataset AirlinesCluster.csv into R and call it "airlines".
airlines = read.csv("AirlinesCluster.csv") 
# Looking at the summary of airlines, which TWO variables have (on average) 
# the smallest values?
summary(airlines)
sort(colMeans(airlines))
# 
# x Balance  
# x QualMiles  
# x BonusMiles
# o BonusTrans
# x FlightMiles
# o FlightTrans
# x DaysSinceEnroll
# 
# Which TWO variables have (on average) the largest values?
# 
# o Balance  
# x QualMiles  
# o BonusMiles
# x BonusTrans
# x FlightMiles
# x FlightTrans
# x DaysSinceEnroll
# 
# 
# ################################################################################
# Problem 1.2 - Normalizing the Data
# 
# In this problem, we will normalize our data before we run the clustering algorithms. 
# Why is it important to normalize the data before clustering?
# 
# x If we don't normalize the data, the clustering algorithms will not work 
#   (we will get an error in R).  
# x If we don't normalize the data, it will be hard to interpret the results of the 
#   clustering.  
# o If we don't normalize the data, the clustering will be dominated by the variables that 
#   are on a larger scale.  
# x If we don't normalize the data, the clustering will be dominated by the variables that are on 
#   a smaller scale.
# 
# 
# ################################################################################
# Problem 1.3 - Normalizing the Data
# Let's go ahead and normalize our data. 
# You can normalize the variables in a data frame by using the preProcess function in the "caret" package. 
# You should already have this package installed from Week 4, but if not, go ahead and 
# install it with install.packages("caret"). 
# Then load the package with library(caret).
# 
# Now, create a normalized data frame called "airlinesNorm" by running the following 
# commands:
# 
library(caret)
preproc = preProcess(airlines)
airlinesNorm = predict(preproc, airlines)
# 
# The first command pre-processes the data, and the second command performs the normalization. 
# If you look at the summary of airlinesNorm, you should see that all of the variables 
# now have mean zero. 
# You can also see that each of the variables has standard deviation 1 by using the sd() 
# function.
sd(airlinesNorm[,7])
# 
# In the normalized data, which variable has the largest maximum value?
summary(airlinesNorm)
# x Balance  
# x QualMiles  
# x BonusMiles
# x BonusTrans
# o FlightMiles
# x FlightTrans
# x DaysSinceEnroll

# 
# In the normalized data, which variable has the smallest minimum value?
# x Balance  
# x QualMiles  
# x BonusMiles
# x BonusTrans
# x FlightMiles
# x FlightTrans
# o DaysSinceEnroll
# 
#
# ################################################################################
# Problem 2.1 - Hierarchical Clustering
# Compute the distances between data points (using euclidean distance) and then run 
# the Hierarchical clustering algorithm (using method="ward.D") on the normalized 
# data. 
# It may take a few minutes for the commands to finish since the dataset has a large 
# number of observations for hierarchical clustering.
# 
# Then, plot the dendrogram of the hierarchical clustering process. 
# Suppose the airline is looking for somewhere between 2 and 10 clusters. 
# According to the dendrogram, which of the following is NOT a good choice for the 
# number of clusters?
# 
distances = dist(airlinesNorm, method="euclidean")
HclustIntense = hclust(distances, method="ward.D")
plot(HclustIntense)
rect.hclust(HclustIntense, k = 2, border = "red")
rect.hclust(HclustIntense, k = 3, border = "blue")
rect.hclust(HclustIntense, k = 6, border = "green")
rect.hclust(HclustIntense, k = 7, border = "purple")
# 6
# 
# 
# ################################################################################
# Problem 2.2 - Hierarchical Clustering
# Suppose that after looking at the dendrogram and discussing with the marketing 
# department, the airline decides to proceed with 5 clusters. 
# Divide the data points into 5 clusters by using the cutree function. 
# How many data points are in Cluster 1?
Hcluster = cutree(HclustIntense, k = 5)
table(Hcluster) #776
# 
# 
# ################################################################################
# Problem 2.3 - Hierarchical Clustering
# Now, use tapply to compare the average values in each of the variables for the 
# 5 clusters (the centroids of the clusters). 
# You may want to compute the average values of the unnormalized data so that it is 
# easier to interpret. 
# You can do this for the variable "Balance" with the following command:

# Compare each variable among groups
sort(tapply(airlines$Balance, Hcluster, mean))
sort(tapply(airlines$QualMiles, Hcluster, mean))
sort(tapply(airlines$BonusMiles, Hcluster, mean))
sort(tapply(airlines$BonusTrans, Hcluster, mean))
sort(tapply(airlines$FlightMiles, Hcluster, mean))
sort(tapply(airlines$FlightTrans, Hcluster, mean))
sort(tapply(airlines$DaysSinceEnroll, Hcluster, mean))

# Compare in its own group
lapply(split(airlinesNorm, Hcluster), colMeans)
# 
# Compared to the other clusters, Cluster 1 has the largest average values in which 
# variables (if any)? Select all that apply.
# 
# DaysSinceEnroll
# 
# How would you describe the customers in Cluster 1?
# 
# x  Relatively new customers who don't use the airline very often.  
# o  Infrequent but loyal customers.  
# x  Customers who have accumulated a large amount of miles, mostly through non-flight transactions.  
# x  Customers who have accumulated a large amount of miles, and the ones with the largest number 
#     of flight transactions.  
# x  Relatively new customers who seem to be accumulating miles, mostly through 
#     non-flight transactions.
# 
#
# ################################################################################
# Problem 2.4 - Hierarchical Clustering
# Compared to the other clusters, Cluster 2 has the largest average values in which variables 
# (if any)? Select all that apply.
lapply(split(airlinesNorm, Hcluster), colMeans)
# 
# o QualMiles  
# o FlightMiles  
# o FlightTrans 
# 
# How would you describe the customers in Cluster 2?
# 
# x Relatively new customers who don't use the airline very often.  
# x Infrequent but loyal customers.  
# x Customers who have accumulated a large amount of miles, mostly through non-flight transactions.  
# o Customers who have accumulated a large amount of miles, and the ones with the largest number 
#     of flight transactions.  
# x Relatively new customers who seem to be accumulating miles, mostly through non-flight transactions.
# 
#
# ################################################################################
# Problem 2.5 - Hierarchical Clustering
# Compared to the other clusters, Cluster 3 has the largest average values in which 
# variables (if any)? Select all that apply.
# 
lapply(split(airlinesNorm, Hcluster), colMeans)
# o Balance  
# QualMiles  
# o BonusMiles  
# o BonusTrans  
# x FlightMiles  FlightTrans  DaysSinceEnroll  None
# 
# How would you describe the customers in Cluster 3?
# 
#  x  Relatively new customers who don't use the airline very often.  
#  x  Infrequent but loyal customers.  
#  o Customers who have accumulated a large amount of miles, mostly through non-flight transactions.  
#  x  Customers who have accumulated a large amount of miles, and the ones with the largest number 
#     of flight transactions.  
#  x Relatively new customers who seem to be accumulating miles, mostly through 
#     non-flight transactions.
# 
#
# ################################################################################
# Problem 2.6 - Hierarchical Clustering
# Compared to the other clusters, Cluster 4 has the largest average values in which 
# variables (if any)? Select all that apply.
lapply(split(airlinesNorm, Hcluster), colMeans)
# 
# None
# 
# How would you describe the customers in Cluster 4?
#   x Relatively new customers who don't use the airline very often.  
#   x Infrequent but loyal customers.  
#   x Customers who have accumulated a large amount of miles, mostly through non-flight transactions.  
#   x Customers who have accumulated a large amount of miles, and the ones with the largest number 
#       of flight transactions.  
#   o Relatively new customers who seem to be accumulating miles, mostly through 
#       non-flight transactions.
# 
#
# ################################################################################
# Problem 2.7 - Hierarchical Clustering
# Compared to the other clusters, Cluster 5 has the largest average values in which 
# variables (if any)? Select all that apply.
# 
lapply(split(airlinesNorm, Hcluster), colMeans)
# None
# 
# How would you describe the customers in Cluster 5?
# 
#   o Relatively new customers who don't use the airline very often.  
#   x Infrequent but loyal customers.  
#   x Customers who have accumulated a large amount of miles, mostly through non-flight transactions.  
#   x Customers who have accumulated a large amount of miles, and the ones with the largest number 
#       of flight transactions.  
#   x Relatively new customers who seem to be accumulating miles, mostly through 
#       non-flight transactions.# 
# 
# ################################################################################
# Problem 3.1 - K-Means Clustering
# Now run the k-means clustering algorithm on the normalized data, again creating 
# 5 clusters. 
# Set the seed to 88 right before running the clustering algorithm, and set the 
# argument iter.max to 1000.
# 
set.seed(88)
Kcluster = kmeans(airlinesNorm, centers = 5, iter.max=1000)
str(Kcluster)
# How many clusters have more than 1,000 observations?
table(Kcluster$cluster) #2
# 
# 
# ################################################################################
# Problem 3.2 - K-Means Clustering
# Now, compare the cluster centroids to each other either by dividing the data points 
# into groups and then using tapply, or by looking at the output of 
# kmeansClust$centers, where "kmeansClust" is the name of the output of the kmeans 
# function. 
# (Note that the output of kmeansClust$centers will be for the normalized data. 
#  If you want to look at the average values for the unnormalized data, you need to use tapply 
#  like we did for hierarchical clustering.)
# 
# Do you expect Cluster 1 of the K-Means clustering output to necessarily be similar to 
# Cluster 1 of the Hierarchical clustering output?
table(Hcluster, Kcluster$cluster)
# 
#   x Yes, because the clusters are displayed in order of size, so the largest cluster 
#       will always be first.  
#   x Yes, because the clusters are displayed according to the properties of the centroid, 
#       so the cluster order will be similar.  
#   o No, because cluster ordering is not meaningful in either k-means clustering or hierarchical clustering. 
#   x No, because the clusters produced by the k-means algorithm will never be similar 
#       to the clusters produced by the Hierarchical algorithm.
# 
# 
# ################################################################################