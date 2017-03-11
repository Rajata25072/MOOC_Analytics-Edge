# Quick Question
# 
# The movie "The Godfather" is in the genres action, crime, and drama, and is defined by the vector: 
#   (0,1,0,0,0,0,1,0,1,0,0,0,0,0,0,0,0,0,0)
# 
# The movie "Titanic" is in the genres action, drama, and romance, and is defined by the vector: 
#   (0,1,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0)
# 
# What is the distance between "The Godfather" and "Titanic", using euclidean distance?

dist(rbind(c(0,1,0,0,0,0,1,0,1,0,0,0,0,0,0,0,0,0,0),
     c(0,1,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0)), method = "euclidean") #1.414214

dist(movies[2:20], method = "euclidean")
rownames(movies)

# Quick Question
# 
# Using the table function in R, please answer the following questions about the dataset "movies".
# 
# How many movies are classified as comedies?
table(movies$Comedy) #502
# 
# 
# How many movies are classified as westerns?
table(movies$Western) #27
# 
# 
# How many movies are classified as romance AND drama?
table(movies$Romance, movies$Drama) #97
# 

# Quick Question
# 
# Run the cutree function again to create the cluster groups, but this time pick k = 2 clusters.
# It turns out that the algorithm groups all of the movies that only belong to one specific genre
# in one cluster (cluster 2), and puts all of the other movies in the other cluster (cluster 1). 
# What is the genre that all of the movies in cluster 2 belong to?

# Assign points to clusters
clusterGroups2 = cutree(clusterMovies, k = 2)
cluster2_1 = subset(movies, clusterGroups2==1)
cluster2_2 = subset(movies, clusterGroups2==2)
colMeans(cluster2_1[2:20]) #Mixed
colMeans(cluster2_2[2:20]) #Drama