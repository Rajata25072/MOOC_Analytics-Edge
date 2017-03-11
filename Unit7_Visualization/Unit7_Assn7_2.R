# VISUALIZING NETWORK DATA
# 
# The cliche goes that the world is an increasingly interconnected place, and the 
# connections between different entities are often best represented with a graph. 
# Graphs are comprised of vertices (also often called "nodes") and edges connecting 
# those nodes. 
# In this assignment, we will learn how to visualize networks using the igraph package in R.
# 
# For this assignment, we will visualize social networking data using anonymized data 
# from Facebook; this data was originally curated in a recent paper about computing 
# social circles in social networks. 
# In our visualizations, the vertices in our network will represent Facebook users and 
# the edges will represent these users being Facebook friends with each other.
# 
# The first file we will use, edges.csv, contains variables V1 and V2, which label the 
# endpoints of edges in our network. 
# Each row represents a pair of users in our graph who are Facebook friends. 
# For a pair of friends A and B, edges.csv will only contain a single row -- the smaller 
# identifier will be listed first in this row. 
# From this row, we will know that A is friends with B and B is friends with A.
# 
# The second file, users.csv, contains information about the Facebook users, who are 
# the vertices in our network. 
# This file contains the following variables:
#   
#   id: A unique identifier for this user; this is the value that appears in the rows of 
#     edges.csv
#   gender: An identifier for the gender of a user taking the values A and B. 
#     Because the data is anonymized, we don't know which value refers to males and 
#     which value refers to females.
#   school: An identifier for the school the user attended taking the values A and AB 
#     (users with AB attended school A as well as another school B). 
#     Because the data is anonymized, we don't know the schools represented by A and B.
#   locale: An identifier for the locale of the user taking the values A and B. 
#     Because the data is anonymized, we don't know which value refers to what locale.
# 
# 
# ################################################################################
# Problem 1.1 - Summarizing the Data
# Load the data from edges.csv into a data frame called edges, and load the data from 
# users.csv into a data frame called users.
# 
edges = read.csv("edges.csv")
users = read.csv("users.csv")
# How many Facebook users are there in our dataset?
nrow(users) #59
# 
# 
# In our dataset, what is the average number of friends per user? 
nrow(edges)*2 / nrow(users) #4.949153
# 
# 
# Hint: this question is tricky, and it might help to start by thinking about a small 
# example with two users who are friends.
# 
# 
# From str(edges) or nrow(edges), we see that there are 146 pairs of users in our dataset
# who are Facebook friends. 
# However, each pair (A, B) must be counted twice, because B is a friend of A and A is 
# a friend of B. 
# To think of this in simpler terms, consider a network with just new people, A and B, 
# and a single edge (A, B). Even though there are two vertices and one edge, each user 
# has on average one friend.
# 
# For our network, the average number of friends per user is 292/59=4.95.
# 
# Finally, note that in all likelihood these users have a much higher number of Facebook 
# friends. 
# We are computing here the average number of people in this dataset who are their friends,
# instead of the average total number of Facebook friends.
# ################################################################################
# Problem 1.2 - Summarizing the Data
# Out of all the students who listed a school, what was the most common locale?
# 
table(school = users$school, locale = users$locale)
# x Locale A  
# o Locale B
# 
# 
# ################################################################################
# Problem 1.3 - Summarizing the Data
# Is it possible that either school A or B is an all-girls or all-boys school?
table(gender=users$gender, school=users$school)
# No
# 
# 
# ################################################################################
# Problem 2.1 - Creating a Network
# We will be using the igraph package to visualize networks; install and load this 
# package using the install.packages and library commands.
# 
# We can create a new graph object using the graph.data.frame() function. 
# Based on ?graph.data.frame, which of the following commands will create a graph g 
# describing our social network, with the attributes of each user correctly loaded?
# 
# Note: A directed graph is one where the edges only go one way -- they point from 
# one vertex to another. 
# The other option is an undirected graph, which means that the relations between the 
# vertices are symmetric.
# 
install.packages("igraph")
library(igraph)
?graph.data.frame
# o g = graph.data.frame(edges, FALSE, users)  
# x g = graph.data.frame(users, FALSE, edges)  
# x g = graph.data.frame(edges, TRUE, users)  
# x g = graph.data.frame(users, TRUE, edges)
# 
# 
# ################################################################################
# Problem 2.2 - Creating a Network
# Use the correct command from Problem 2.1 to load the graph g.
# 
# Now, we want to plot our graph. By default, the vertices are large and have text 
# labels of a user's identifier. 
# Because this would clutter the output, we will plot with no text labels and smaller 
# vertices:
#   plot(g, vertex.size=5, vertex.label=NA)
# 
g = graph.data.frame(edges, FALSE, users)  
plot(g, vertex.size=5, vertex.label=NA)
# In this graph, there are a number of groups of nodes where all the nodes in each 
# group are connected but the groups are disjoint from one another, forming "islands" 
# in the graph. 
# Such groups are called "connected components," or "components" for short. 
# How many connected components with at least 2 nodes are there in the graph?
# 4
# 
# 
# How many users are there with no friends in the network?
# 7
# 
# 
# ################################################################################
# Problem 2.3 - Creating a Network
# In our graph, the "degree" of a node is its number of friends. 
# We have already seen that some nodes in our graph have degree 0 
# (these are the nodes with no friends), while others have much higher degree. 
# We can use degree(g) to compute the degree of all the nodes in our graph g.
# 
sort(degree(g))
# How many users are friends with 10 or more other Facebook users in this network?
sum(degree(g)>=10) #9
# 
# ################################################################################
# Problem 2.4 - Creating a Network
# In a network, it's often visually useful to draw attention to "important" nodes 
# in the network. 
# While this might mean different things in different contexts, in a social network we 
# might consider a user with a large number of friends to be an important user. 
# From the previous problem, we know this is the same as saying that nodes with a 
# high degree are important users.
# 
# To visually draw attention to these nodes, we will change the size of the vertices so 
# the vertices with high degrees are larger. 
# To do this, we will change the "size" attribute of the vertices of our graph to be an 
# increasing function of their degrees:
# 
 V(g)$size = degree(g)/2+2
# 
# Now that we have specified the vertex size of each vertex, we will no longer use the 
# vertex.size parameter when we plot our graph:
# 
 plot(g, vertex.label=NA)
# 
# What is the largest size we assigned to any node in our graph?
sort(V(g)$size) #11
# 
# 
# What is the smallest size we assigned to any node in our graph?
sort(V(g)$size) #2
# From table(degree(g)) or summary(degree(g)), we see that the maximum degree of any node 
# in the graph is 18 and the minimum degree of any node is 0. 
# Therefore, the maximum size of any point is 18/2+2=11, and the minimum size is 0/2+2=2.
# 
# 
# ################################################################################
# Problem 3.1 - Coloring Vertices
# Thus far, we have changed the "size" attributes of our vertices. 
# However, we can also change the colors of vertices to capture additional information 
# about the Facebook users we are depicting.
# 
# When changing the size of nodes, we first obtained the vertices of our graph with 
# V(g) and then accessed the the size attribute with V(g)$size. 
# To change the color, we will update the attribute V(g)$color.
# 
# To color the vertices based on the gender of the user, we will need access to that 
# variable. 
# When we created our graph g, we provided it with the data frame users, which had 
# variables gender, school, and locale. 
# These are now stored as attributes V(g)$gender, V(g)$school, and V(g)$locale.
# We can update the colors by setting the color to black for all vertices, than setting it 
# to red for the vertices with gender A and setting it to gray for the vertices with 
# gender B:
# 
V(g)$color = "black"
V(g)$color[V(g)$gender == "A"] = "red"
V(g)$color[V(g)$gender == "B"] = "gray"
# 
# Plot the resulting graph. What is the gender of the users with the highest degree in 
# the graph?
plot(g, vertex.label=NA)
table(V(g)$color)
# 
# x Missing gender value  
# x Gender A  
# o Gender B
# 
# 
# ################################################################################
# Problem 3.2 - Coloring Vertices
# Now, color the vertices based on the school that each user in our network attended.
# 
V(g)$color = "black"
V(g)$color[V(g)$school == "A"] = "red"
V(g)$color[V(g)$school == "AB"] = "gray"
table(V(g)$color)
plot(g, vertex.label=NA)
# Are the two users who attended both schools A and B Facebook friends with each other?
# 
# o Yes  
# x No
# 
# 
# What best describes the users with highest degree?
# 
table(V(g)$color, degree(g))
# x None of the high-degree users attended school A  
# o Some, but not all, of the high-degree users attended school A  
# x All of the high-degree users attended school A
# 
# 
# ################################################################################
# Problem 3.3 - Coloring Vertices
# Now, color the vertices based on the locale of the user.
# 
V(g)$color = "black"
V(g)$color[V(g)$locale == "A"] = "red"
V(g)$color[V(g)$locale == "B"] = "gray"
plot(g, vertex.label=NA)
# The large connected component is most associated with which locale?
# 
# x Locale A  
# o Locale B
# 
# 
# The 4-user connected component is most associated with which locale?
# o Locale A  
# x Locale B
# 
# 
# ################################################################################
# Problem 4 - Other Plotting Options
# The help page is a helpful tool when making visualizations. 
# Answer the following questions with the help of ?igraph.plotting and experimentation in 
# your R console.
# 
# Which igraph plotting function would enable us to plot our graph in 3-D?
?igraph #rglplot
plot(g, vertex.label=NA)
rglplot(g, vertex.label=NA)
# 
# 
# What parameter to the plot() function would we use to change the edge width when 
# plotting g?
?igraph #edge
# 
# The three functions to plot the igraph are plot.igraph 
# (the function we used through the command "plot"), tkplot, and rglplot. 
# rglplot makes 3-D plots -- you can try one with rglplot(g, vertex.label=NA). 
# Once you've made the plot, you can click and drag to rotate the graph. 
# To use this function, you will need to install and load the "rgl" package.
# 
# To change the edge width, you need to change the edge parameter called "width". 
# From ?igraph.plotting, we read that we need to append the prefix "edge." to 
# the beginning for our call to plot, so the full parameter is called "edge.width". 
# For instance, we could plot with edge width 2 with the command 
# plot(g, edge.width=2, vertex.label=NA).