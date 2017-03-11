# Quick Question
# (1 point possible)
# Create the fertility rate versus population under 15 plot again:
#   
#   ggplot(WHO, aes(x = FertilityRate, y = Under15)) + geom_point()
# 
# Now, color the points by the Region variable.
# 
# Note: You can add scale_color_brewer(palette="Dark2") to your plot if you are having a hard time
# distinguishing the colors (this color palette is often better if you are colorblind). 
# To use this option, you should just add scale_color_brewer(palette="Dark2") to your 
# plotting command right after geom_point(). 
# To find out more about using ggplot in a colorblind-friendly way, please see this website.
# 
# One region in particular has a lot of countries with a very low fertility rate and 
# a very low percentage of the population under 15. Which region is it?

WHO = read.csv("WHO.csv")
str(WHO)
library(ggplot2)
ggplot(WHO, aes(x = FertilityRate, y = Under15, color = Region)) + geom_point() +
  scale_color_brewer(palette="Dark2")