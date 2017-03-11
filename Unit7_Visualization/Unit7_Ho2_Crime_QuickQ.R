## QuickQ
# Create a new line plot, like the one in Video 3, but add the argument "linetype=2". 
# So the geom_line part of the plotting command should look like:
#   
#   geom_line(aes(group=1), linetype=2)
# 
# What does this do?
# 
# Load our data:
mvt = read.csv("mvt.csv", stringsAsFactors=FALSE)
# Convert the Date variable to a format that R will recognize:
mvt$Date = strptime(mvt$Date, format="%m/%d/%y %H:%M")

# Extract the hour and the day of the week:
mvt$Weekday = weekdays(mvt$Date)
mvt$Hour = mvt$Date$hour

# Let's take a look at the structure of our data again:
str(mvt)

# Create a simple line plot - need the total number of crimes on each day of the week. We can get this information by creating a table:
table(mvt$Weekday)

# Save this table as a data frame:
WeekdayCounts = as.data.frame(table(mvt$Weekday))

str(WeekdayCounts) 


# Load the ggplot2 library:
library(ggplot2)

# Create our plot
ggplot(WeekdayCounts, aes(x=Var1, y=Freq)) + geom_line(aes(group=1))  
ggplot(WeekdayCounts, aes(x=Var1, y=Freq)) + geom_line(aes(group=1), linetype=2)



# x Makes the line thicker 
# x Changes the color of the line to blue  
# o Makes the line dashed  
# x Makes the line lighter in color
# 
# 
# 
# Now, change the alpha parameter to 0.3 by replacing "linetype=2" with "alpha=0.3" in the plot command. 
# What does this do?
# 
ggplot(WeekdayCounts, aes(x=Var1, y=Freq)) + geom_line(aes(group=1), alpha=0.3)

# x Makes the line thicker  
# x Changes the color of the line to blue  
# x Makes the line dashed  
# o Makes the line lighter in color