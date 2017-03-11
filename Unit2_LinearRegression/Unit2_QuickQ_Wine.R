# 
# In R, use the dataset wine.csv to create a linear regression model to 
# predict Price using HarvestRain and WinterRain as independent variables. 
wine = read.csv("wine.csv"); str(wine)
modelWine = lm(Price ~ HarvestRain + WinterRain, data=wine)

# Using the summary output of this model, answer the following questions:
summary(modelWine)   
# What is the "Multiple R-squared" value of your model?
# 0.3177
# What is the coefficient for HarvestRain?
# -0.004971
# What is the intercept coefficient?
# 7.865

# Use the dataset wine.csv to create a linear regression model to 
# predict Price using HarvestRain and WinterRain as independent variables, 
# like you did in the previous quick question. 
# 
# Using the summary output of this model, answer the following questions:
#   
# Is the coefficient for HarvestRain significant?
# Y
# Is the coefficient for WinterRain significant?
# N


# Using the data set wine.csv, what is the correlation between 
# HarvestRain and WinterRain?
cor(wine$HarvestRain, wine$WinterRain) #-0.2754409


# Which of the following are NOT valid values for an out-of-sample (test set) R² ? 
# Select all that apply.
wineTest = read.csv("wine_test.csv")
predictTest = predict(modelWine, newdata=wineTest)
predictTest

SSE = sum((wineTest$Price - predictTest)^2)
SST = sum((wineTest$Price - mean(wine$Price))^2)

wineTest$Price - mean(wineTest$Price)

1 - SSE/SST