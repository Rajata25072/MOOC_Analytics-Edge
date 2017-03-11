# If a baseball team scores 713 runs and allows 614 runs, how many games do we expect the team 
# to win?
# 
# Using the linear regression model constructed during the lecture, enter the number of games 
# we expect the team to win:

# Read in data
baseball = read.csv("baseball.csv")
str(baseball)

# Subset to only include moneyball years
moneyball = subset(baseball, Year < 2002)
str(moneyball)

# Compute Run Difference
moneyball$RD = moneyball$RS - moneyball$RA
str(moneyball)

# Scatterplot to check for linear relationship
plot(moneyball$RD, moneyball$W)

# Regression model to predict wins
WinsReg = lm(W ~ RD, data=moneyball)
summary(WinsReg)

WinsReg$coefficients[1] + WinsReg$coefficients[2]*(713-614) #91

# If a baseball team's OBP is 0.311 and SLG is 0.405, how many runs do we expect the team 
# to score?
# 
# Using the linear regression model constructed during the lecture (the one that uses OBP and SLG
# as independent variables), enter the number of runs we expect the team to score:
RunsSReg = lm(RS ~ OBP + SLG, data=moneyball) 
summary(RunsSReg)
OBP = 0.311; SLG=0.405
RunsSReg$coefficients[1]+ RunsSReg$coefficients[2]*OBP + RunsSReg$coefficients[3]*SLG #689
# 
#  
# If a baseball team's opponents OBP (OOBP) is 0.297 and oppenents SLG (OSLG) is 0.370, 
# how many runs do we expect the team to allow?
# 
# Using the linear regression model discussed during the lecture (the one on the last slide 
# of the previous video), enter the number of runs we expect the team to allow:
#   
str(moneyball)
RunsAReg = lm(RA ~ OOBP + OSLG, data=moneyball) 
summary(RunsAReg)
OOBP = 0.297; OSLG=0.370
RunsAReg$coefficients[1]+ RunsAReg$coefficients[2]*OOBP + RunsAReg$coefficients[3]*OSLG #588

# Suppose you are the General Manager of a baseball team, and you are selecting TWO players\
# for your team. 
# 
# You have a budget of $1,500,000, and you have the choice between the following players:
#   
# Player Name  OBP	SLG	Salary
# Eric Chavez	0.338	0.540	$1,400,000
# Jeremy Giambi	0.391	0.450	$1,065,000
# Frank Menechino	0.369	0.374	$295,000
# Greg Myers	0.313	0.447	$800,000
# Carlos Pena	0.361	0.500	$300,000
# Given your budget and the player statistics, which TWO players would you select?
PlayerList <- matrix(c(0.338, 0.540, 1400000,
                       0.391, 0.450, 1065000,
                       0.369, 0.374, 295000,
                       0.313, 0.447, 800000, 
                       0.361, 0.500, 300000), nrow=5, ncol=3, byrow = TRUE,
                     dimnames = list(c("Eric Chavez", "Jeremy Giambi", "Frank Menechino", 
                                       "Greg Myes", "Carlos Pena"),
                                     c("OBP", "SLG", "Salary")))

#RunScored Calculated
summary(RunsReg)
RunS = RunsSReg$coefficients[1]+ RunsSReg$coefficients[2]*PlayerList[,1] + 
  RunsSReg$coefficients[3]*PlayerList[,2]

sort(RunS / PlayerList[,3], decreasing = TRUE)
# Giambi + Pena


# In 2012 and 2013, there were 10 teams in the MLB playoffs: the six teams that had 
# the most wins in each baseball division, and four "wild card" teams. 
# 
# The playoffs start between the four wild card teams - the two teams that win proceed 
# in the playoffs (8 teams remaining). 
# 
# Then, these teams are paired off and play a series of games. 
# 
# The four teams that win are then paired and play to determine who will play in 
# the World Series. 
# 
# We can assign rankings to the teams as follows:
#   
# Rank 1: the team that won the World Series
# Rank 2: the team that lost the World Series
# Rank 3: the two teams that lost to the teams in the World Series
# Rank 4: the four teams that made it past the wild card round, but lost to the above four teams
# Rank 5: the two teams that lost the wild card round
# In your R console, create a corresponding rank vector by typing

teamRank = c(1,2,3,3,4,4,4,4,5,5)

# In this quick question, we'll see how well these rankings correlate with the 
# regular season wins of the teams. In 2012, the ranking of the teams and 
# their regular season wins were as follows:
# 
# Rank 1: San Francisco Giants (Wins = 94)
# Rank 2: Detroit Tigers (Wins = 88)
# Rank 3: New York Yankees (Wins = 95), and St. Louis Cardinals (Wins = 88)
# Rank 4: Baltimore Orioles (Wins = 93), Oakland A's (Wins = 94), 
# Washington Nationals (Wins = 98), Cincinnati Reds (Wins = 97)
# Rank 5: Texas Rangers (Wins = 93), and Atlanta Braves (Wins = 94) 
# 
# Create a vector in R called wins2012, that has the wins of each team in 2012, 
# in order of rank (the vector should have 10 numbers).

wins2012 = c(94,88,95,88,93,94,98,97,93,94)

wins2012 = c("San Francisco Giants", 
             "Detroit Tigers", 
             "New York Yankees", "St. Louis Cardinals",
             "Baltimore Orioles", "Oakland A's", "Washington Nationals", "Cincinnati Reds",
             "Texas Rangers", "Atlanta Braves")

# 
# In 2013, the ranking of the teams and their regular season wins were as follows:
#   
# Rank 1: Boston Red Sox (Wins = 97)
# Rank 2: St. Louis Cardinals (Wins = 97)
# Rank 3: Los Angeles Dodgers (Wins = 92), and Detroit Tigers (Wins = 93)
# Rank 4: Tampa Bay Rays (Wins = 92), Oakland A's (Wins = 96), 
# Pittsburgh Pirates (Wins = 94), and Atlanta Braves (Wins = 96)
# Rank 5: Cleveland Indians (Wins = 92), and Cincinnati Reds (Wins = 90) 
# 
# Create another vector in R called wins2013, that has the wins of each team in 2013, 
# in order of rank (the vector should have 10 numbers).

wins2013 = c(97,97,92,93,92,96,94,96,92,90)

What is the correlation between teamRank and wins2012?
cor(teamRank, wins2012); #0.3477129

What is the correlation between teamRank and wins2013?
cor(teamRank, wins2013); #-0.6556945
