# Pracetice with GoogleAdwords - Wrong!!!

# load the lpSolveAPI package:
library(lpSolveAPI)

# Create input data
# Create PricePerClick data frame (PPC)
PPC = data.frame(Query1=c(5,10,5), Query2=c(5,5,20), Query3=c(20,20,25))
rownames(PPC) = c("AT&T", "T-Mobile", "Verizon")
as.matrix(APPD[1,])

# Crete ClickThroughRate data frame (CTR)
CTR = data.frame(Query1=c(0.1,0.1,0.1), Query2=c(0.1,0.15,0.2), Query3=c(0.08,0.1,0.2))
rownames(CTR) = c("AT&T", "T-Mobile", "Verizon")

# Create AvgPricePerDisplay data frame (APPD)
APPD = data.frame(Query1=c(0.5,1,0.5), Query2=c(0.5,0.75,4), Query3=c(1.6,2,5))
rownames(APPD) = c("AT&T", "T-Mobile", "Verizon")

# Budgets
Budget = data.frame(Budget=c(170,100,160))
rownames(Budget) = c("AT&T", "T-Mobile", "Verizon")

# QueryEstimate
QEst = data.frame(Budget=c(140,80,80))
rownames(QEst) = c("AT&T", "T-Mobile", "Verizon")

# Objective & Constraint 
# The first step is to create a model, which takes as arguments the number of constraints in your model, 
# and the number of decision variables in your model. 
# We have 6 constraints (one capacity constraint and two demand constraints) and 9 decision variables.

GoogleAds = make.lp(6,9) #Stop showing at 9 decision variables

# by viewing the constraints in a matrix format. Our objective and 6W constraints are as follows:

# max         AQ1*0.5 + TQ1*0.1 + VQ1*0.5 + AQ2*0.5 + TQ2*0.75 + VQ2*4 + TQ3*2 +  AQ3*1.6 + VQ3*5 - unlist(APPD)
# subject to    AQ1 + TQ1 + VQ1 <= 140  #Query1 - c(1,1,1,0,0,0,0,0,0)
#               AQ2 + TQ2 + VQ2 <= 80   #Query2 - c(0,0,0,1,1,1,0,0,0)
#               AQ3 + TQ3 + VQ3 <= 80   #Query3 - c(0,0,0,0,0,0,1,1,1)
#               AQ1 + TQ1 + VQ1 <= 170  #AT&T Budget - constraint = APPD[1,] - c(unlist(APPD[1,]),0,0,0,0,0,0)
#               AQ2 + TQ2 + VQ2 <= 100  #T-Mobile Budget - - constraint = APPD[2,] - c(0,0,0,unlist(APPD[2,]),0,0,0)
#               AQ3 + TQ3 + VQ3 <= 60   #Verizon Budget - constraint = APPD$[3,] - c(unlist(0,0,0,0,0,0,APPD[3,]))

# set constraints parameter
set.row(GoogleAds, 1, c(1,1,1,0,0,0,0,0,0))
set.row(GoogleAds, 2, c(0,0,0,1,1,1,0,0,0))
set.row(GoogleAds, 3, c(0,0,0,0,0,0,1,1,1))
set.row(GoogleAds, 4, c(unlist(APPD[1,]),0,0,0,0,0,0))
set.row(GoogleAds, 5, c(0,0,0,unlist(APPD[2,]),0,0,0))
set.row(GoogleAds, 6, c(0,0,0,0,0,0,unlist(APPD[3,])))
# set constraint condition
set.constr.type(GoogleAds, c("<=","<=","<=","<=","<=","<="))
# set constraint bound
set.rhs(GoogleAds, c(140,80,80,170,100,60))

# set objective coefficient
set.objfn(GoogleAds, unlist(APPD))

# set to max revenue
lp.control(GoogleAds,sense='max')

solve(GoogleAds)

get.objective(GoogleAds)
get.variables(GoogleAds)
