# source("DBDA2E-utilities.R")  # Load definitions of graphics functions etc.
source("BernBeta.R")          # Load the definition of the BernBeta function

# Specify the prior:
t = 0.75             # Specify the prior MODE.
n = 25               # Specify the effective prior sample size.
a = t*(n-2) + 1      # Convert to beta shape parameter a.
b = (1-t)*(n-2) + 1  # Convert to beta shape parameter b.

Prior = c(a,b)       # Specify Prior as vector with the two shape parameters.

# Specify the data:
N = 1                         # The total number of flips.
z = 1                         # The number of heads.
Data = c(rep(0,N-z),rep(1,z))  # Convert N and z into vector of 0's and 1's.

openGraph(width=5,height=7)
posterior = BernBeta( priorBetaAB=Prior, Data=Data , plotType="Bars" , 
                      showCentTend="Mode" , showHDI=TRUE , showpD=FALSE )
saveGraph(file="BernBetaExample",type="png")



beta(2,4)

# Exercise 6.1A:
openGraph(width=5,height=7)
postA = BernBeta( priorBetaAB=c(3,3) , Data=c(1) )

# Exercise 6.1B:
# openGraph(width=5,height=7)
postB = BernBeta( priorBetaAB=postA , Data=c(0) )

# Exercise 6.1C:
# openGraph(width=5,height=7)
postC = BernBeta( priorBetaAB=postB , Data=c(1) )
