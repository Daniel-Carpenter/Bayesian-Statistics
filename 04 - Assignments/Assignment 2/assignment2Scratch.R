

Mybeta2 <- function(alpha = 1, beta = 1, n, x, alphaLevel=0.05) {

  source("DBDA2E-utilities.R")  # Load definitions of graphics functions etc.
  source("BernBeta.R")          # Load the definition of the BernBeta function
  
  # Specify the prior:
  t = x                          # Specify the prior MODE.
  
  Prior = c(alpha,beta)          # Specify Prior as vector with the two shape parameters.
  
  # Specify the data:
  N = n                          # The total number of flips.
  z = x                          # The number of heads.
  Data = c(rep(0,N-z),rep(1,z))  # Convert N and z into vector of 0's and 1's.
  
  # Make plots for prior, lik, and post  
  openGraph()
  posterior = BernBeta( priorBetaAB=Prior, Data=Data , plotType="Bars" , 
                        showCentTend="Mode" , showHDI=TRUE , showpD=FALSE )
  
  # Get alpha and beta used to form the posterior distribution
  postAlpha = posterior[1]
  postBeta  = posterior[2]
  
  # Calculate the HDI Tails
  lowerLevel = (alphaLevel / 2)
  upperLevel = (1 - alphaLevel / 2)
  HDI = qbeta(c(lowerLevel, upperLevel), postAlpha, postBeta)
  
  # Return the Lower and Upper HDI at the given level of alpha
  return(list('lowerHDI' = HDI[1],
              'upperHDI' = HDI[2]))
}

n = 10
x = 4
Mybeta(alpha=2, beta=2, n=10, x=6)



# Task 2

Mymixplot <- function(w, a1,b1, a2,b2) {
  myymix<- function(x, w, a1,b1, a2,b2) {
    w*dbeta(x, a1, b1) +(1-w)*dbeta(x, a2,b2)
  }
  curve(myymix(x, w, a1,b1, a2,b2), col = "steelblue3", lwd = 4)
  polygon(curve(myymix(x, w, a1,b1, a2,b2)), col = "steelblue3")
  title('Mixed Beta Plot\nDaniel Carpenter')
}
Mymixplot(0.5, 10,2,3,50)
