

# Optional generic preliminaries:
graphics.off() # This closes all of R's graphics windows.
rm(list=ls())  # Careful! This clears all of R's memory!

# Load the functions used below:
source("DBDA2E-utilities.R") # Must be in R's current working directory.
require(rjags)               # Must have previously installed package rjags.

fileNameRoot="Jags-ExampleScript" # For output file names.
# Talk sociology
# Bayes regression
# 



# Load the data:
myData = read.csv("SPRUCE.csv") # Read data file; must be in curr. work. dir.
#myData = read.csv("hw37.csv") # Read data file; must be in curr. work. dir.
head(myData)
y = myData$Height        # The y values are in the column named y.
x = myData$BHDiameter
Ntotal = length(y)  # Compute the total number of flips.
dataList = list(    # Put the information into a list.
  x = x,
  y = y ,
  Ntotal = Ntotal 
)

# Define the model:
modelString = "
model {
  for ( i in 1:Ntotal ) {
    y[i] ~ dnorm( mu[i], tau )
    mu[i] <- beta0 + beta1*x[i] + beta2*x[i]^2
  }
  beta0 ~ dnorm(0, 1.0E-06)
  beta1 ~ dnorm(0, 1.0E-06)
  #beta2 ~ dnorm(4, 10)
  beta2 ~ dnorm(0, 1.0E-06)
  sigma ~ dunif(0,1000)
 tau<-pow(sigma,-2)
 
}
" # close quote for modelString
writeLines( modelString , con="TEMPmodel.txt" )

# Initialize the chains based on MLE of data.
# Option: Use single initial value for all chains:
#  thetaInit = sum(y)/length(y)
#  initsList = list( theta=thetaInit )
# Option: Use function that generates random values for each chain:
initsList = list(beta0 = 0, beta1 = 0, beta2 = 0, sigma =10)

# Run the chains:
jagsModel = jags.model( file="TEMPmodel.txt" , data=dataList , inits=initsList , 
                        n.chains=3 , n.adapt=500 )
update( jagsModel , n.iter=500 )
codaSamples = coda.samples( jagsModel , variable.names=c("beta0", "beta1", "beta2", "sigma") ,
                            n.iter=33340 )
save( codaSamples , file=paste0(fileNameRoot,"Mcmc.Rdata") )

# Examine the chains:
# Convergence diagnostics:
diagMCMC( codaObject=codaSamples , parName=c("beta0" ))
diagMCMC( codaObject=codaSamples , parName=c("beta1" ))
diagMCMC( codaObject=codaSamples , parName=c("beta2" ))
diagMCMC( codaObject=codaSamples , parName=c("sigma" ))

saveGraph( file=paste0(fileNameRoot,"betaDiag") , type="eps" )
# Posterior descriptives:
# 

openGraph(height=3,width=4)
par( mar=c(3.5,0.5,2.5,0.5) , mgp=c(2.25,0.7,0) )
plotPost( codaSamples[,"beta0"] , main="beta0" , xlab=bquote(beta[0]))
saveGraph( file=paste0(fileNameRoot,"beta0Post") , type="eps" )
# Re-plot with different annotations:

openGraph(height=3,width=4)
par( mar=c(3.5,0.5,2.5,0.5) , mgp=c(2.25,0.7,0) )
plotPost( codaSamples[,"beta1"] , main="beta1" , xlab=bquote(beta[1]) )
saveGraph( file=paste0(fileNameRoot,"beta1Post2") , type="eps" )

openGraph(height=3,width=4)
par( mar=c(3.5,0.5,2.5,0.5) , mgp=c(2.25,0.7,0) )
plotPost( codaSamples[,"beta2"] , main="beta2" , xlab=bquote(beta[2]) )
saveGraph( file=paste0(fileNameRoot,"beta2Post2") , type="eps" )

openGraph(height=3,width=4)
par( mar=c(3.5,0.5,2.5,0.5) , mgp=c(2.25,0.7,0) )
plotPost( codaSamples[,"sigma"] , main="sigma" , xlab=bquote(sigma) )
saveGraph( file=paste0(fileNameRoot,"sigmaPost2") , type="eps" )

summary(codaSamples)

