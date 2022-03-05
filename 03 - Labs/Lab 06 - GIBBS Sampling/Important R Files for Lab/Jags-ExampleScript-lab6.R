# Jags-ExampleScript-lab6.R 

# Optional generic preliminaries:
graphics.off() # This closes all of R's graphics windows.
rm(list=ls())  # Careful! This clears all of R's memory!

# Load the functions used below:
source("DBDA2E-utilities.R") # Must be in R's current working directory.
obj = require(rjags)               # Must have previously installed package rjags.
obj
fileNameRoot="JAGS_Lab6_Task3_Ouput_Folder/" # For output file names.
dir.create(fileNameRoot)

# Define the model:
modelString = "
model {
  y[1:2] ~ dmnorm.vcov(mu[1:2],Sigma[1:2,1:2])
Sigma[1,1]<- 4
Sigma[2,2]<- 16
Sigma[1,2]<- 0.2*2*4
Sigma[2,1]<- 0.2*2*4
mu[1]<-10
mu[2]<-5
}
" # close quote for modelString
writeLines( modelString , con=paste0(fileNameRoot, "TEMPmodel.txt" ))

# Run the chains:
jagsModel = jags.model( file=paste0(fileNameRoot, "TEMPmodel.txt" )  , 
                        n.chains=3 , n.adapt=500 )
update( jagsModel , n.iter=5000 )
codaSamples = coda.samples( jagsModel , variable.names=c("y[1]","y[2]") ,
                            n.iter=3334 )
save( codaSamples , file=paste0(fileNameRoot,"Mcmc.Rdata") )

# Examine the chains:
# Convergence diagnostics:
diagMCMC( codaObject=codaSamples , parName="y[1]" )
saveGraph( file=paste0(fileNameRoot,"y[1]") , type="jpeg" )

diagMCMC( codaObject=codaSamples , parName="y[2]" )
saveGraph( file=paste0(fileNameRoot,"y[2]") , type="jpeg" )

# Posterior descriptives:
openGraph()
par( mar=c(3.5,0.5,2.5,0.5) , mgp=c(2.25,0.7,0) )
plotPost( codaSamples[,"y[1]"] , main="x[1]" , xlab=bquote(x[1]) )
saveGraph( file=paste0(fileNameRoot,"x[1]") , type="jpeg" )

# Re-plot with different annotations:
openGraph()
plotPost( codaSamples[,"y[2]"] , main="y[2]" , xlab=bquote(y[2]) )
saveGraph( file=paste0(fileNameRoot,"y[2]") , type="jpeg" )

# Process the MCMC samples in a specialist package
library(ggmcmc)
s = ggs(codaSamples) # This formats the samples for the package
dev.new()
ggs_density(s)
dev.new()
ggs_pairs(s)



