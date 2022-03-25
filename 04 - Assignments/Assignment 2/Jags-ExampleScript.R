
mypriormix <- function(n=10) {

  # Optional generic preliminaries:
  graphics.off() # This closes all of R's graphics windows.
  rm(list=ls())  # Careful! This clears all of R's memory!
  
  
  # Load the functions used below:
  source("DBDA2E-utilities.R") # Must be in R's current working directory.
  require(rjags)               # Must have previously installed package rjags.
  
  fileNameRoot="Assn2Jags/" # For output file names.
  dir.create(fileNameRoot)
  
  # Load the data:
  Ntotal = 10  # Compute the total number of flips.
  dataList = list(    # Put the information into a list.
    n = Ntotal
  )
  
  # Define the model:
  modelString = "
  model {
    x ~ dbin( p, n ) p <- theta[pick] 
    
    # pick is 2 if biased 1 unbiased 
    pick ~ dcat(q[]) # categorical 1 produced prob q[1], etc 
    
    q[1]<-0.9 
    q[2]<-0.1 
    theta[1] <-0.5        # unbiased 
    theta[2] ~ dunif(0,1) # biased
    biased <- pick - 1
  }
  " # close quote for modelString
  writeLines( modelString , con="TEMPmodel.txt" )
  
  # Initialize the chains based on MLE of data.
  initsList = list( pick = 1)
  
  # Run the chains:
  jagsModel = jags.model( file="TEMPmodel.txt" , data=dataList , inits=initsList , 
                          n.chains=3 , n.adapt=500 )
  update( jagsModel , n.iter=500 )
  codaSamples = coda.samples( jagsModel , variable.names=c("theta[2]", "biased") ,
                              n.iter=3334 )
  save( codaSamples , file=paste0(fileNameRoot,"Mcmc.Rdata") )
  
  # Examine the chains ----------------------------------------------------------
    
    # Convergence diagnostics:
    diagMCMC( codaObject=codaSamples , parName="theta[2]" )
    saveGraph( file=paste0(fileNameRoot,"Diag") , type="eps" )
    
    # Posterior descriptives:
    openGraph(height=3,width=4)
    par( mar=c(3.5,0.5,2.5,0.5) , mgp=c(2.25,0.7,0) )
    plotPost( codaSamples[,"theta[2]"] , main="theta[2]" , xlab=bquote(theta) )
    saveGraph( file=paste0(fileNameRoot,"Post") , type="eps" )
    
    # Re-plot with different annotations:
    plotPost( codaSamples[,"theta[2]"] , main="theta[2]" , xlab=bquote(theta) , 
              cenTend="median" , compVal=0.5 , ROPE=c(0.45,0.55) , credMass=0.90 )
    saveGraph( file=paste0(fileNameRoot,"Post2") , type="eps" )
  
    
  # Summary Statistics/Point Estimates
  return(list('summaryOfSamples' = summary(codaSamples)))
}

mypriormix()
