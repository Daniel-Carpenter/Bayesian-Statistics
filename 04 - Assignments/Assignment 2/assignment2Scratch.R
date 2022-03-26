

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


# TASK 2 ==========================================================

Mymixbeta <- function(w=0.5, n=10, x, a1=1, a2=1, b1=1, b2=1) {
  # Specify the prior:
  t = x                          # Specify the prior MODE.
  
  # Specify the data:
  N = n                          # The total number of flips.
  z = x                          # The number of heads.
  Data = c(rep(0,N-z),rep(1,z))  # Convert N and z into vector of 0's and 1's.
  
  # Create summary values of Data:
  z = sum( Data ) # number of 1's in Data
  N = length( Data ) 
  
  Theta = seq(0.001,0.999,by=0.001)                                                 # points for plotting
  pTheta = w*dbeta(Theta,a1,b1) +(1-w)*dbeta(Theta,a2,b2)                           # prior for plotting
  pThetaGivenData = w*dbeta(Theta, a1+z, b1+N-z) + (1-w)*dbeta(Theta, a2+z, b2+N-z) # posterior for plotting
  pDataGivenTheta = Theta^z * (1-Theta)^(N-z)                                       # likelihood for plotting
  
  
  # Plot Layout
  layout( matrix( c( 1,2,3 ) ,nrow=3 ,ncol=1 ,byrow=FALSE ) ) # 3x1 panels
  par( mar=c(3,3,1,0) , mgp=c(2,0.7,0) , mai=c(0.5,0.5,0.3,0.1) ) # margins
  cexAxis = 1.33
  cexLab = 1.75
  # convert plotType to notation used by plot:
  plotType="h"
  dotsize = 5 # how big to make the plotted dots
  barsize = 5 # how wide to make the bar lines    
  # y limits for prior and posterior:
  yLim = c(0,1.1*max(c(pTheta,pThetaGivenData)))
  
  
  # Plot the Prior
  myplot1 <- plot( Theta , pTheta , type=plotType , 
        pch="." , cex=dotsize , lwd=barsize ,
        xlim=c(0,1) , ylim=yLim , cex.axis=cexAxis ,
        xlab=bquote(theta) , ylab='Mixed Beta' , 
        cex.lab=cexLab ,
        main="Prior (beta)" , cex.main=1.5 , col="skyblue")
  
  # Plot the likelihood
  plot( Theta , pDataGivenTheta , type=plotType , 
        pch="." , cex=dotsize , lwd=barsize ,
        xlim=c(0,1) , ylim=c(0,1.1*max(pDataGivenTheta)) , cex.axis=cexAxis ,
        xlab=bquote(theta) , ylab=bquote( "p(D|" * theta * ")" ) , 
        cex.lab=cexLab ,
        main="Likelihood (Bernoulli)" , cex.main=1.5 , col="skyblue" )
  
  # Plot the posterior.
  plot( Theta , pThetaGivenData , type=plotType , 
        pch="." , cex=dotsize , lwd=barsize ,
        xlim=c(0,1) , ylim=yLim , cex.axis=cexAxis ,
        xlab=bquote(theta) , ylab='Mixed Beta Posterior' , 
        cex.lab=cexLab ,
        main="Posterior (beta)" , cex.main=1.5 , col="skyblue" )
  print(myplot1)
}

Mymixbeta(w=0.5, n=10, x=4, a1=2, a2=2, b1=4, b2=4)



# TASK 3 =============================================================================


mypriormix <- function() {
  
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
  dataList = list(x = 15, n = 20)
  
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
  codaSamples = coda.samples( jagsModel , variable.names=c("theta[2]","biased","theta[1]") ,
                              n.iter=3334 )
  save( codaSamples , file=paste0(fileNameRoot,"Mcmc.Rdata") )
  
  # Examine the chains ----------------------------------------------------------
  
  # Convergence diagnostics:
  filename1 <- paste0(fileNameRoot,"Theta[2]Diag")
  diagMCMC( codaObject=codaSamples , parName="theta[2]" )
  saveGraph( file= filename1, type="jpg" )
  
  
  # Posterior descriptives:
  filename2 <- paste0(fileNameRoot,"Theta[2]Post")
  openGraph(height=3,width=4)
  par( mar=c(3.5,0.5,2.5,0.5) , mgp=c(2.25,0.7,0) )
  plotPost( codaSamples[,"theta[2]"] , main="theta[2]" , xlab=bquote(theta[2]) )
  saveGraph( file=filename2, type="jpg" )
  graphics.off() # this is done to prevent very last graph being displyed in RMD out
  
  
  # Summary Statistics/Point Estimates
  return(list(su = summary(codaSamples), codaSamples = codaSamples,
              diagGraph = paste0(filename1, '.jpg'), postGraph = paste0(filename2, '.jpg')))
}

mypriormix() -> ans



# TASK 4 =======================================================================

class(ans$codaSamples) # Class is a list
str(ans$codaSamples)   # Structure of the mcmc list

# Use codaSamples to get the Monitored Variables - show head too
mcmc1 = as.data.frame(ans$codaSamples[[1]]); head(mcmc1)
mcmc2 = as.data.frame(ans$codaSamples[[2]]); head(mcmc2)
mcmc3 = as.data.frame(ans$codaSamples[[3]]); head(mcmc3)

# make mcmcT by combining above
mcmcT = rbind.data.frame(mcmc1,mcmc2,mcmc3)

require(ggplot2)

# Problem c
ggplot(mcmcT, aes(x = `theta[2]`, fill = biased)) +
  geom_histogram() + # Create a  hist layer
  facet_wrap(~biased) +
  labs(title = 'Theta[2] Seperated by Biased and Unbiased',
  subtitle = 'Daniel Carpenter') +
  theme_minimal() + theme(text = element_text(color = '#666666'))
  
# Problem e
ggplot(mcmcT, aes(x = `theta[2]`)) +
  geom_histogram(color = 'skyblue4', fill = 'skyblue', alpha = 0.5) + # Create a  hist layer
  labs(title = 'Incorrect Posterior from MCMC Sample',
  subtitle = 'Theta[2] MCMC Sample: Gibbs Updating from a Mixture - Daniel Carpenter') +
  theme_minimal() + theme(text = element_text(color = '#666666'))

