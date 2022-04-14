# ==============================================================================
# TASK 1
# ==============================================================================

# The data input
y = c( rep(1,9),rep(0,3) , rep(1,45),rep(0,15) , rep(1,3),rep(0,9) ) 
s = c( rep("A",12) , rep("B",60) , rep("C",12) )
fileName = "Ass3.1.csv"
write.csv( data.frame(y=y,s=s) , file=fileName , row.names=FALSE )


# Example for Jags-Ydich-XnomSsubj-Mbernbeta.R 
#------------------------------------------------------------------------------- 
# Optional generic preliminaries:
graphics.off() # This closes all of R's graphics windows.
# rm(list=ls())  # Careful! This clears all of R's memory!

#------------------------------------------------------------------------------- 
# Load The data 
myData = read.csv(fileName)

# Include head of data
head(myData)

# N.B.: The functions below expect the data to be a data frame, 
# with one component named y being a vector of integer 0,1 values,
# and one component named s being a factor of subject identifiers.
myData$s = factor( myData$s )

#------------------------------------------------------------------------------- 
# Load the relevant model into R's working memory:
source("Jags-Ydich-XnomSsubj-MbernBeta.R")

#------------------------------------------------------------------------------- 
# Optional: Specify filename root and graphical format for saving output.
# Otherwise specify as NULL or leave saveName and saveType arguments 
# out of function calls.
dir.create('Output')
fileNameRoot = "Output//"
graphFileType = "png" 

#------------------------------------------------------------------------------- 
# Generate the MCMC chain:
mcmcCoda = genMCMC( data=myData , numSavedSteps=50000 , saveName=fileNameRoot )

#------------------------------------------------------------------------------- 
# Display diagnostics of chain, for specified parameters:
parameterNames = varnames(mcmcCoda) # get all parameter names
for ( parName in parameterNames ) {
  diagMCMC( codaObject=mcmcCoda , parName=parName , 
            saveName=fileNameRoot , saveType=graphFileType )
}
#------------------------------------------------------------------------------- 
# Get summary statistics of chain:
summaryInfo = smryMCMC( mcmcCoda , compVal=NULL , rope=c(0.45,0.55) ,
                        compValDiff=0.0 , ropeDiff = c(-0.05,0.05) ,
                        saveName=fileNameRoot )

# Display posterior information:
plotMCMC( mcmcCoda , data=myData , compVal=NULL , rope=c(0.45,0.55) ,
          compValDiff=0.0 , ropeDiff = c(-0.05,0.05) ,
          saveName=fileNameRoot , saveType=graphFileType )

# Add the histogram
plotPost(mcmcCoda[,'theta[1]'], xlim = c(0,1), xlab= expression(theta[1]),
         main = 'Daniel Carpenter', col = 'darkseagreen3',
                      showCurve = FALSE)

# Add the density line
paramSampleVec = as.matrix(mcmcCoda[,'theta[1]'])
densCurve = density( paramSampleVec , adjust=2 )
lines( densCurve$x , densCurve$y , type="l" , lwd=2.5 , col='darkseagreen4' , bty="n")


graphics.off() # This closes all of R's graphics windows.


# ==============================================================================
# TASK 2
# ==============================================================================

y_s1 = c(1,0,0,0); paste('Proportion of Heads for y[s[1]]:', sum(y_s1) / sum(y_s1>=0)) # for y_s1
y_s2 = c(1,1,0,0); paste('Proportion of Heads for y[s[2]]:', sum(y_s2) / sum(y_s2>=0)) # for y_s2
y_s3 = c(1,1,0,0); paste('Proportion of Heads for y[s[3]]:', sum(y_s3) / sum(y_s3>=0)) # for y_s3
y_s4 = c(1,1,1,0); paste('Proportion of Heads for y[s[4]]:', sum(y_s4) / sum(y_s4>=0)) # for y_s4

# Function to estimate the liklihood from the MLE
likFromMLE <- function(N, z, omega, kappa, theta) {
  alpha = omega * (kappa-2) + 1 
  beta = (1-omega) * (kappa-2) + 1
  mleProb = theta^z * (1-theta)^(N-z) * dbeta(theta, alpha, beta)
  lik = prod(mleProb) 
  
  return(lik) # Return the likelihood
}

# Inputs 
N     = rep(4,4) 
omega = 0.5 
kappa = 2 
theta = c(0.25, 0.50, 0.50, 0.75) 
z = sum(y_s1, y_s2, y_s3, y_s4) # Sum of the heads

# Call the function
likFromMLE(N, z, omega, kappa, theta)


# ==============================================================================
# TASK 6
# ==============================================================================

# a ----------------------------------------------------------------------------

# Function to sample n tosses with head proposed has a probability p
# proposed 2-values in vector. First value is success, second is failure
myprop <- function(n, p, proposed=c(0.3, 0.6)){ 
  proposalDist <- sample(proposed, size = n,
                         replace = TRUE, prob = c(p, 1-p)) 
  
  return(list('proposalDist' = proposalDist, 'n'=n, 'p'=p))
}

# b ----------------------------------------------------------------------------

myprop(n=10,  p=0.5) 
myprop(n=20,  p=0.1)
myprop(n=100, p=0.4)


# c ----------------------------------------------------------------------------

# Create a bar plot given the output of a myprop function
createBarPlot <- function(myprop) {
  
  # Colors to reuse  
  fillColor   = c('darkseagreen', 'skyblue')
  borderColor = c('darkseagreen4', 'skyblue4')
  
  # Create the barplot
  barplot(table(myprop$proposalDist) / myprop$n, 
          main = paste0('Proposal Distribution from myprop(n,p) | p = ',myprop$p,'\n Daniel Carpenter'),
          col  = fillColor, border = borderColor,
          xlab = expression(theta), ylab = 'Probability')
  
  # Create the legend
  legend('topleft', legend=c("Outcome of 0.3", "Outcome of 0.6"), 
         bty = 1, fill=fillColor, border=borderColor, box.col = 'grey90')
}

n = 1000 # number of tosses
p = 0.3  # Probability of a success, in this case is outcome of 0.3

# Call the bar plot function
# Note proposed c(0.3, 0.6) by default...
# createBarPlot(myprop(n, p))


# e mymcmc ---------------------------------------------------------------------

coindie<-function(n=1000, prob=0.3, 
                  h=c(1/4,3/4),E2=c(5,6),
                  proposedValues, ...){
  
  # Initial value uses the proposed values
  init = proposedValues[2]
  
  library(xtable)
  dieset<-c()
  dieset[1]<-"E1"
  die<-function(n=1){
    sample(1:6,size=n,replace=TRUE)
  }
  
  # Function to sample n tosses with head proposed has a probability p
  # proposed 2-values in vector. First value is success, second is failure
  myprop <- function(n=1, p=prob, proposed=proposedValues){ 
    proposalDist <- sample(proposed, size = n,
                           replace = TRUE, prob = c(p, 1-p)) 
    
    return(list('proposalDist' = proposalDist, 'n'=n, 'p'=p))
  }
  face<-c()
  alpha<-c()      # holds acceptance probs
  alpha[1]<-1
  post<-c()       # post sample
  prop<-c()       # vec of proposed states 1s and 2s
  prop[1]=init    # initial state
  post[1]=prop[1]
  dice<-c()
  dice[1]<-die()
  
  # Form the Acceptance Set
  for(i in 2:n){ # starts at 2 because initial value given above
    
    # Formulate the Proposal
    prop[i]<-myprop()$proposalDist          
    
    # Formulate the Acceptance Set using the Proposal and the Posterior
    alpha[i]=min(1,h[prop[i]]/h[post[i-1]])
    
    dice[i]<-die()
    ifelse(alpha[i]==1,dieset[i]<-"E1",dieset[i]<-"E2")
    
    # is x an element of set y
    if(alpha[i]==1 | (is.element(dice[i],E2) & alpha[i]!=1)){post[i]<-prop[i]}
    else{post[i]<-post[i-1]}
  }  
  res<-matrix(c(prop,round(alpha,2),dieset,dice,post ),nc=5,nr=n,byrow=FALSE,dimnames=list(1:n,c("proposal","alpha", "E","dice","post")))
  sim<-table(post)/n
  print(sim)
  postexact<-h/sum(h)
  
  # Colors to reuse  
  fillColor   = c('darkseagreen1', 'skyblue1')
  borderColor = c('darkseagreen4', 'skyblue4')
  
  # Plot the proposal
  barplot(table(prop)/n,
          main = 'Proposal', xlab = expression(theta), ylab = 'Probability',
          col = fillColor, border = borderColor,
          ...)
  
  
  fillColor   = c('darkseagreen4', 'skyblue3')
  
  # Plot the posterior
  barplot(table(post)/n,
          main = 'Posterior', xlab = expression(theta), ylab = 'Probability',
          col = fillColor, border = borderColor,
          ...)
  
  return(list(iter=res,sim=sim,postexact=postexact,post=post,xtable=xtable(res,dig=1)) )
}

proposedValues=c(0.3, 0.6)

# Call the function
ans <- coindie(n=n, p=p, proposedValues=proposedValues,
               h=c(0.6,0.4),E2=c(2,3,4,5))














