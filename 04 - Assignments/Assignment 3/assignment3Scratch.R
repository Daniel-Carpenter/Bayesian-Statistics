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

mymcmc <- function(n=10000, h, p=0.5, proposed, ...){
  
  # Function to sample n tosses with head proposed has a probability p
  # proposed 2-values in vector. First value is success, second is failure
  myprop <- function(n, p, proposed){ 
    proposalDist <- sample(proposed, size = n,
                           replace = TRUE, prob = c(p, 1-p)) 
    
    return(list('proposalDist' = proposalDist, 'n'=n, 'p'=p))
  }
  alpha<-c() # holds transition probs
  alpha[1]<-1
  u<-c() # holds uniform values
  u[1]<-1
  post<-c()# post sample
  prop<-c() # vec of proposed states 1s and 2s
  prop[1]=proposed[1] # initial state
  post[1]=prop[1]
  
  for(i in 2:n){ # starts at 2 because initial value given above
    # proposal state 
    prop[i]=myprop(n=1, p=p, proposed=proposed)$proposalDist   
    
    # calculate alpha
    # notice h[prop[i]] gives the actual value of h
    alpha[i]=min(1,h[prop[i]]/h[post[i-1]])
    
    # to calculate accepting proposal with prob alpha
    # select a random value from  a uniform (0,1)
    u[i]=runif(1)
    if(u[i]<=alpha[i]){post[i]<-prop[i]}
    else{post[i]<-post[i-1]}
  }
  res<-matrix(c(prop,u,alpha,post ),nc=4,nr=n,byrow=FALSE)
  sim<-table(post)/n
  # windows only works with a pc
  # quartz with macs
  # dev.new(noRStudioGD = TRUE) # or quartz() 
  
  postexact<-h/sum(h)
  
  
  # PLOTS ======================================================================
  
  require(tidyverse)
  require(ggplot2)
  
  # Create a simple theme for reuse
  myTheme <- theme_minimal() + theme(text = element_text(color = '#666666'),
                                     panel.grid.major = element_blank())
  
  
  # Make the iter vector a data frame
  iterOut <- as.data.frame(res) %>% 
    mutate(numIterations = row_number()) 
  
  
  
  # Show Proposal  -------------------------------------------------------------
  propPlot <- ggplot(data.frame(table(prop)/n),
                     aes(x=prop, y = Freq)) +
    
    # COlumn Chart of the post estimated
    geom_col( fill="lightblue", colour="lightblue4", alpha = 0.5) +
    
    # Theme
    myTheme +
    
    # Labels
    labs(title = 'Proposal', y = "Probability", x = expression(theta) )
  
  
  # Posterior simulation ------------------------------------------------------
  postPlot <- ggplot(data.frame(table(post)/n), 
                     aes(x=post, y = Freq)) + 
    
    # COlumn Chart of the post estimated
    geom_col( fill="tomato3", colour="tomato4", alpha = 0.5) +
    
    # Theme
    myTheme + 
    
    # Labels
    labs(title = "Posterior", y = "Probability", x = expression(theta) )
  
  
  # Posterior Trace Plot  ------------------------------------------------------
  tracePostPlot <- ggplot(data=iterOut,
                          aes(x=numIterations, y=V4)) +
    
    # Add the lines
    geom_line(color='grey75', alpha = 0.7) +
    
    # Add some points
    geom_point(color = 'grey40', size = 0.25, alpha = 0.1) + 
    
    # Theme and Labels
    myTheme +
    labs(title = "Trace Plot", x = "Iterations", y = expression(theta) )
  
  
  # Print the plots on single grid ---------------------------------------------
  require(cowplot) # allows for plotting on single grid using plot_grid())
  print(plot_grid(propPlot, postPlot, tracePostPlot,
                  nrow = 1, rel_widths = c(1,1,2)))
  
  
  # The returned output is a list 
  # Use obj$ to obtain whatever interests you
  return(list(iter=res,sim=sim,postexact=postexact,post=post) )
}


# Create a function to establish a Uniform Binomial Experiment
getBernBetaH <- function(numThetaValues, 
                         x, n, 
                         shape1=2, shape2=2) {
  
  ## Form uniform probability
  theta <- seq(0, 1, length = numThetaValues)
  
  ## Calculate prior assuming uniform distribution
  prior = dbeta(theta, shape1, shape2)
  plot(prior)
  
  ## Calculate the likelihood
  likelihood  = dbinom(x=x, size=n, prob=theta)
  plot(likelihood)
  
  ## Calculate the Prior x the Likelihood
  h <-  prior * likelihood
  plot(h)
  
  return(h)
}


# Inputs for getH()
numThetaValues = 1000

# Proposal Inputs 
p=0.5
proposed=c(0.3,0.6)

# Beta prior inputs
shape1 = 2
shape2 = 2

# Bernoulli Liklihood inputs
x = 4
n = 10

# h: prior x liklihood using beta prior and bernoulli likelihood
h=getBernBetaH(numThetaValues, x, n, shape1,shape2)

# Call mydmcmc and add some extras to the plot
mydmcmcOutput <- mymcmc(n=300, h, p=0.5, proposed=c(0.3,0.6))














