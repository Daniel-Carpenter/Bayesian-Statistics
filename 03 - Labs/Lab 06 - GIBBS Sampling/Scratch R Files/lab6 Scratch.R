# This function makes discrete simulations from a posterior with any number of h values
# n=nu of iterations
# You can embellish this function
simR<-function(n=10000, h,...){
  alpha<-c() # holds transition probs
  alpha[1]<-1
  u<-c() # holds uniform values
  u[1]<-1
  post<-c()# post sample
  prop<-c() # vec of proposed states 1s and 2s
  prop[1]=1 # initial state
  post[1]=prop[1]
  for(i in 2:n){ # starts at 2 because initial value given above
    # proposal state 
    prop[i]=sample(1:length(h),1,replace=TRUE)
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
  
    
  # Simulation output ------------------------------------------------------
  simPlot <- ggplot(data.frame(sim), 
                    aes(x=post, y = Freq)) + 
    
    # COlumn Chart of the post estimated
    geom_col( fill="lightblue", colour="lightblue4", alpha = 0.6) +
    
    # Line of the Posterior Exactly Calculated
    
    
    
    # Theme
    myTheme + 
    
    # Labels
    labs(title = paste("Discrete MCMC Similation Output using simR()\nAssumes",
                       "Uniform Prior dist. over ", numThetaValues, 
                       " values of theta.\nProb. ", x, 
                       "with", n, "bernoulli trials - Daniel Carpenter"),
         y = "Frequency",
         x = "Proposed h's")
    # Now print the plot
    print(simPlot)
    
    
  
  # Trace Plot  ------------------------------------------------------------
    tracePlot <- ggplot(data=iterOut,
           aes(x=numIterations, y=V4)) +
      
      # Add the lines
      geom_line(color='lightblue3', alpha = 0.8) +
      
      # Add some points
      geom_point(color = 'lightblue4', size = 0.25, alpha = 0.1) + 
      
      # Theme and Labels
      myTheme +
      labs(title = "Trace Plot - Est. Posterior",
           subtitle = paste("Assumes Uniform Prior dist. over ", numThetaValues, 
                            " values of theta.\nProb. ", x, 
                            "with", n, "bernoulli trials - Daniel Carpenter"),
           x = "Number of Iterations",
           y = "Posterior Value")
    
    # Now print the aboce plot
    print(tracePlot)
    
    
  # Show Alpha  -------------------------------------------------------------
    alphaPlot <- ggplot(data=iterOut,
           aes(x=numIterations, y=V3)) +
      
      # Add the lines
      geom_line(color='lightblue3', alpha = 0.8) +
      
      # Add some points
      geom_point(color = 'lightblue4', size = 0.25, alpha = 0.1) + 
      
      # Theme and Labels
      myTheme +
      labs(title = "Trace Plot - Est. Alpha",
           subtitle = paste("Assumes Uniform Prior dist. over ", numThetaValues, 
                            " values of theta.\nProb. ", x, 
                            "with", n, "bernoulli trials - Daniel Carpenter"),
           x = "Number of Iterations",
           y = "Alpha Value")
    
    # Now print the plot
    print(alphaPlot)
    
    
  # Show Alpha  -------------------------------------------------------------
    propOut <- ggplot(data=iterOut,
           aes(x=numIterations, y=V1)) +
      
      # Add the lines
      geom_line(color='lightblue3', alpha = 0.8) +
      
      # Add some points
      geom_point(color = 'lightblue4', size = 0.25, alpha = 0.1) + 
      
      # Theme and Labels
      myTheme +
      labs(title = "Trace Plot - The Proposal",
           subtitle = paste("Assumes Uniform Prior dist. over ", numThetaValues, 
                            " values of theta.\nProb. ", x, 
                            "with", n, "bernoulli trials - Daniel Carpenter"),
           x = "Number of Iterations",
           y = "Alpha Value")
    
    # Now print the plot
    print(propOut)
    
    
  # The returned output is a list 
  # Use obj$ to obtain whatever interests you
  return(list(iter=res,sim=sim,postexact=postexact,post=post) )
}


# Create a function to establish a Uniform Binomial Experiment
getH <- function(numThetaValues, x, n) {
  
  ## Form uniform probability
  theta <- seq(0, 1, length = numThetaValues)
  
  ## Calculate prior assuming uniform distribution
  prior = rep(1/numThetaValues, numThetaValues)
  
  ## Calculate the likelihood
  likelihood  = dbinom(x=x, size=n, prob=theta)
  
  ## Calculate the Prior x the Likelihood
  h <-  prior * likelihood
  
  return(h)
}


# Inputs for getH()
numThetaValues = 40
x = 4
n = 10

# Call simROut and add some extras to the plot
simR(n=10000, h=getH(numThetaValues, x, n))



