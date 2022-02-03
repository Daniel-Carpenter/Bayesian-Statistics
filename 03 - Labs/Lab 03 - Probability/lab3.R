# ------------------------------------------------------------------------------
# Bayesian course
# Lab 3
# distributional theory + plots
# ------------------------------------------------------------------------------


# PROBABILITY CALCULATIONS =====================================================

  # End points do not matter for continuous variables
  #P(X>=3)=P(X>3)
  
  # X ~ N(20,5) 
  # X is a normal distribution, mean of 20, sd of 5
  
  
  # P(X>22)? -------------------------------------------------------------------
  # Probability that X is greater than 22?
  # >= or > matter a lot for discrete vars, not so much continuous
  
  ## p on front gives a tail, by default lower tail. 1 minus (-) gives upper
  ## Area from negative infinity up to 22
  # Check by drawing it (or plotting it)
  1-pnorm(22, mean=10, sd=5) # notice options are in order:  x,mean, sd
  
  
  # P(X<17) --------------------------------------------------------------------
  ## Remember lower tail by default
  ## Probability less than 17
  pnorm(17,mean=10,sd=5)


# PLOTS ========================================================================

  # Base plotting --------------------------------------------------------------
  
  windows() 

  
  ## ADD THE CURVE --------------------------------------------------------------
  
    ### Decide `on a suitable range for x
    ### mean +- 3sd
    curve(dnorm(x,10,5), 
          
          #### Within 3 standard deviations of the mean will include everything
          xlim=c(10-3*5,10+3*5), 
          
          #### Plot
          lwd =2, main = "Wayne's plot")
  

  ## ADD THE AREA --------------------------------------------------------------
  
    ### Say we need P(8<X<=20) - Prob between 8 and 202
    
      #### X axis range (8 to 10)
      xcurve = seq(8,20, length=1000) # length is arbitrary 
      
      #### Y axis - needs xcurve,  mean and sd
      ycurve = dnorm(xcurve, mean=10, sd=5)
      
      #### Use the x and y curve and add to the plot
      polygon(c(8,xcurve,20), c(0,ycurve,0), col="green3") # this adds the area
      
      
  ## ADD THE PROBABILITY TEXT --------------------------------------------------
      area=pnorm(20, mean=10, sd=5) - pnorm(8, mean=10, sd=5)
      area2=round(area,4)
    
      #### Now place this on the above plot
      text(12,0.02,substitute(paste("Prob=",area2 ), list(area2 = area2)))

      
  ## PLOT FUNCTION FROM ABOVE --------------------------------------------------
  
    normalPDF = function(mu, sigma, lowerBound, upperBound)
    {
      
      # Open a window
      dev.new(noRStudioGD = TRUE) # DO NOT CLOSE THIS WINDOW
      
      # Create the line that displays the bell curve
      curve(dnorm(x,mu,sigma), xlim=c(mu-3*sigma,mu+3*sigma), lwd =2, 
            main = paste0("Probability Distribution\n",
            "X ~ N(",mu,", ",sigma,"), P(", lowerBound," < X <= ",upperBound,")"))
      
      # Add the AREA of between the lower and upper bound P(lowerBound<X<=upperBound)
      
        ## X-Axis curve 
        xcurve = seq(lowerBound,upperBound, length=1000) # length is arbitrary 
        
        ## Y-Axis Curve
        ycurve = dnorm(xcurve, mu,sigma)
        
        ## Combine the X and Y curve to form the area (in green)
        polygon(c(lowerBound,xcurve,upperBound), 
                c(0,ycurve,0), 
                col="green3") 
      
      # Add the probability as text
        
        ## Calculate the area (probability)
        area=pnorm(upperBound,mu,sigma) - pnorm(lowerBound,mu,sigma)
        area2=round(area,4)
        
        ## Place this on the above plot
        text(12,0.02,substitute(paste("Prob. = ",area2 ), list(area2 = area2)))
      
      # Return stats about the Plot
      return(list(mu = mu, 
                  sigma = sigma,
                  lowerBound = lowerBound,
                  upperBound = upperBound,
                  prob = area2)) # this is the last line and will be released to the command line
      
    }
  
  # Call the above Functions
  obj = normalPDF(mu = 10, sigma = 5,
                  lowerBound = 8, upperBound = 20)
  
  obj
  
  # Get the probability from obj
  obj$prob
  
  
  # Plotting with ggplot
  # Continuous variables 
  if(!require(ggplot2)) install.packages("ggplot2")
  
  g = ggplot(NULL, aes(c(-10,10))) + 
    geom_area(stat = "function", 
              fun = dnorm, 
              args = list(mean = 0, sd = 3),
              fill = "red", xlim = c(-10, 0)) 
   
  g
  
  
