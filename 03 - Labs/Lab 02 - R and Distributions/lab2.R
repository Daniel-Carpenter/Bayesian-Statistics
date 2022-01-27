# PART I =======================================================================

  # Inputs -----------------------------------------------------------------------
    n = 10   # N Number of trials
    p = 0.5  # Probability of success 
    X = 0:10  # Number of Successes in N independent trials
  
  
  # Create function for discrete binomial distribution ---------------------------
    dmybin <- function(X, n, p) {
      
      # Change X to k to be consistent with textbook
      k = X
      
      # Calculate the binomial coefficient (N k)
      binomialCoefficient <- choose(n, X)
      
      # Return the discrete binomial calculation
      return(binomialCoefficient * p^k * (1 - p)^(n - k))
    }
  
  # Call and return Results of the function
    y.dmybin = dmybin(X, n=n, p=p)
    y.dmybin
    
    # Plot the distribution
    plot(X, y.dmybin)
    
    
  # Create a Cumulative Probability Function Called `pmybin` ---------------------
    pmybin <- function(dmybin, X, n, p) {
      
      # Return the Cumulative Probability
      return(sum(dmybin(X, n, p)))
    }
    
  # Call the Cumulative Probability Function `pmybin` and Return Results
    cumulativeProbability.pmybin <- pmybin(dmybin, X, n, p)
    cumulativeProbability.pmybin
  
  # Call and Return Results of Base R Binomial Distribution Function `pbinom`
    cumulativeProbability.pbinom <- pbinom(max(X), size=n, prob=p)
    cumulativeProbability.pbinom
    
    
  # Call and return Results of the Base R Binomial Distribution Function ---------
    y.dbinom = dbinom(X, size=n, prob=p)
    y.dbinom
    
    # Plot the distribution
    plot(X, y.dbinom)
    
    
  # Create a plot for Binomial Dist ----------------------------------------------
    xVar <- 0:n
    yVar <- dbinom(X, size=n, prob=p)
    
    plot(x = xVar, y = yVar, type = 'h', main = 'Daniel Carpenter - Binomial Distribution Plot')
  
    
# PART II - Using dstem, pstem, rstem, qstem ===================================
  
      # i. Find 𝑃(𝑋 = 4|𝜆 = 3) (What is the probability that there are exactly 4 successes when 3 is the average)
        dpois(x = 4, lambda = 3)
    
      # ii. Find 𝑃(𝑋 ≤ 4|𝜆 = 3) (What is the probability that there are 4 or less successes when 3 is the average) 
        ppois(q = 4, lambda = 3)
        
      # iii. Find 𝑃(𝑋 > 4|𝜆 = 3) (What is the probability that there are more than 4 successes when 3 is the average)
        ppois(q = 4, lambda = 3, lower.tail = FALSE)
      
      # iv. Find x so that 𝑃(𝑋 ≤ 𝑥|𝜆 = 3)= 0.9997077 
        qpois(p = 0.9997077, lambda = 3)
        
        ## Validation
        #ppois(q = qpois(p = 0.9997077, lambda = 3), lambda = 3)
        #qpois(p = 0.9999286, lambda = 3)
        
      # v. Create a sample of size 100 from a Poisson distribution that has parameter 𝜆 = 3. Store in an object.
        poissonSample3 <- rpois(n = 100, lambda = 3)
        
      # vi. Make a second sample of size 100 from a Poisson that has parameter 𝜆 = 6, store in an object
        poissonSample6 <- rpois(n = 100, lambda = 6)

      # Make boxplots of the random samples you made above. --------------------
        
        if(!require(tidyverse)) install.packages(tidyverse)
        
        # Create data frame with both samples
        df <- data.frame(Fst = poissonSample3,
                         Snd = poissonSample6) %>%
          
          # Pivot data into single column for ggplot use
          pivot_longer(cols      = c("Fst", "Snd"),
                       names_to  = "Sample",
                       values_to = "x")
        
        
        # Create a base Plot Object for future distribution graphs
        basePlot <- ggplot(df,
                           aes(x = Sample,
                               y = x,
                               fill = Sample)) + 
                    
                    # Color palette and theme
                    scale_fill_brewer(palette = "Pastel1") +
                    theme_minimal() +
                  
                    # Title
                    labs(title    = 'Sample Distribution Created from rpois Poisson Function',
                         subtitle = 'Sample Size (n) = 100 | Fst Lambda = 3; Snd Lamda = 6')
        
        # Create Box Plots
        basePlot + geom_boxplot()
                    
        # Create Violin Plots
        basePlot + geom_violin()
          
        # Create Histogram Plots
        ggplot(df) + 
          geom_histogram(aes(y = x, fill = Sample)) + 
          facet_wrap(~Sample) +
          theme_minimal()
        
        
        
        