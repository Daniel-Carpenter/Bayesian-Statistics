
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

  


