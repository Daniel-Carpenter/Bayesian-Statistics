# FUNCTION - Make a GIBBS sampler for a Bivariate Normal ----------------------- 
  myngibbs <- function(n, mu_x, sd_x, mu_y, sd_y, rho) {
    
    # Create a Matrix to hold the Bivariate Normals for X and Y
    resultDistr <- matrix(ncol = 2, nrow = n)
    colnames(resultDistr) <- c("X","Y")
    
    # Now Calculate the Biv. Normals
    x <- 0
    y <- 0
    resultDistr[1, ] <- c(x, y)
    for (i in 2:n) {
      
      # Y|X = x ~ N(. . .)
      x <- rnorm(1, mu_y + rho*(sd_x/sd_y)*(y - mu_x), sqrt( sd_x^2)*(1 - rho^2) )
      y <- rnorm(1, mu_x + rho*(sd_y/sd_x)*(x - mu_y), sqrt( sd_y^2)*(1 - rho^2) )
      resultDistr[i, ] <- c(x, y)
    }
    
    # Give to output options for plotting ease
    require(tidyverse)
    df.matrixStyle <- as.data.frame(resultDistr)
    
    df.plotStyle <- df.matrixStyle %>%
      pivot_longer(cols      = c('X', 'Y'),
                   names_to  = 'variableName',
                   values_to = 'variableValue')
    
    # Return the Result Distribution for the Bivariate Normals
    return(list('matrixStyle' = df.matrixStyle, 
                'plotSyle'    = df.plotStyle,
                'stats' = list(n=n, 
                               mu_x=mu_x, sd_x=sd_x, 
                               mu_y=mu_y, sd_y=sd_y, rho=rho)))
  }

  
# PARAMETERS -------------------------------------------
  
  # Number of Iterations
  n <- 10000 
  set.seed(30)
  
  # Correlation Coefficient
  rho = 0.20
  
  # Values for Mean and Standard Deviation for Y and X
  mu_x = 1; sd_x = 8
  mu_y = 1; sd_y = 2
  
  
  
  
results <- myngibbs(n, mu_x, sd_x, mu_y, sd_y, rho)
head(results$matrixStyle)
head(results$plotSyle)


df.mat <- results$matrixStyle %>%
  mutate(idx = row_number())

df.plot <- results$plotSyle

require(ggplot2)

stats <- results$stats
calcCorr <- format(round(cor(df.mat$X, df.mat$Y),3), 3)
titleVar <- 'Bivariate Normal Distribution Created using MCMC'
subtitleVar <- paste('Iterations:', stats$n, 'Intended Correlation:', rho, 'Calculated Correlation:', calcCorr)


# Violin plots
ggplot(df.plot, aes(x=variableName, y=variableValue, fill=variableName)) +
  geom_violin() +
  scale_fill_brewer(palette='Pastel1') +
  labs(title = titleVar, subtitle = subtitleVar,
       x = 'The Distributions',
       y = 'Value within Distribution')

# Histogram and Density Plots
ggplot(df.plot, aes(x = variableValue, fill=variableName)) +
  geom_histogram(aes(y = ..density..)) +
  geom_density(alpha=0) +
  facet_wrap(nrow = 2, facets = vars(variableName)) + 
  scale_fill_brewer(palette='Pastel1') +
  labs(title = titleVar, subtitle = subtitleVar,
       x = 'Value within Distribution',
       y = 'Density')

slopeLine <- geom_smooth(method = 'lm', color = 'white')

# Base Scatter Plot of Y~X
baseScatter <- ggplot(df.mat, aes(X, Y)) + 
  labs(title = titleVar, subtitle = subtitleVar)
  
# Density of Bivariate Normal with COntours
baseScatter +                   
  geom_point(alpha = 0.2, color = 'steelblue') + 
  geom_density_2d(color='steelblue4', size=0.75, alpha=0.75) + 
  slopeLine + annotate(x=max(df.mat$X)-10, y=0, 
                       label=paste("Calculated\nCorrelation = ", calcCorr), 
                       geom="text", size=5, color = '#666666')
  
# Density of Bivariate Normal Raster Plot
corrAnnotation <- annotate(x=max(df.mat$X)-10, y=0, 
                           label=paste("Calculated\nCorrelation = ", calcCorr), 
                           geom="text", size=5, color = 'white')
baseScatter + 
  stat_density_2d(
  geom = "raster",
  aes(fill = after_stat(density)),
  contour = FALSE) + 
  scale_fill_viridis_c() +
  slopeLine + corrAnnotation