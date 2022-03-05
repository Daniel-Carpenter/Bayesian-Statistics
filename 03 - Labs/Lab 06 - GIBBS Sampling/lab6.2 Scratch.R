# FUNCTION - Make a GIBBS sampler for a Bivariate Normal ----------------------- 
  myngibbs <- function(n, mu_x, sd_x, mu_y, sd_y, rho) {
    
    # Set a random seed
    set.seed(30)#round(runif(1)*100,0))
    
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
    
    # Convert Results distribution to a dataframe for manipulation ease
    df.mat <- as.data.frame(resultDistr)
    
    # Maked a pivoted df for plotting ease
    df.plot <- df.mat %>%
      pivot_longer(cols      = c('X', 'Y'),
                   names_to  = 'variableName',
                   values_to = 'variableValue') %>%
      mutate(iteration = row_number())
    
    
    # PLOTS ====================================================================
      
      require(ggplot2)
      
      # Create a simple theme for reuse
      myTheme <- theme_minimal() + theme(text = element_text(color = '#666666'),
                                         panel.grid.major = element_blank())
    
    
      # What is the correlation from the MCMC output? 
      calcCorr <- format(round(cor(df.mat$X, df.mat$Y),3), 3)
      
      # Create titles and subtitles for reuse later on
      titleVar <- 'Bivariate Normal Distribution Created using MCMC'
      
      stats <- results$stats
      subtitleVar <- paste('Iterations:', stats$n, 'Intended Correlation:', rho, 'Calculated Correlation:', calcCorr)
      
      
      # Violin plots -----------------------------------------------------------
      myViolin <- ggplot(df.plot, aes(x=variableName, y=variableValue, fill=variableName)) +
                    geom_violin() +
                    scale_fill_brewer(palette='Pastel1') +
                    labs(title = titleVar, subtitle = subtitleVar,
                         x = 'The Distributions',
                         y = 'Value within Distribution') + 
                    myTheme
      print(myViolin)
      
      # Histogram and Density Plots --------------------------------------------
      myHist <- ggplot(df.plot, aes(x = variableValue, fill=variableName)) +
                  geom_histogram(aes(y = ..density..)) +
                  geom_density(alpha=0) +
                  facet_wrap(nrow = 2, facets = vars(variableName)) + 
                  scale_fill_brewer(palette='Pastel1') +
                  labs(title = titleVar, subtitle = subtitleVar,
                       x = 'Value within Distribution',
                       y = 'Density') + 
                       myTheme
      print(myHist)
      
      # Base Scatter Plot of Y~X -----------------------------------------------
        
        # Create a slope object for reuse
        slopeLine <- geom_smooth(method = 'lm', color = '#666666') 
        
        # Create a slope object
        baseScatter <- ggplot(df.mat, aes(X, Y)) + 
          labs(title = titleVar, subtitle = subtitleVar)
      
      # Density of Bivariate Normal with COntours ------------------------------
      myContour <- baseScatter +                   
                    geom_point(alpha = 0.2, color = 'steelblue') + 
                    geom_density_2d(color='steelblue4', size=0.75, alpha=0.75) + 
                    slopeLine + annotate(x=max(df.mat$X)-10, y=0, 
                                         label=paste("Calculated\nCorrelation = ", calcCorr), 
                                         geom="text", size=5, color = '#666666') + 
                    myTheme
      print(myContour)
      
      # Density of Bivariate Normal Raster Plot --------------------------------
      slopeLine <- geom_smooth(method = 'lm', color = 'white')
      corrAnnotation <- annotate(x=max(df.mat$X)-10, y=0, 
                                 label=paste("Calculated\nCorrelation = ", calcCorr), 
                                 geom="text", size=5, color = 'white')
      myRaster <- baseScatter + 
                    stat_density_2d(
                      geom = "raster",
                      aes(fill = after_stat(density)),
                      contour = FALSE) + 
                    scale_fill_viridis_c() +
                    slopeLine + corrAnnotation + 
                    myTheme
      print(myRaster)
    
    # Return the Result Distribution for the Bivariate Normals and stats =======
    return(list('df.matrix' = df.mat, 
                'df.plot'    = df.plot,
                'stats' = list(n=n, 
                               mu_x=mu_x, sd_x=sd_x, 
                               mu_y=mu_y, sd_y=sd_y, 
                               rho=rho, rhoCalculated = calcCorr)))
  }

  
# PARAMETERS -------------------------------------------
  
  # Number of Iterations
  n <- 10000 
  
  # Correlation Coefficient
  rho = 0.20
  
  # Values for Mean and Standard Deviation for Y and X
  mu_x = 1; sd_x = 8
  mu_y = 1; sd_y = 2
  
# Call the function  
results <- myngibbs(n, mu_x, sd_x, mu_y, sd_y, rho)

# Show some data
head(results$df.matrix)
head(results$df.plot)
