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
  dec = 3 # num decimal places to round to
  calcCorr <- format(round(cor(df.mat$X, df.mat$Y), dec), nsmall=dec)
  
  # Create titles and subtitles for reuse later on
  titleVar <- 'Bivariate Normal Distribution Created using MCMC'
  
  stats <- list(n=n, 
                mu_x=mu_x, sd_x=sd_x, 
                mu_y=mu_y, sd_y=sd_y, 
                rho=rho, rhoCalculated = calcCorr)
  
  subtitleVar <- paste0('Iterations: ', format(stats$n, big.mark = ','), 
                        '; Intended Correlation: ', format(round(rho, dec), nsmall=dec), 
                        '; Calculated Correlation: ', calcCorr)
  captionVar <- paste0('Note on the Initial Variables:\n',
                       'mu[x] = ', mu_x,    ', mu[y] = ', mu_y,
                       '\nsigma[x] = ', sd_x, ', sigma[y] = ', sd_y)
  
  
  # Violin plots -----------------------------------------------------------
  myViolin <- ggplot(df.plot, aes(x=variableName, y=variableValue, fill=variableName)) +
    geom_violin() +
    scale_fill_brewer(palette='Pastel1') +
    labs(title = paste('Violin Plot |', titleVar), subtitle = subtitleVar,
         x = '',
         y = 'Estimated Outcome',
         caption = captionVar) + 
    myTheme
  print(myViolin)
  
  # Histogram and Density Plots --------------------------------------------
  myHist <- ggplot(df.plot, aes(x = variableValue, fill=variableName)) +
    geom_histogram(aes(y = ..density..)) +
    geom_density(alpha=0) +
    facet_wrap(nrow = 2, facets = vars(variableName)) + 
    scale_fill_brewer(palette='Pastel1') +
    labs(title = paste('Histogram |', titleVar), subtitle = subtitleVar,
         x = 'Estimated Outcome',
         y = 'Density',
         caption = captionVar) + 
    myTheme
  print(myHist)
  
  # Base Scatter Plot of Y~X -----------------------------------------------
  
  # Create a slope object for reuse
  slopeLine <- geom_smooth(method = 'lm', color = '#666666') 
  
  # Create a slope object
  baseScatter <- ggplot(df.mat, aes(X, Y)) + 
    labs(title = paste('Marginals |', titleVar), subtitle = subtitleVar,
         caption = captionVar)
  
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
  
  # Density of Bivariate Normal with COntours ------------------------------
  myContour <- baseScatter +                   
    geom_point(alpha = 0.2, color = 'steelblue') + 
    geom_density_2d(color='steelblue4', size=0.75, alpha=0.75) + 
    slopeLine + annotate(x=max(df.mat$X)-10, y=0, 
                         label=paste("Calculated\nCorrelation = ", calcCorr), 
                         geom="text", size=5, color = '#666666') + 
    myTheme
  print(myContour)
  
  myTrace <- ggplot(df.plot, aes(x=iteration, y=variableValue, color=variableName)) +
    geom_line(alpha=0.75) +
    scale_color_brewer(palette='Pastel1') +
    facet_wrap(nrow = 2, facets = vars(variableName)) +
    labs(title = paste('Trace Plot |', titleVar), subtitle = subtitleVar,
         x = 'Iteration of MCMC',
         y = 'Paramter Value',
         caption = captionVar) + 
    myTheme + theme(legend.text = element_blank(), legend.title = element_blank())
  print(myTrace)
  
  # Return the Result Distribution for the Bivariate Normals and stats =======
  return(list('df.matrix' = df.mat, 
              'df.plot'    = df.plot,
              'stats' = stats))
}


# PARAMETERS -------------------------------------------

# Number of Iterations
n <- 10000 

# Correlation Coefficient
rho = 0.20

# Values for Mean and Standard Deviation for Y and X
mu_x = 10; sd_x = 2
mu_y = 5;  sd_y = 4

# Call the function  
results <- myngibbs(n, mu_x, sd_x, mu_y, sd_y, rho)

# Initial Values 
results$stats

# Show Results of the data (only top few rows)
head(results$df.matrix)