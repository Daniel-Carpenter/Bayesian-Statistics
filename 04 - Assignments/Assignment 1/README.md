Assignment 1
================
Daniel Carpenter
February 2022

-   [Task `1`: Bayes Box & Pr/Lik/Post Plots
    Function](#task-1-bayes-box--prlikpost-plots-function)
    -   [`a.` Classical Point Estimate for 𝜃 (*Probability of
        Success*)](#a-classical-point-estimate-for-𝜃-probability-of-success)
    -   [`b.` Find classical 95% confidence interval using
        ![\\theta](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Ctheta "\theta")](#b-find-classical-95-confidence-interval-using-theta)
    -   [`c.` `mycoin()`: Bayes Box and Related
        Data](#c-mycoin-bayes-box-and-related-data)
-   [Task `2`: Variance Proof](#task-2-variance-proof)
-   [Task `3`: MGF Proof](#task-3-mgf-proof)
    -   [`i` Definition of The M.G.F of the Binomial
        Distribution:](#i-definition-of-the-mgf-of-the-binomial-distribution)
    -   [`ii. / iii.` Find the second moment using product
        rule:](#ii--iii-find-the-second-moment-using-product-rule)
-   [Task `4`: Normal Density Function](#task-4-normal-density-function)
    -   [Create Normal Density Function called
        `mynorm()`](#create-normal-density-function-called-mynorm)
    -   [`a.` Call Normal Density Function
        `mynorm()`](#a-call-normal-density-function-mynorm)
-   [Task `5`: Maximum Likelihood Proof and
    Function](#task-5-maximum-likelihood-proof-and-function)
    -   [`5.1.` Find
        ![\\hat{\\lambda}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Chat%7B%5Clambda%7D "\hat{\lambda}")
        as a formula](#51-find-hatlambda-as-a-formula)
    -   [`5.2.` Find the second derivative of
        ![L(\\lambda)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;L%28%5Clambda%29 "L(\lambda)")
        as a
        formula.](#52-find-the-second-derivative-of-llambda-as-a-formula)
    -   [`5.3/5.4.` Show
        ![\\hat{\\lambda}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Chat%7B%5Clambda%7D "\hat{\lambda}")
        is a maximum.](#5354-show-hatlambda-is-a-maximum)
    -   [`5.5.` Make a function called
        `myml(x)`](#55-make-a-function-called-mymlx)
    -   [`5.6.` MLE Does not Account for
        Priors](#56-mle-does-not-account-for-priors)
-   [Task `6`: Un/Biased
    ![\\lambda](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Clambda "\lambda")
    Proof](#task-6-unbiased-lambda-proof)
-   [Task `7`: Maximum Likelihood for *Bernoulli*
    Proof](#task-7-maximum-likelihood-for-bernoulli-proof)
-   [Task `8`: Newton Raphson Expression &
    Function](#task-8-newton-raphson-expression--function)
    -   [Newton Rasphson Expression](#newton-rasphson-expression)
    -   [Create `mynr()` Function](#create-mynr-function)
    -   [Call `mynr()` Function](#call-mynr-function)
-   [Task `9`: Maximum Likelihood Using Newton
    Raphson](#task-9-maximum-likelihood-using-newton-raphson)
    -   [Create `mynrml()` Function](#create-mynrml-function)
    -   [Call `mynrml()` Function](#call-mynrml-function)
-   [Task `10`: NR Function without Dervative as
    Parameter](#task-10-nr-function-without-dervative-as-parameter)
    -   [Create `mynrf()` Function](#create-mynrf-function)
    -   [Call `mynrf()` Function](#call-mynrf-function)

------------------------------------------------------------------------

# Task `1`: Bayes Box & Pr/Lik/Post Plots Function

## `a.` Classical Point Estimate for 𝜃 (*Probability of Success*)

``` r
# Function that creates a classical point est
classicalPointEst <- function(n) {
  theta <- seq(0, 1, length = n)
  
  # Point Estimate for Theta
  return(mean(theta))
}

n = 10 # Num Trials
classicalPointEst(n)
```

    ## [1] 0.5

## `b.` Find classical 95% confidence interval using ![\\theta](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Ctheta "\theta")

``` r
# Function for classical CI est at 95% confidence
classicalConfInt95 <- function(classicalPointEstFun = classicalPointEst, n) {
  # Get the classical point est
  pointEst <- classicalPointEstFun(n)
  
  # Return the classical interval
  return(pointEst + c(-1, 1) * 1.95 * sqrt(pointEst*(1 - pointEst) / n))
}
  
classicalConfInt95(classicalPointEst, n)
```

    ## [1] 0.1916779 0.8083221

## `c.` `mycoin()`: Bayes Box and Related Data

### Create the function `mycoin()`

``` r
mycoin <- mybinpost <- function(n, x, p, prior, alpha) {
  
  # CALCULATIONS -------------------------------------------
    
    ## Get the length of p
    numRows <- length(p)
  
    ## Calculate the likelihood
    likelihood  = dbinom(x=x, size=n, prob=p)
    
    ## Calculate the Prior x the Likelihood
    h <-  prior * likelihood
    
    ## Get the posterior Distribution
    posterior = h / sum(h) 
    
    ## Consolidate into a matrix with row and column names
    bayesMatrix <- matrix(c(p, prior, likelihood, h, posterior), 
                        nr = numRows, nc = 5, byrow = FALSE)
    colnames(bayesMatrix) <- c("p", "prior", "likelihood", "h", "posterior")
    rownames(bayesMatrix)= c(rep("", numRows))
    rbind(bayesMatrix, colSums(bayesMatrix)) # Column totals

        
  # PLOTTING -----------------------------------------------
    
    ## Define some colors for the later plots
      red   = 'tomato3'
      blue  = 'steelblue'
      green = 'darkseagreen'
      colorPalette <- c(blue, green, red) # Consolidate in color palette
    
    ## Number of Theta numbers to plot
    thetaNumericValues = 1:numRows
    
    ## Convert to data frame for ggplot
    df <- as.data.frame(cbind(bayesMatrix, thetaNumericValues))
    
    
    if(!require(tidyverse)) install.packages(tidyverse)
    df <- df %>%
      
      ### Select to only the needed data
      select(-h) %>%
      
      ### Pivot y axis variables into single column for ggplot-ing
      pivot_longer(cols      = c("prior", "likelihood", "posterior"),
                   values_to = "values",
                   names_to  = "statNames")
    
    ## Plot it!
    basePlot <- ggplot(df,
                       aes(x = thetaNumericValues,
                           y = values,
                           color = statNames)) +
      ### Theme and colors
      theme_minimal() +
      scale_color_manual(values = colorPalette) +
      
      ### Labels
      labs(title = "Prior, Likelihood, Posterior over the Discrete Values of Theta",
           subtitle = paste0("Daniel Carpenter | x = ", x, ", n = ", n),
           x = 'Number of Theta Values', y = 'Probability')+ 
      
      ### Create the points
      geom_point()
    
    # Output Plot
    print(basePlot)
    
    
  # BCI & Point Est ------------------------------------------
    
    ## BCI
    cp = cumsum(posterior) # cumulative sum
    L = max(which(cp<alpha/2)) # this gives the max index where  cp < alpha/2
    U = min(which(cp > 1-alpha/2))
    BCI = df$p[c(L,U)] # close to the desired BCI
    
    ## Bayesian point estimate is the posterior mean
    bayesPointEst <- mean(posterior)
    
    
  # CLasical Point Est and Interval 95% CI ----------------
      classicalPointEstimate <- classicalPointEst(n)    
      cCI95 <- classicalConfInt95(classicalPointEst, n)

          
  # RETURN DATA ---------------------------------------------
      
    # Create a folder for the Output to stay organized
    outputFolder <- 'Output/'
    dir.create(outputFolder)
    
    # Create the folder for this task
    task1Folder  <- paste0(outputFolder, 'Task_01/')
    dir.create(task1Folder)
    print(paste0('Please find the Output Files located at ', task1Folder))
    
    ## File name for writing data to current wd
    nameOfFile <- paste("", sum(p),sum(prior),n,x,alpha, sep = "_") # Used this name since unique identifier
    
    ## Write a CSV  
    write.csv(x = as.data.frame(bayesMatrix), 
              file = paste0(task1Folder, "BayesBox", nameOfFile, ".csv"))
      
    ## Write above plot to jpg
    ggsave(filename = paste0(task1Folder, "BayesPlot", nameOfFile, ".jpg"),
           plot     = basePlot,
           height   = 8.5,
           width    = 11)
    
    ## Used for latex output of a matrix
    if(!require(xtable)) install.packages(xtable)
    
    ## Return a list of relevant data
    return(list('bayesMatrix'      = bayesMatrix,
                'bayesPointEst'    = bayesPointEst,
                'bayesCredIntvl95' = BCI,
                'classicalPEst'    = classicalPointEstimate,
                'classicalCI95'    = cCI95))
}
```

### Call the function `mycoin()` with 3 sets of inputs

``` r
mycoin(p = seq(0,1,length=20), prior =rep(1/20, 20), n=10, x=4, alpha = 0.05)
```

    ## [1] "Please find the Output Files located at Output/Task_01/"

![](assignment1_files/figure-gfm/1cii-1.png)<!-- -->

    ## $bayesMatrix
    ##           p prior   likelihood            h    posterior
    ##  0.00000000  0.05 0.000000e+00 0.000000e+00 0.000000e+00
    ##  0.05263158  0.05 1.164980e-03 5.824898e-05 6.744611e-04
    ##  0.10526316  0.05 1.322808e-02 6.614040e-04 7.658353e-03
    ##  0.15789474  0.05 4.654663e-02 2.327331e-03 2.694802e-02
    ##  0.21052632  0.05 9.987822e-02 4.993911e-03 5.782417e-02
    ##  0.26315789  0.05 1.611876e-01 8.059378e-03 9.331900e-02
    ##  0.31578947  0.05 2.142636e-01 1.071318e-02 1.240472e-01
    ##  0.36842105  0.05 2.455630e-01 1.227815e-02 1.421679e-01
    ##  0.42105263  0.05 2.485417e-01 1.242709e-02 1.438924e-01
    ##  0.47368421  0.05 2.247260e-01 1.123630e-02 1.301044e-01
    ##  0.52631579  0.05 1.820281e-01 9.101403e-03 1.053845e-01
    ##  0.57894737  0.05 1.314601e-01 6.573005e-03 7.610839e-02
    ##  0.63157895  0.05 8.355963e-02 4.177982e-03 4.837657e-02
    ##  0.68421053  0.05 4.564195e-02 2.282098e-03 2.642426e-02
    ##  0.73684211  0.05 2.055964e-02 1.027982e-03 1.190293e-02
    ##  0.78947368  0.05 7.102451e-03 3.551226e-04 4.111941e-03
    ##  0.84210526  0.05 1.636405e-03 8.182024e-05 9.473912e-04
    ##  0.89473684  0.05 1.830876e-04 9.154380e-06 1.059980e-04
    ##  0.94736842  0.05 3.595616e-06 1.797808e-07 2.081670e-06
    ##  1.00000000  0.05 0.000000e+00 0.000000e+00 0.000000e+00
    ## 
    ## $bayesPointEst
    ## [1] 0.05
    ## 
    ## $bayesCredIntvl95
    ## [1] 0.0000000 0.2105263
    ## 
    ## $classicalPEst
    ## [1] 0.5
    ## 
    ## $classicalCI95
    ## [1] 0.1916779 0.8083221

``` r
mycoin(p = seq(0,1,length=40), prior =rep(1/40, 40), n=10, x=4, alpha = 0.05)
```

    ## [1] "Please find the Output Files located at Output/Task_01/"

![](assignment1_files/figure-gfm/1cii-2.png)<!-- -->

    ## $bayesMatrix
    ##           p prior   likelihood            h    posterior
    ##  0.00000000 0.025 0.000000e+00 0.000000e+00 0.000000e+00
    ##  0.02564103 0.025 7.767385e-05 1.941846e-06 2.190801e-05
    ##  0.05128205 0.025 1.059019e-03 2.647546e-05 2.986975e-04
    ##  0.07692308 0.025 4.548553e-03 1.137138e-04 1.282925e-03
    ##  0.10256410 0.025 1.214008e-02 3.035019e-04 3.424124e-03
    ##  0.12820513 0.025 2.490731e-02 6.226826e-04 7.025137e-03
    ##  0.15384615 0.025 4.317793e-02 1.079448e-03 1.217839e-02
    ##  0.17948718 0.025 6.650668e-02 1.662667e-03 1.875829e-02
    ##  0.20512821 0.025 9.377851e-02 2.344463e-03 2.645035e-02
    ##  0.23076923 0.025 1.233874e-01 3.084685e-03 3.480157e-02
    ##  0.25641026 0.025 1.534480e-01 3.836200e-03 4.328020e-02
    ##  0.28205128 0.025 1.820087e-01 4.550218e-03 5.133579e-02
    ##  0.30769231 0.025 2.072434e-01 5.181086e-03 5.845328e-02
    ##  0.33333333 0.025 2.276076e-01 5.690190e-03 6.419701e-02
    ##  0.35897436 0.025 2.419499e-01 6.048747e-03 6.824227e-02
    ##  0.38461538 0.025 2.495777e-01 6.239442e-03 7.039370e-02
    ##  0.41025641 0.025 2.502766e-01 6.256914e-03 7.059082e-02
    ##  0.43589744 0.025 2.442896e-01 6.107240e-03 6.890220e-02
    ##  0.46153846 0.025 2.322625e-01 5.806561e-03 6.550992e-02
    ##  0.48717949 0.025 2.151630e-01 5.379075e-03 6.068700e-02
    ##  0.51282051 0.025 1.941846e-01 4.854615e-03 5.477002e-02
    ##  0.53846154 0.025 1.706418e-01 4.266045e-03 4.812974e-02
    ##  0.56410256 0.025 1.458671e-01 3.646679e-03 4.114201e-02
    ##  0.58974359 0.025 1.211168e-01 3.027921e-03 3.416115e-02
    ##  0.61538462 0.025 9.749127e-02 2.437282e-03 2.749754e-02
    ##  0.64102564 0.025 7.587548e-02 1.896887e-03 2.140078e-02
    ##  0.66666667 0.025 5.690190e-02 1.422547e-03 1.604925e-02
    ##  0.69230769 0.025 4.093698e-02 1.023424e-03 1.154633e-02
    ##  0.71794872 0.025 2.809063e-02 7.022657e-04 7.922998e-03
    ##  0.74358974 0.025 1.824590e-02 4.561474e-04 5.146279e-03
    ##  0.76923077 0.025 1.110487e-02 2.776216e-04 3.132141e-03
    ##  0.79487179 0.025 6.245395e-03 1.561349e-04 1.761522e-03
    ##  0.82051282 0.025 3.182448e-03 7.956121e-05 8.976137e-04
    ##  0.84615385 0.025 1.427370e-03 3.568424e-05 4.025915e-04
    ##  0.87179487 0.025 5.386528e-04 1.346632e-05 1.519277e-04
    ##  0.89743590 0.025 1.585643e-04 3.964107e-06 4.472326e-05
    ##  0.92307692 0.025 3.158717e-05 7.896793e-07 8.909202e-06
    ##  0.94871795 0.025 3.094283e-06 7.735709e-08 8.727466e-07
    ##  0.97435897 0.025 5.379075e-08 1.344769e-09 1.517175e-08
    ##  1.00000000 0.025 0.000000e+00 0.000000e+00 0.000000e+00
    ## 
    ## $bayesPointEst
    ## [1] 0.025
    ## 
    ## $bayesCredIntvl95
    ## [1] 0.05128205 0.23076923
    ## 
    ## $classicalPEst
    ## [1] 0.5
    ## 
    ## $classicalCI95
    ## [1] 0.1916779 0.8083221

``` r
mycoin(p = seq(0,1,length=20), prior =rep(1/20, 20), n=10, x=4, alpha = 0.1)
```

    ## [1] "Please find the Output Files located at Output/Task_01/"

![](assignment1_files/figure-gfm/1cii-3.png)<!-- -->

    ## $bayesMatrix
    ##           p prior   likelihood            h    posterior
    ##  0.00000000  0.05 0.000000e+00 0.000000e+00 0.000000e+00
    ##  0.05263158  0.05 1.164980e-03 5.824898e-05 6.744611e-04
    ##  0.10526316  0.05 1.322808e-02 6.614040e-04 7.658353e-03
    ##  0.15789474  0.05 4.654663e-02 2.327331e-03 2.694802e-02
    ##  0.21052632  0.05 9.987822e-02 4.993911e-03 5.782417e-02
    ##  0.26315789  0.05 1.611876e-01 8.059378e-03 9.331900e-02
    ##  0.31578947  0.05 2.142636e-01 1.071318e-02 1.240472e-01
    ##  0.36842105  0.05 2.455630e-01 1.227815e-02 1.421679e-01
    ##  0.42105263  0.05 2.485417e-01 1.242709e-02 1.438924e-01
    ##  0.47368421  0.05 2.247260e-01 1.123630e-02 1.301044e-01
    ##  0.52631579  0.05 1.820281e-01 9.101403e-03 1.053845e-01
    ##  0.57894737  0.05 1.314601e-01 6.573005e-03 7.610839e-02
    ##  0.63157895  0.05 8.355963e-02 4.177982e-03 4.837657e-02
    ##  0.68421053  0.05 4.564195e-02 2.282098e-03 2.642426e-02
    ##  0.73684211  0.05 2.055964e-02 1.027982e-03 1.190293e-02
    ##  0.78947368  0.05 7.102451e-03 3.551226e-04 4.111941e-03
    ##  0.84210526  0.05 1.636405e-03 8.182024e-05 9.473912e-04
    ##  0.89473684  0.05 1.830876e-04 9.154380e-06 1.059980e-04
    ##  0.94736842  0.05 3.595616e-06 1.797808e-07 2.081670e-06
    ##  1.00000000  0.05 0.000000e+00 0.000000e+00 0.000000e+00
    ## 
    ## $bayesPointEst
    ## [1] 0.05
    ## 
    ## $bayesCredIntvl95
    ## [1] 0.05263158 0.21052632
    ## 
    ## $classicalPEst
    ## [1] 0.5
    ## 
    ## $classicalCI95
    ## [1] 0.1916779 0.8083221

``` r
# Assume Prior equaling the length of the p
pr =rep(1/40, 40)
mycoin(p = seq(0,1,length=40), prior = pr, n =10, x=4, alpha=0.05)
```

    ## [1] "Please find the Output Files located at Output/Task_01/"

![](assignment1_files/figure-gfm/1cii-4.png)<!-- -->

    ## $bayesMatrix
    ##           p prior   likelihood            h    posterior
    ##  0.00000000 0.025 0.000000e+00 0.000000e+00 0.000000e+00
    ##  0.02564103 0.025 7.767385e-05 1.941846e-06 2.190801e-05
    ##  0.05128205 0.025 1.059019e-03 2.647546e-05 2.986975e-04
    ##  0.07692308 0.025 4.548553e-03 1.137138e-04 1.282925e-03
    ##  0.10256410 0.025 1.214008e-02 3.035019e-04 3.424124e-03
    ##  0.12820513 0.025 2.490731e-02 6.226826e-04 7.025137e-03
    ##  0.15384615 0.025 4.317793e-02 1.079448e-03 1.217839e-02
    ##  0.17948718 0.025 6.650668e-02 1.662667e-03 1.875829e-02
    ##  0.20512821 0.025 9.377851e-02 2.344463e-03 2.645035e-02
    ##  0.23076923 0.025 1.233874e-01 3.084685e-03 3.480157e-02
    ##  0.25641026 0.025 1.534480e-01 3.836200e-03 4.328020e-02
    ##  0.28205128 0.025 1.820087e-01 4.550218e-03 5.133579e-02
    ##  0.30769231 0.025 2.072434e-01 5.181086e-03 5.845328e-02
    ##  0.33333333 0.025 2.276076e-01 5.690190e-03 6.419701e-02
    ##  0.35897436 0.025 2.419499e-01 6.048747e-03 6.824227e-02
    ##  0.38461538 0.025 2.495777e-01 6.239442e-03 7.039370e-02
    ##  0.41025641 0.025 2.502766e-01 6.256914e-03 7.059082e-02
    ##  0.43589744 0.025 2.442896e-01 6.107240e-03 6.890220e-02
    ##  0.46153846 0.025 2.322625e-01 5.806561e-03 6.550992e-02
    ##  0.48717949 0.025 2.151630e-01 5.379075e-03 6.068700e-02
    ##  0.51282051 0.025 1.941846e-01 4.854615e-03 5.477002e-02
    ##  0.53846154 0.025 1.706418e-01 4.266045e-03 4.812974e-02
    ##  0.56410256 0.025 1.458671e-01 3.646679e-03 4.114201e-02
    ##  0.58974359 0.025 1.211168e-01 3.027921e-03 3.416115e-02
    ##  0.61538462 0.025 9.749127e-02 2.437282e-03 2.749754e-02
    ##  0.64102564 0.025 7.587548e-02 1.896887e-03 2.140078e-02
    ##  0.66666667 0.025 5.690190e-02 1.422547e-03 1.604925e-02
    ##  0.69230769 0.025 4.093698e-02 1.023424e-03 1.154633e-02
    ##  0.71794872 0.025 2.809063e-02 7.022657e-04 7.922998e-03
    ##  0.74358974 0.025 1.824590e-02 4.561474e-04 5.146279e-03
    ##  0.76923077 0.025 1.110487e-02 2.776216e-04 3.132141e-03
    ##  0.79487179 0.025 6.245395e-03 1.561349e-04 1.761522e-03
    ##  0.82051282 0.025 3.182448e-03 7.956121e-05 8.976137e-04
    ##  0.84615385 0.025 1.427370e-03 3.568424e-05 4.025915e-04
    ##  0.87179487 0.025 5.386528e-04 1.346632e-05 1.519277e-04
    ##  0.89743590 0.025 1.585643e-04 3.964107e-06 4.472326e-05
    ##  0.92307692 0.025 3.158717e-05 7.896793e-07 8.909202e-06
    ##  0.94871795 0.025 3.094283e-06 7.735709e-08 8.727466e-07
    ##  0.97435897 0.025 5.379075e-08 1.344769e-09 1.517175e-08
    ##  1.00000000 0.025 0.000000e+00 0.000000e+00 0.000000e+00
    ## 
    ## $bayesPointEst
    ## [1] 0.025
    ## 
    ## $bayesCredIntvl95
    ## [1] 0.05128205 0.23076923
    ## 
    ## $classicalPEst
    ## [1] 0.5
    ## 
    ## $classicalCI95
    ## [1] 0.1916779 0.8083221

------------------------------------------------------------------------

<br>

# Task `2`: Variance Proof

> *THIS SHOWS AN EXAMPLE, SEE PART `ii/iii` OF `TASK 3` FOR FINAL PROOF
> OF
> ![V(X)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;V%28X%29 "V(X)")*

**Inputs**

``` r
n = 1000  # Intervals
p = 0.5   # Probability
```

**Variance using raw Definition**
![V(X) = \\frac{E(X - \\mu)^2}{n}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;V%28X%29%20%3D%20%5Cfrac%7BE%28X%20-%20%5Cmu%29%5E2%7D%7Bn%7D "V(X) = \frac{E(X - \mu)^2}{n}")

``` r
varMethod1 <- function(n, p) {
  # Calculate the binomial distribution using parameters n, p, and q
  mu = n * p
  X  = pbinom(q=1-p, size=n, prob=p)
  
  # Calculate the variance of X using above formula 
  variance = (X - mu)^2 / n
  return(format(round(variance, 1), nsmall=1))
}

varMethod1(n, p)
```

    ## [1] "250.0"

**Verify**
![n \\times p \\times q](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;n%20%5Ctimes%20p%20%5Ctimes%20q "n \times p \times q")
**is the above variance**

``` r
varMethod2 <- function(n, p) {
  # Calculate q
  q = 1 - p
  
  # Calculate the variance
  variance2 <- n*p*q
  return(variance2)
}

varMethod2(n, p)
```

    ## [1] 250

------------------------------------------------------------------------

<br>

# Task `3`: MGF Proof

## `i` Definition of The M.G.F of the Binomial Distribution:

![
b(x ; n, p)=\\frac{n !}{x !(n-x) !} p^{x} q^{n-x} \\quad \\text { with } \\quad q=1-p .
](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0Ab%28x%20%3B%20n%2C%20p%29%3D%5Cfrac%7Bn%20%21%7D%7Bx%20%21%28n-x%29%20%21%7D%20p%5E%7Bx%7D%20q%5E%7Bn-x%7D%20%5Cquad%20%5Ctext%20%7B%20with%20%7D%20%5Cquad%20q%3D1-p%20.%0A "
b(x ; n, p)=\frac{n !}{x !(n-x) !} p^{x} q^{n-x} \quad \text { with } \quad q=1-p .
")

MGF Given by:

![
\\begin{aligned}
M(x, t) &=\\sum\_{x=0}^{n} e^{x t} \\frac{n !}{x !(n-x) !} p^{x} q^{n-x} \\\\
&=\\sum\_{x=0}^{n} \\frac{n !}{x !(n-x) !}\\left(p e^{t}\\right)^{x} q^{n-x} \\\\
&=\\left(p e^{t}+q\\right)^{n}
\\end{aligned}
](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0A%5Cbegin%7Baligned%7D%0AM%28x%2C%20t%29%20%26%3D%5Csum_%7Bx%3D0%7D%5E%7Bn%7D%20e%5E%7Bx%20t%7D%20%5Cfrac%7Bn%20%21%7D%7Bx%20%21%28n-x%29%20%21%7D%20p%5E%7Bx%7D%20q%5E%7Bn-x%7D%20%5C%5C%0A%26%3D%5Csum_%7Bx%3D0%7D%5E%7Bn%7D%20%5Cfrac%7Bn%20%21%7D%7Bx%20%21%28n-x%29%20%21%7D%5Cleft%28p%20e%5E%7Bt%7D%5Cright%29%5E%7Bx%7D%20q%5E%7Bn-x%7D%20%5C%5C%0A%26%3D%5Cleft%28p%20e%5E%7Bt%7D%2Bq%5Cright%29%5E%7Bn%7D%0A%5Cend%7Baligned%7D%0A "
\begin{aligned}
M(x, t) &=\sum_{x=0}^{n} e^{x t} \frac{n !}{x !(n-x) !} p^{x} q^{n-x} \\
&=\sum_{x=0}^{n} \frac{n !}{x !(n-x) !}\left(p e^{t}\right)^{x} q^{n-x} \\
&=\left(p e^{t}+q\right)^{n}
\end{aligned}
")

Differentiate the MGF with respect to
![t](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;t "t")
using the function-of-a-function rule:

![
\\begin{aligned}
\\frac{d M(x, t)}{d t} &=n\\left(q+p e^{t}\\right)^{n-1} p e^{t} \\\\
&=n p e^{t}\\left(p e^{t}+q\\right)^{n-1}
\\end{aligned}
](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0A%5Cbegin%7Baligned%7D%0A%5Cfrac%7Bd%20M%28x%2C%20t%29%7D%7Bd%20t%7D%20%26%3Dn%5Cleft%28q%2Bp%20e%5E%7Bt%7D%5Cright%29%5E%7Bn-1%7D%20p%20e%5E%7Bt%7D%20%5C%5C%0A%26%3Dn%20p%20e%5E%7Bt%7D%5Cleft%28p%20e%5E%7Bt%7D%2Bq%5Cright%29%5E%7Bn-1%7D%0A%5Cend%7Baligned%7D%0A "
\begin{aligned}
\frac{d M(x, t)}{d t} &=n\left(q+p e^{t}\right)^{n-1} p e^{t} \\
&=n p e^{t}\left(p e^{t}+q\right)^{n-1}
\end{aligned}
")

Now use
![t=0](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;t%3D0 "t=0")
to get
![E(x)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;E%28x%29 "E(x)")

![
E(x)=n p(p+q)^{n-1}=n p .
](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0AE%28x%29%3Dn%20p%28p%2Bq%29%5E%7Bn-1%7D%3Dn%20p%20.%0A "
E(x)=n p(p+q)^{n-1}=n p .
")

## `ii. / iii.` Find the second moment using product rule:

![
\\frac{d u v}{d x}=u \\frac{d v}{d x}+v \\frac{d u}{d x}
](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0A%5Cfrac%7Bd%20u%20v%7D%7Bd%20x%7D%3Du%20%5Cfrac%7Bd%20v%7D%7Bd%20x%7D%2Bv%20%5Cfrac%7Bd%20u%7D%7Bd%20x%7D%0A "
\frac{d u v}{d x}=u \frac{d v}{d x}+v \frac{d u}{d x}
")

![
\\begin{aligned}
\\frac{d^{2} M(x, t)}{d t^{2}} &=n p e^{t}\\left\\{(n-1)\\left(p e^{t}+q\\right)^{n-2} p e^{t}\\right\\}+\\left(p e^{t}+q\\right)^{n-1}\\left\\{n p e^{t}\\right\\} \\\\
&=n p e^{t}\\left(p e^{t}+q\\right)^{n-2}\\left\\{(n-1) p e^{t}+\\left(p e^{t}+q\\right)\\right\\} \\\\
&=n p e^{t}\\left(p e^{t}+q\\right)^{n-2}\\left\\{q+n p e^{t}\\right\\} .
\\end{aligned}
](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0A%5Cbegin%7Baligned%7D%0A%5Cfrac%7Bd%5E%7B2%7D%20M%28x%2C%20t%29%7D%7Bd%20t%5E%7B2%7D%7D%20%26%3Dn%20p%20e%5E%7Bt%7D%5Cleft%5C%7B%28n-1%29%5Cleft%28p%20e%5E%7Bt%7D%2Bq%5Cright%29%5E%7Bn-2%7D%20p%20e%5E%7Bt%7D%5Cright%5C%7D%2B%5Cleft%28p%20e%5E%7Bt%7D%2Bq%5Cright%29%5E%7Bn-1%7D%5Cleft%5C%7Bn%20p%20e%5E%7Bt%7D%5Cright%5C%7D%20%5C%5C%0A%26%3Dn%20p%20e%5E%7Bt%7D%5Cleft%28p%20e%5E%7Bt%7D%2Bq%5Cright%29%5E%7Bn-2%7D%5Cleft%5C%7B%28n-1%29%20p%20e%5E%7Bt%7D%2B%5Cleft%28p%20e%5E%7Bt%7D%2Bq%5Cright%29%5Cright%5C%7D%20%5C%5C%0A%26%3Dn%20p%20e%5E%7Bt%7D%5Cleft%28p%20e%5E%7Bt%7D%2Bq%5Cright%29%5E%7Bn-2%7D%5Cleft%5C%7Bq%2Bn%20p%20e%5E%7Bt%7D%5Cright%5C%7D%20.%0A%5Cend%7Baligned%7D%0A "
\begin{aligned}
\frac{d^{2} M(x, t)}{d t^{2}} &=n p e^{t}\left\{(n-1)\left(p e^{t}+q\right)^{n-2} p e^{t}\right\}+\left(p e^{t}+q\right)^{n-1}\left\{n p e^{t}\right\} \\
&=n p e^{t}\left(p e^{t}+q\right)^{n-2}\left\{(n-1) p e^{t}+\left(p e^{t}+q\right)\right\} \\
&=n p e^{t}\left(p e^{t}+q\right)^{n-2}\left\{q+n p e^{t}\right\} .
\end{aligned}
")

Use
![t=0](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;t%3D0 "t=0")
again:

![
\\begin{aligned}
E\\left(x^{2}\\right) &=n p(p+q)^{n-2}(n p+q) \\\\
&=n p(n p+q)
\\end{aligned}
](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0A%5Cbegin%7Baligned%7D%0AE%5Cleft%28x%5E%7B2%7D%5Cright%29%20%26%3Dn%20p%28p%2Bq%29%5E%7Bn-2%7D%28n%20p%2Bq%29%20%5C%5C%0A%26%3Dn%20p%28n%20p%2Bq%29%0A%5Cend%7Baligned%7D%0A "
\begin{aligned}
E\left(x^{2}\right) &=n p(p+q)^{n-2}(n p+q) \\
&=n p(n p+q)
\end{aligned}
")

**Finally we now have derived
![V(X)=E(X-\\mu)^{2}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;V%28X%29%3DE%28X-%5Cmu%29%5E%7B2%7D "V(X)=E(X-\mu)^{2}")
(`i`) AND that the variance of
![X](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;X "X")
is
![n p q](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;n%20p%20q "n p q")
where
![q=1-p](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;q%3D1-p "q=1-p")
(`ii.`).**

![
\\begin{aligned}
V(x) &=E\\left(x^{2}\\right)-\\{E(x)\\}^{2} \\\\
&=n p(n p+q)-n^{2} p^{2} \\\\
&=n p q
\\end{aligned}
](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0A%5Cbegin%7Baligned%7D%0AV%28x%29%20%26%3DE%5Cleft%28x%5E%7B2%7D%5Cright%29-%5C%7BE%28x%29%5C%7D%5E%7B2%7D%20%5C%5C%0A%26%3Dn%20p%28n%20p%2Bq%29-n%5E%7B2%7D%20p%5E%7B2%7D%20%5C%5C%0A%26%3Dn%20p%20q%0A%5Cend%7Baligned%7D%0A "
\begin{aligned}
V(x) &=E\left(x^{2}\right)-\{E(x)\}^{2} \\
&=n p(n p+q)-n^{2} p^{2} \\
&=n p q
\end{aligned}
")

------------------------------------------------------------------------

<br>

# Task `4`: Normal Density Function

## Create Normal Density Function called `mynorm()`

``` r
    mynorm <- function(mu, sigma, 
                       lowerBound = NA, upperBound = NA, 
                       alpha,
                       roundTo = 4, 
                       color = paste0("lightsteelblue", floor(runif(1, min=1, max=4))),
                       returnCMD = TRUE)
    {
      
      # Calculate the "xlim" lower and upper bound for the Normal PDF Curve
      curveLowerBound <- mu - 3*sigma
      curveUpperBound <- mu + 3*sigma
      
      # Initialize variables related to output and graph
      title <- ""   # Title of graph
      exactProb = 0 # The exact probability of the questions
      
      # If no provided LOWER AND UPPER Bound (NA as parameter value) then assume none
      if (!(is.na(lowerBound)) & !(is.na(upperBound))) {
        title <- paste0(", P(", lowerBound, " <= X < ",upperBound,")")
        exactProb = pnorm(upperBound, mu, sigma) - pnorm(lowerBound, mu, sigma) # calculate prob 
        
      # If no provided LOWER Bound (NA as parameter value) then assume none
      } else if (is.na(lowerBound)) {
        lowerBound = curveLowerBound
        title <- paste0(", P(X < ",upperBound,")") # Set a dynamic title
        exactProb = pnorm(upperBound, mu, sigma) # calculate prob 
        
      # If no provided UPPER Bound (NA as parameter value) then assume none
      } else if(is.na(upperBound)) {
        upperBound = curveUpperBound
        title <- paste0(", P(X >= ",lowerBound,")")
        exactProb = 1 - pnorm(lowerBound, mu, sigma) # calculate prob 
      }
      
      # Create the line that displays the bell curve (between the CURVE bounds defined above)
      curve(
        
        ## Normally Distributed
        dnorm(x,mu,sigma), 
        
        ## Normally Distributed
        xlim=c(curveLowerBound, curveUpperBound), 
        
        ## Line width
        lwd =2, 
        
        ## Title with descriptive characteristics about function parameters
        main = paste0("Probability Distribution (by Daniel Carpenter)\n",
                      "X ~ N(",mu,", ",sigma,")", title),
        
        ## X and Y labels
        ylab = 'Probability Density',
        xlab = 'Value of X',
      )

      
      # Add the AREA of between the lower and upper bound P(lowerBound<X<=upperBound)
      
        ## X-Axis curve (length does not matter)
        xcurve = seq(lowerBound,upperBound, length=1000)
        
        ## Y-Axis Curve
        ycurve = dnorm(xcurve, mu,sigma)
        
        ## Combine the X and Y curve to form the area (in green)
        polygon(c(lowerBound, xcurve, upperBound), 
                c(0, ycurve, 0), 
                col=color) 
        
        ## Legend
        legend("topleft", legend="Area (of Prob. Density)", 
               fill=color, bty = "n")
      
      # Add the probability as text
        
        ## Calculate the area (probability)
        area = exactProb
        areaRounded = format(round(area, roundTo), nsmall=roundTo)
        
        ## Place this on the above plot
        text(12,0.02,substitute(paste("Probability = ", areaRounded), 
                                list(areaRounded = areaRounded)))
        
      # Quantiles
        
        ## Alpha (Lower Tail)
        lowerTail = qnorm(p = alpha / 2, 
                          mean = mu, sd = sigma,
                          lower.tail = TRUE)
        
        ## 1 minus alpha (upper tail)
        upperTail = qnorm(p = alpha / 2, 
                          mean = mu, sd = sigma,
                          lower.tail = FALSE)
        
      # Return stats about the Plot
      if (returnCMD) {
        return(list(shadedArea = areaRounded,
                    lowerTail = lowerTail,
                    upperTail = upperTail))
      }
    }
```

## `a.` Call Normal Density Function `mynorm()`

``` r
mynorm(mu=10, sigma=8, 
       lowerBound = 8, upperBound = 11,
       alpha = 0.10)
```

![](assignment1_files/figure-gfm/2a-1.png)<!-- -->

    ## $shadedArea
    ## [1] "0.1484"
    ## 
    ## $lowerTail
    ## [1] -3.158829
    ## 
    ## $upperTail
    ## [1] 23.15883

------------------------------------------------------------------------

<br>

# Task `5`: Maximum Likelihood Proof and Function

## `5.1.` Find ![\\hat{\\lambda}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Chat%7B%5Clambda%7D "\hat{\lambda}") as a formula

-   Do this by first finding the likelihood function which is shown as
    ![L(\\lambda)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;L%28%5Clambda%29 "L(\lambda)")

    ![
    L(\\lambda)=\\prod\_{i=1}^{n} f\_{X}\\left(x\_{i} ; \\lambda\\right)=\\prod\_{i=1}^{n}\\left\\{\\frac{\\lambda^{x\_{i}}}{x\_{i} !} e^{-\\lambda}\\right\\}=\\frac{\\lambda^{x\_{1}+\\ldots+x\_{n}}}{x\_{1} ! \\ldots x\_{n} !} e^{-n \\lambda}
    ](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0AL%28%5Clambda%29%3D%5Cprod_%7Bi%3D1%7D%5E%7Bn%7D%20f_%7BX%7D%5Cleft%28x_%7Bi%7D%20%3B%20%5Clambda%5Cright%29%3D%5Cprod_%7Bi%3D1%7D%5E%7Bn%7D%5Cleft%5C%7B%5Cfrac%7B%5Clambda%5E%7Bx_%7Bi%7D%7D%7D%7Bx_%7Bi%7D%20%21%7D%20e%5E%7B-%5Clambda%7D%5Cright%5C%7D%3D%5Cfrac%7B%5Clambda%5E%7Bx_%7B1%7D%2B%5Cldots%2Bx_%7Bn%7D%7D%7D%7Bx_%7B1%7D%20%21%20%5Cldots%20x_%7Bn%7D%20%21%7D%20e%5E%7B-n%20%5Clambda%7D%0A "
    L(\lambda)=\prod_{i=1}^{n} f_{X}\left(x_{i} ; \lambda\right)=\prod_{i=1}^{n}\left\{\frac{\lambda^{x_{i}}}{x_{i} !} e^{-\lambda}\right\}=\frac{\lambda^{x_{1}+\ldots+x_{n}}}{x_{1} ! \ldots x_{n} !} e^{-n \lambda}
    ")

## `5.2.` Find the second derivative of ![L(\\lambda)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;L%28%5Clambda%29 "L(\lambda)") as a formula.

### `5.2.1`: get ![\\log L(\\lambda)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Clog%20L%28%5Clambda%29 "\log L(\lambda)")

![
\\log L(\\lambda)=\\sum\_{i=1}^{n} x\_{i} \\log \\lambda-n \\lambda-\\sum\_{i=1}^{n} \\log \\left(x\_{i} !\\right)
](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0A%5Clog%20L%28%5Clambda%29%3D%5Csum_%7Bi%3D1%7D%5E%7Bn%7D%20x_%7Bi%7D%20%5Clog%20%5Clambda-n%20%5Clambda-%5Csum_%7Bi%3D1%7D%5E%7Bn%7D%20%5Clog%20%5Cleft%28x_%7Bi%7D%20%21%5Cright%29%0A "
\log L(\lambda)=\sum_{i=1}^{n} x_{i} \log \lambda-n \lambda-\sum_{i=1}^{n} \log \left(x_{i} !\right)
")

### `5.2.1`: Second derivative of ![L(\\lambda)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;L%28%5Clambda%29 "L(\lambda)")

![
\\frac{d}{d \\lambda}\\{\\log L(\\lambda)\\}
=\\sum\_{i=1}^{n} \\frac{x\_{i}}{\\lambda}-n=
0 \\\\ \\text{so,}  \\ \\widehat{\\lambda}=\\frac{1}{n} 
\\sum\_{i=1}^{n} x\_{i}=\\bar{x}
](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0A%5Cfrac%7Bd%7D%7Bd%20%5Clambda%7D%5C%7B%5Clog%20L%28%5Clambda%29%5C%7D%0A%3D%5Csum_%7Bi%3D1%7D%5E%7Bn%7D%20%5Cfrac%7Bx_%7Bi%7D%7D%7B%5Clambda%7D-n%3D%0A0%20%5C%5C%20%5Ctext%7Bso%2C%7D%20%20%5C%20%5Cwidehat%7B%5Clambda%7D%3D%5Cfrac%7B1%7D%7Bn%7D%20%0A%5Csum_%7Bi%3D1%7D%5E%7Bn%7D%20x_%7Bi%7D%3D%5Cbar%7Bx%7D%0A "
\frac{d}{d \lambda}\{\log L(\lambda)\}
=\sum_{i=1}^{n} \frac{x_{i}}{\lambda}-n=
0 \\ \text{so,}  \ \widehat{\lambda}=\frac{1}{n} 
\sum_{i=1}^{n} x_{i}=\bar{x}
")

## `5.3/5.4.` Show ![\\hat{\\lambda}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Chat%7B%5Clambda%7D "\hat{\lambda}") is a maximum.

-   To check for the maximum, set
    ![\\lambda = \\hat{\\lambda}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Clambda%20%3D%20%5Chat%7B%5Clambda%7D "\lambda = \hat{\lambda}")
    when in above derivation.
-   Then check to see if negative

![
\\frac{d^{2}}{d \\lambda^{2}}\\{\\log L(\\lambda)\\}
=-\\frac{1}{\\lambda^{2}} \\sum\_{i=1}^{n} x\_{i}\<0 \\quad 
\\text {where} \\ \\lambda=\\widehat{\\lambda}
](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0A%5Cfrac%7Bd%5E%7B2%7D%7D%7Bd%20%5Clambda%5E%7B2%7D%7D%5C%7B%5Clog%20L%28%5Clambda%29%5C%7D%0A%3D-%5Cfrac%7B1%7D%7B%5Clambda%5E%7B2%7D%7D%20%5Csum_%7Bi%3D1%7D%5E%7Bn%7D%20x_%7Bi%7D%3C0%20%5Cquad%20%0A%5Ctext%20%7Bwhere%7D%20%5C%20%5Clambda%3D%5Cwidehat%7B%5Clambda%7D%0A "
\frac{d^{2}}{d \lambda^{2}}\{\log L(\lambda)\}
=-\frac{1}{\lambda^{2}} \sum_{i=1}^{n} x_{i}<0 \quad 
\text {where} \ \lambda=\widehat{\lambda}
")

## `5.5.` Make a function called `myml(x)`

-   Draws the graph of 𝐿(𝜆) and 𝑙(𝜆) with the x = vector of data

``` r
myml = function(x) {
  
  # Create lambda vector as a set of values ranging from 0 to 2*max(x)
  lambda=seq(.01,2*max(x),0.5)
  
  # Calculate Poisson MLE
  lambdahat = sum(x)/length(x)
  
  #the likelihood function
  lik <- exp(-length(x)* lambda)*(lambda^sum(x))/prod(factorial(x))
  
  #the loglikelihood function
  loglik <- -length(x)* lambda+sum(x)*log(lambda)+log(prod(factorial(x)))
  
  # Plot the likelihood
  plot(lambda, lik,    
       col = 'steelblue', type = 'l', 
       main = 'Plot for Likelihood Lambda (Poisson MLE)\nDaniel Carpenter',
       xlab = 'Lambda', ylab = 'Likelihood')
  text(x=lambdahat, y=mean(lik), paste('L Hat=',format(round(lambdahat, 4), nsmall=4)))
  
  # Plot the loglikelihood
  plot(lambda, loglik,
       col = 'tomato3', type = 'l', 
       main = 'Plot for Log(Likelihood) Lambda (Poisson MLE)\nDaniel Carpenter',
       xlab = 'Lambda', ylab = 'Log Likelihood')
  text(x=lambdahat, y=mean(loglik), paste('L Hat=',format(round(lambdahat, 4), nsmall=4)))
  
  return(list(lambdahat = lambdahat))
}
```

> Give the output of your function when x = {3,4,3,5,6}.

``` r
x = c(3,4,3,5,6)
myml(x)
```

![](assignment1_files/figure-gfm/out5.5-1.png)<!-- -->![](assignment1_files/figure-gfm/out5.5-2.png)<!-- -->

    ## $lambdahat
    ## [1] 4.2

## `5.6.` MLE Does not Account for Priors

The maximum likelihood estimate does not account for prior information.
We know the prior times the likelihood forms the posterior, which the
M.L. Estimate only uses the likelihood.

------------------------------------------------------------------------

<br>

# Task `6`: Un/Biased ![\\lambda](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Clambda "\lambda") Proof

Prove whether
![\\hat\\lambda](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Chat%5Clambda "\hat\lambda")
by proving
![E(y) = \\lambda](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;E%28y%29%20%3D%20%5Clambda "E(y) = \lambda"),
for a random variable
![Y](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;Y "Y")
∼
![Poisson(\\lambda)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;Poisson%28%5Clambda%29 "Poisson(\lambda)"),
![E(Y) = \\lambda](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;E%28Y%29%20%3D%20%5Clambda "E(Y) = \lambda")

![
\\begin{aligned}
E(Y) &=\\sum\_{y=0}^{\\infty} y \\frac{e^{-\\lambda} \\lambda^{y}}{y !} \\\\
&=\\sum\_{y=1}^{\\infty} y \\frac{e^{-\\lambda} \\lambda^{y}}{y !} \\\\
&=\\sum\_{y=1}^{\\infty} \\frac{e^{-\\lambda} \\lambda^{y}}{(y-1) !} \\\\
&=\\lambda e^{-\\lambda} \\sum\_{y=1}^{\\infty} \\frac{\\lambda^{y-1}}{(y-1) !} \\\\
&=\\lambda e^{-\\lambda} \\sum\_{y=0}^{\\infty} \\frac{\\lambda^{y}}{y !} \\\\
&=\\lambda e^{-\\lambda} e^{\\lambda} \\\\
&=\\lambda
\\end{aligned}
](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0A%5Cbegin%7Baligned%7D%0AE%28Y%29%20%26%3D%5Csum_%7By%3D0%7D%5E%7B%5Cinfty%7D%20y%20%5Cfrac%7Be%5E%7B-%5Clambda%7D%20%5Clambda%5E%7By%7D%7D%7By%20%21%7D%20%5C%5C%0A%26%3D%5Csum_%7By%3D1%7D%5E%7B%5Cinfty%7D%20y%20%5Cfrac%7Be%5E%7B-%5Clambda%7D%20%5Clambda%5E%7By%7D%7D%7By%20%21%7D%20%5C%5C%0A%26%3D%5Csum_%7By%3D1%7D%5E%7B%5Cinfty%7D%20%5Cfrac%7Be%5E%7B-%5Clambda%7D%20%5Clambda%5E%7By%7D%7D%7B%28y-1%29%20%21%7D%20%5C%5C%0A%26%3D%5Clambda%20e%5E%7B-%5Clambda%7D%20%5Csum_%7By%3D1%7D%5E%7B%5Cinfty%7D%20%5Cfrac%7B%5Clambda%5E%7By-1%7D%7D%7B%28y-1%29%20%21%7D%20%5C%5C%0A%26%3D%5Clambda%20e%5E%7B-%5Clambda%7D%20%5Csum_%7By%3D0%7D%5E%7B%5Cinfty%7D%20%5Cfrac%7B%5Clambda%5E%7By%7D%7D%7By%20%21%7D%20%5C%5C%0A%26%3D%5Clambda%20e%5E%7B-%5Clambda%7D%20e%5E%7B%5Clambda%7D%20%5C%5C%0A%26%3D%5Clambda%0A%5Cend%7Baligned%7D%0A "
\begin{aligned}
E(Y) &=\sum_{y=0}^{\infty} y \frac{e^{-\lambda} \lambda^{y}}{y !} \\
&=\sum_{y=1}^{\infty} y \frac{e^{-\lambda} \lambda^{y}}{y !} \\
&=\sum_{y=1}^{\infty} \frac{e^{-\lambda} \lambda^{y}}{(y-1) !} \\
&=\lambda e^{-\lambda} \sum_{y=1}^{\infty} \frac{\lambda^{y-1}}{(y-1) !} \\
&=\lambda e^{-\lambda} \sum_{y=0}^{\infty} \frac{\lambda^{y}}{y !} \\
&=\lambda e^{-\lambda} e^{\lambda} \\
&=\lambda
\end{aligned}
")

Therefore,
![E(Y) = \\lambda](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;E%28Y%29%20%3D%20%5Clambda "E(Y) = \lambda"),
which indicates that
![\\lambda](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Clambda "\lambda")
is not biased.

------------------------------------------------------------------------

<br>

# Task `7`: Maximum Likelihood for *Bernoulli* Proof

Consider
![n](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;n "n")
Bernoulli trials with
![X](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;X "X")
successes.

![
p(x)=\\left(\\begin{array}{l}
n \\\\
X
\\end{array}\\right) \\theta^{X}(1-\\theta)^{n-X}
](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0Ap%28x%29%3D%5Cleft%28%5Cbegin%7Barray%7D%7Bl%7D%0An%20%5C%5C%0AX%0A%5Cend%7Barray%7D%5Cright%29%20%5Ctheta%5E%7BX%7D%281-%5Ctheta%29%5E%7Bn-X%7D%0A "
p(x)=\left(\begin{array}{l}
n \\
X
\end{array}\right) \theta^{X}(1-\theta)^{n-X}
")

Compute the likelihood:
![L(\\theta)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;L%28%5Ctheta%29 "L(\theta)")

![
L(\\theta)=\\left(\\begin{array}{l}
n \\\\
X
\\end{array}\\right) \\theta^{X}(1-\\theta)^{n-X}
](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0AL%28%5Ctheta%29%3D%5Cleft%28%5Cbegin%7Barray%7D%7Bl%7D%0An%20%5C%5C%0AX%0A%5Cend%7Barray%7D%5Cright%29%20%5Ctheta%5E%7BX%7D%281-%5Ctheta%29%5E%7Bn-X%7D%0A "
L(\theta)=\left(\begin{array}{l}
n \\
X
\end{array}\right) \theta^{X}(1-\theta)^{n-X}
")

Find the log(liklihood):

![
\\log L(\\theta)=\\log \\left\\{\\left(\\begin{array}{l}
n \\\\
x
\\end{array}\\right)\\right\\}+x \\log (\\theta)+(n-x) \\log (1-\\theta)
](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0A%5Clog%20L%28%5Ctheta%29%3D%5Clog%20%5Cleft%5C%7B%5Cleft%28%5Cbegin%7Barray%7D%7Bl%7D%0An%20%5C%5C%0Ax%0A%5Cend%7Barray%7D%5Cright%29%5Cright%5C%7D%2Bx%20%5Clog%20%28%5Ctheta%29%2B%28n-x%29%20%5Clog%20%281-%5Ctheta%29%0A "
\log L(\theta)=\log \left\{\left(\begin{array}{l}
n \\
x
\end{array}\right)\right\}+x \log (\theta)+(n-x) \log (1-\theta)
")

Then, Find the derivative of log(likelihood)

![
\\frac{d}{d \\theta} \\log L(\\theta)=\\frac{x}{\\theta}-\\frac{n-x}{1-\\theta}
](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0A%5Cfrac%7Bd%7D%7Bd%20%5Ctheta%7D%20%5Clog%20L%28%5Ctheta%29%3D%5Cfrac%7Bx%7D%7B%5Ctheta%7D-%5Cfrac%7Bn-x%7D%7B1-%5Ctheta%7D%0A "
\frac{d}{d \theta} \log L(\theta)=\frac{x}{\theta}-\frac{n-x}{1-\theta}
")

Set log(likelihood) to
![0](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;0 "0")

![
\\frac{x}{\\theta}-\\frac{n-x}{1-\\theta}=0 \\Longleftrightarrow x-\\theta x=n \\theta-\\theta x \\Longleftrightarrow \\theta=\\frac{x}{n}
](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0A%5Cfrac%7Bx%7D%7B%5Ctheta%7D-%5Cfrac%7Bn-x%7D%7B1-%5Ctheta%7D%3D0%20%5CLongleftrightarrow%20x-%5Ctheta%20x%3Dn%20%5Ctheta-%5Ctheta%20x%20%5CLongleftrightarrow%20%5Ctheta%3D%5Cfrac%7Bx%7D%7Bn%7D%0A "
\frac{x}{\theta}-\frac{n-x}{1-\theta}=0 \Longleftrightarrow x-\theta x=n \theta-\theta x \Longleftrightarrow \theta=\frac{x}{n}
")

![\\hat{\\theta}=\\frac{x}{n}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Chat%7B%5Ctheta%7D%3D%5Cfrac%7Bx%7D%7Bn%7D "\hat{\theta}=\frac{x}{n}")
is the candidate. Now take the second derivative:

![
\\frac{d^{2}}{d \\theta^{2}} \\log L(\\theta)=-\\frac{x}{\\theta^{2}}-\\frac{n-x}{(1-\\theta)^{2}}
](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0A%5Cfrac%7Bd%5E%7B2%7D%7D%7Bd%20%5Ctheta%5E%7B2%7D%7D%20%5Clog%20L%28%5Ctheta%29%3D-%5Cfrac%7Bx%7D%7B%5Ctheta%5E%7B2%7D%7D-%5Cfrac%7Bn-x%7D%7B%281-%5Ctheta%29%5E%7B2%7D%7D%0A "
\frac{d^{2}}{d \theta^{2}} \log L(\theta)=-\frac{x}{\theta^{2}}-\frac{n-x}{(1-\theta)^{2}}
")

Above is is always less than 0. Therefore
![\\hat\\theta = \\frac{X}{n}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Chat%5Ctheta%20%3D%20%5Cfrac%7BX%7D%7Bn%7D "\hat\theta = \frac{X}{n}")
is the maximum likelihood estimator for
![\\theta](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Ctheta "\theta").

------------------------------------------------------------------------

<br>

# Task `8`: Newton Raphson Expression & Function

> Estimate the root of a function and graphs it

## Newton Rasphson Expression

![
x\_{n+1}=x\_{n}-\\frac{f\\left(x\_{n}\\right)}{f^{\\prime}\\left(x\_{n}\\right)}
](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0Ax_%7Bn%2B1%7D%3Dx_%7Bn%7D-%5Cfrac%7Bf%5Cleft%28x_%7Bn%7D%5Cright%29%7D%7Bf%5E%7B%5Cprime%7D%5Cleft%28x_%7Bn%7D%5Cright%29%7D%0A "
x_{n+1}=x_{n}-\frac{f\left(x_{n}\right)}{f^{\prime}\left(x_{n}\right)}
")

## Create `mynr()` Function

``` r
mynr <- function(f, fdash, x0,
                 tol = 1e-5, n = 1000,
                 dec = 4) 
{
  
  # ROOT CALCULATIONS ---------------------------------------------------------
  
  # Check the initial guess is correct
  fa <- f(x0)
  if (fa == 0.0) {
    approxRoot <- x0
  } else {   # If not the guess, then solve
    for (i in 1:n) {
      x1 <- x0 - (f(x0) / fdash) # Calculate next value x1
      n[i] <- x1 # Store x1
      
      # Once the difference between x0 and x1 becomes sufficiently small, output the results.
      if (abs(x1 - x0) < tol) {
        approxRoot <- tail(n, n=1)
      }
      # If mynr function has not yet reached convergence set x1 as x0 and continue
      x0 <- x1
    }
  }
  
  # PLOTS --------------------------------------------------------------------
  
  # Plot the function
  curve(f, col='slategray4', lwd=3, lty=1, ylab='f(x)',
        xlim=c(approxRoot-3,approxRoot+3), ylim=c(-1,3))
  
  # X Axis
  abline(h=0, col = 'tomato3', lty=2, lwd=1)
  
  # Where function crosses x axis
  abline(v=approxRoot, col = 'seagreen4',  lty=2, lwd=1)
  
  # Plot the root as text, rounded to 4 decimals
  text(x=approxRoot + 1, y = -0.5, paste("Root = ",format(round(approxRoot, dec), nsmall=dec)))
  
  # Title
  title(main = "Newton Raphson Method of Plotting Derivative Roots\nDaniel Carpenter")
  
  # Return the Root Approximation  
  return(list('ApproxRoot' = approxRoot))
}
```

## Call `mynr()` Function

``` r
# Package for computing derivative
if(!require(numDeriv)) install.packages('numDeriv') 

# Inputs
  
  ## Base Function to take derivative of and plot
  f <- function(x) { x^2-5*x+6 } 
  
  ## Initial guess of root
  x0 <- 5 
  
  ## First-order derivative f'(x0)
  fdash <- genD(func = f, x = x0)$D[1] 

# Call Function
mynr(f, fdash, x0)
```

![](assignment1_files/figure-gfm/call_mynrFun-1.png)<!-- -->

    ## $ApproxRoot
    ## [1] 3

------------------------------------------------------------------------

<br>

# Task `9`: Maximum Likelihood Using Newton Raphson

## Create `mynrml()` Function

``` r
mynrml <- function(x, x0, 
                   tol = 1e-7, N = 100, dec = 4, a = 0, b = 10) {
  
  loglik.dash <- function(lambda, arg = x) {
    (1/lambda)*sum(arg) - length(arg)
  }
  
  loglik.dashdash <- function(lambda, arg = x) {
    (-1/(lambda^2))*sum(arg)
  }
  
  # Create The Plot
  curve(loglik.dash, col = 'lightsteelblue', 
        lwd = 2, xlim = c(a,b), ylim = c(-5,10), 
        xlab ='Theta', ylab ='Derivative of Log Likelihood', 
        main ='MLE Using NR Method\nDaniel Carpenter')
  
    # Use the Newton Raphson method -----------------------------------------
    
    # First initialize k to 0, the vector containing each iteration, 
    # i, the count of iterations, and x1.
    k <- vector(mode = , length = )
    i <- 1
    x1 <- x0
    
    # implement the loop
    while (i < N) {
      x1 <- x0 - (loglik.dash(lambda = x0) / loglik.dashdash(lambda = x0))
      
      # Store each iterating in k
      k[i] <- x1
      
      # If the sequence of x_i’s converges, i.e. the distance between x_i and x_i+1 
      # is below the pre-specified tolerance level, the loop ends
      if (abs(x1 - x0) < tol) {
        root <- k[length(k)]
        out <- list(Estimate = root)
      } #if the loop isn’t ready to end, iterate
      
      i <- i+1
      x0 <- x1 
    } #If the loop doesn’t end after N iterations, it stops
  
  # Add the MLE Estimate on the graph with text
  points(x0, 0, col = 'steelblue4', pch = 16, cex=1.5)
  text(x0 + 1, 1, paste('Estimate=',format(round(x0, dec), nsmall=dec)))
  
  return(out)
}
```

## Call `mynrml()` Function

``` r
x = c(3,4,3,5,6)
x0 = 3

mynrml(x, x0)
```

![](assignment1_files/figure-gfm/mynrmlFun_call-1.png)<!-- -->

    ## $Estimate
    ## [1] 4.2

------------------------------------------------------------------------

<br>

# Task `10`: NR Function without Dervative as Parameter

## Create `mynrf()` Function

``` r
mynrf <- function(f, x0,
                 tol = 1e-5, n = 1000,
                 dec = 4) 
{
  
  # ROOT CALCULATIONS ---------------------------------------------------------
 
  # Package for computing derivative
  if(!require(numDeriv)) install.packages('numDeriv') 

  ## First-order derivative f'(x0)
  fdash <- genD(func = f, x = x0)$D[1]  
  
  # Check the initial guess is correct
  fa <- f(x0)
  if (fa == 0.0) {
    approxRoot <- x0
  } else {   # If not the guess, then solve
    for (i in 1:n) {
      x1 <- x0 - (f(x0) / fdash) # Calculate next value x1
      n[i] <- x1 # Store x1
      
      # Once the difference between x0 and x1 becomes sufficiently small, output the results.
      if (abs(x1 - x0) < tol) {
        approxRoot <- tail(n, n=1)
      }
      # If mynr function has not yet reached convergence set x1 as x0 and continue
      x0 <- x1
    }
  }
  
  # PLOTS --------------------------------------------------------------------
  
  # Plot the function
  curve(f, col='slategray4', lwd=3, lty=1, ylab='f(x)',
        xlim=c(approxRoot-3,approxRoot+3), ylim=c(-1,3))
  
  # X Axis
  abline(h=0, col = 'tomato3', lty=2, lwd=1)
  
  # Where function crosses x axis
  abline(v=approxRoot, col = 'seagreen4',  lty=2, lwd=1)
  
  # Plot the root as text, rounded to 4 decimals
  text(x=approxRoot + 1, y = -0.5, paste("Root = ",format(round(approxRoot, dec), nsmall=dec)))
  
  # Title
  title(main = "Newton Raphson Method of Plotting Derivative Roots\nDaniel Carpenter")
  
  # Return the Root Approximation  
  return(list('ApproxRoot' = approxRoot))
}
```

## Call `mynrf()` Function

``` r
# Inputs
  
  ## Base Function to take derivative of and plot
  f <- function(x) { x^2-5*x+6 } 
  
  ## Initial guess of root
  x0 <- 5 

# Call Function
mynrf(f, x0)
```

![](assignment1_files/figure-gfm/call_mynrfFun-1.png)<!-- -->

    ## $ApproxRoot
    ## [1] 3
