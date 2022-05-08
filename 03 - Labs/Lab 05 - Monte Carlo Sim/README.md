Lab 5 - MCMC
================
Daniel Carpenter
February 2022

-   [Task `1` Two-State Bayes Box & Coin-Die
    Experiment](#task-1-two-state-bayes-box--coin-die-experiment)
    -   [`1.1b. i-ii` Prior for two state Bayes’ box Binomial
        Experiment](#11b-i-ii-prior-for-two-state-bayes-box-binomial-experiment)
    -   [`1.1b. iii` Acceptance and rejection
        set](#11b-iii-acceptance-and-rejection-set)
    -   [`1.2` - Derivation of Algorithm for Coin-Die Bayes
        Box](#12---derivation-of-algorithm-for-coin-die-bayes-box)
-   [Task `2` Coin Die MCMC
    Simulation](#task-2-coin-die-mcmc-simulation)
    -   [`1 a-c` Create Function](#1-a-c-create-function)
    -   [`2` Use output of `cdbox` as input to
        `coindie`](#2-use-output-of-cdbox-as-input-to-coindie)
-   [Task `3` Discrete Simulation using
    MCMC](#task-3-discrete-simulation-using-mcmc)
    -   [`3.1` Overview of Algorithm](#31-overview-of-algorithm)
    -   [`3.2` Create the Function](#32-create-the-function)
-   [Task `4` Discrete Simulation using
    MCMC](#task-4-discrete-simulation-using-mcmc)
-   [Task `5` Continuous Similation using
    MCMC](#task-5-continuous-similation-using-mcmc)
    -   [`Part 1-4` Create Function with Beta
        Proposal](#part-1-4-create-function-with-beta-proposal)
-   [Task `6` Gibbs Sampling via MCMC](#task-6-gibbs-sampling-via-mcmc)
    -   [`6.1` Gibbs Sampling MCMC and
        `OpenBUGS`](#61-gibbs-sampling-mcmc-and-openbugs)
    -   [`6.2-3` - OpenBUGS Model and Pretty Print
        Output](#62-3---openbugs-model-and-pretty-print-output)
    -   [`6.4-6` Basic SLR Model using
        JAGS](#64-6-basic-slr-model-using-jags)
    -   [`6.7` Compare classical tests using the `lm()`
        function](#67-compare-classical-tests-using-the-lm-function)
    -   [`6.8` Adding
        ![beta^{2}\_{2}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;beta%5E%7B2%7D_%7B2%7D "beta^{2}_{2}")
        to Model](#68-adding-beta2_2-to-model)
    -   [`6.9` Comparison of results](#69-comparison-of-results)

## Task `1` Two-State Bayes Box & Coin-Die Experiment

### `1.1b. i-ii` Prior for two state Bayes’ box Binomial Experiment

-   Make a prior for a two state Bayes’ box that corresponds to an
    acceptance set that has 2 values in it, x=4, n=10 in a Binomial
    experiment. The parameter values are 0.4 and 0.8.

``` r
  # 1 (i-ii)
  # Make a coin-die bbox
  # k = number of faces in the event set E for acceptance of proposal
  # Lik = likelihood for 2 states of theta
  # theta = two states
  # h1 = small relative to h2
  cdbbox<-function(k=1,lik,theta, h1="s")
  { 
    # K=1...6
    # xtable is a library which has functions useful for latex output
    library(xtable)
    
    # rename the first and second components of the likelihood
    lik1<-lik[1]
    lik2<-lik[2]
    
    # We will now make a prior that has the desired characteristics
    # See if you can prove the following
    # if h1 small "s" then ... else ...
    ifelse(h1=="s",
           pi1<-k/6*lik2/(lik1+k/6*lik2), 
           pi1<-lik2/(lik2+k/6*lik1)
    )
    
    # sum of probs is 1
    prior=c(pi1,1-pi1)
    
    #lik<-c(lik1,lik2)
    h<-prior*lik
    
    # Bayes
    post=h/sum(h)
    
    # Make a matrix for the Bayes box
    mat<-cbind(theta,prior,lik,h,post)
    rownames(mat)<-1:length(lik)
    Totals=c(NA,sum(prior),NA,sum(h),sum(post))
    mat2=rbind(mat,Totals)
    
    # Now make some plots useful in explaining the procedure
    # graphics.off()
    # dev.new(noRStudioGD = TRUE)
    layout(matrix(c(1,2),nr=1,nc=2,byrow=TRUE))
    barplot(matrix(c(0.5,0.5),nc=2,nr=1,byrow=TRUE,dimnames=list(c("Coin"),theta)),
            ylim=c(0,1),las=1,main="Proposal\n Uniform")
    barplot(matrix(h,nc=2,nr=1,byrow=TRUE,dimnames=list(c("Coin"),theta)),
            ylim=c(0,max(h)+0.5*max(h)),las=1,main="Proportional to target\n h")
    # Return a list of useful objects
    return(list(bbox=mat2,latex=xtable(mat2,digits=6),mat=mat,h=h,h1=h1,k=k))
  }
  
  # Call function
  cdbbox(k=2,lik=dbinom(x=4,size=10,prob=c(0.4,0.8)),theta=c(0.4,0.8),h1="s")->ans 
```

![](lab5_files/figure-gfm/1a-1.png)<!-- -->

``` r
  # Get the bayes box and knit using kable for aesthetics
  knitr::kable(ans$bbox)
```

|        | theta |     prior |       lik |         h | post |
|:-------|------:|----------:|----------:|----------:|-----:|
| 1      |   0.4 | 0.0072628 | 0.2508227 | 0.0018217 | 0.25 |
| 2      |   0.8 | 0.9927372 | 0.0055050 | 0.0054650 | 0.75 |
| Totals |    NA | 1.0000000 |        NA | 0.0072867 | 1.00 |

### `1.1b. iii` Acceptance and rejection set

Accept it with a probability. Need an acceptance and rejection set.
Anything lower than the probability can be in the acceptance set. For
Example, if 4/6, then can accept {1,2,3,4}

``` r
  ## Impoved Graphic
  cdbbox2<-function(k=1,lik,theta, h1="s")
  { 
    # K=1...6
    # xtable is a library which has functions useful for latex output
    library(xtable)
    
    # rename the first and second components of the likelihood
    lik1<-lik[1]
    lik2<-lik[2]
    
    # We will now make a prior that has the desired characteristics
    # See if you can prove the following
    # if h1 small "s" then ... else ...
    ifelse(h1=="s",
           pi1 <- k/6 * lik2 / (lik1 + k/6 * lik2), 
           pi1 <-       lik2 / (lik2 + k/6 * lik1)
    )
    
    # sum of probs is 1
    prior=c(pi1,1-pi1)
    
    #lik<-c(lik1,lik2)
    h<-prior*lik
    
    # Bayes
    post=h/sum(h)
    
    # Make a matrix for the Bayes box
    mat<-cbind(theta,prior,lik,h,post)
    rownames(mat)<-1:length(lik)
    Totals=c(NA,sum(prior),NA,sum(h),sum(post))
    mat2=rbind(mat,Totals)
    
    # Now make some plots useful in explaining the procedure
    # graphics.off()
    # dev.new(noRStudioGD = TRUE)
    layout(matrix(c(1,2),nr=1,nc=2,byrow=TRUE))
    barplot(matrix(c(0.5,0.5),nc=2,nr=1,byrow=TRUE,dimnames=list(c("Coin"),theta)),
            ylim=c(0,1),las=1,main="Proposal Distribution\n (Uniform)",
            col = 'grey20',
            ylab = 'Probabilities',
            xlab = 'Outcome')
    barplot(matrix(h,nc=2,nr=1,byrow=TRUE,dimnames=list(c("Coin"),theta)),
            ylim=c(0,max(h)+0.5*max(h)),las=1,main="Proportional to target\n h",
            col = 'red',
            ylab = 'Probabilities',
            xlab = 'Outcome')
    # Return a list of useful objects
    return(list(bbox=mat2,latex=xtable(mat2,digits=6),mat=mat,h=h,h1=h1,k=k))
  }
  
  # Call Function
  outputOfCdbbox2 <- cdbbox2(k=2,lik=dbinom(x=4,size=10,prob=c(0.4,0.8)),theta=c(0.4,0.8),h1="s") 
```

![](lab5_files/figure-gfm/1b-1.png)<!-- -->

### `1.2` - Derivation of Algorithm for Coin-Die Bayes Box

#### `R` Code

``` r
ifelse(h1=="s",pi1<-k/6*lik2/(lik1+k/6*lik2), pi1<-lik2/(lik2+k/6*lik1))
```

#### ![\\LaTeX](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5CLaTeX "\LaTeX") Derivation:

![
\\text{then} \\ p\_{i,1} = \\frac{k}{6} \\times \\frac{lik_2}{lik_1 + k \\times lik_2}, \\\\
\\text{else} \\ p\_{i,1} = \\times \\frac{lik_2}{lik_2 + k \\times lik_1}
](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0A%5Ctext%7Bthen%7D%20%5C%20p_%7Bi%2C1%7D%20%3D%20%5Cfrac%7Bk%7D%7B6%7D%20%5Ctimes%20%5Cfrac%7Blik_2%7D%7Blik_1%20%2B%20k%20%5Ctimes%20lik_2%7D%2C%20%5C%5C%0A%5Ctext%7Belse%7D%20%5C%20p_%7Bi%2C1%7D%20%3D%20%5Ctimes%20%5Cfrac%7Blik_2%7D%7Blik_2%20%2B%20k%20%5Ctimes%20lik_1%7D%0A "
\text{then} \ p_{i,1} = \frac{k}{6} \times \frac{lik_2}{lik_1 + k \times lik_2}, \\
\text{else} \ p_{i,1} = \times \frac{lik_2}{lik_2 + k \times lik_1}
")

------------------------------------------------------------------------

<br>

## Task `2` Coin Die MCMC Simulation

### `1 a-c` Create Function

``` r
coindie<-function(n=100, h=c(1/4,3/4),E2=c(5,6),init=1,...){
  library(xtable)
  dieset<-c()
  dieset[1]<-"E1"
  die<-function(n=1){
  sample(1:6,size=n,replace=TRUE)
  }
  
  coin<-function(n=1){
  sample(1:2,size=n,replace=TRUE)
  }
  face<-c()
  alpha<-c() # holds acceptance probs
  alpha[1]<-1
  post<-c()# post sample
  prop<-c() # vec of proposed states 1s and 2s
  prop[1]=init # initial state
  post[1]=prop[1]
  dice<-c()
  dice[1]<-die()
  
  for(i in 2:n){ # starts at 2 because initial value given above
  prop[i]<-coin()
  alpha[i]=min(1,h[prop[i]]/h[post[i-1]])
  
  dice[i]<-die()
  ifelse(alpha[i]==1,dieset[i]<-"E1",dieset[i]<-"E2")
  # is x an element of set y
  if(alpha[i]==1 | (is.element(dice[i],E2) & alpha[i]!=1)){post[i]<-prop[i]}
  else{post[i]<-post[i-1]}
   }  
  res<-matrix(c(prop,round(alpha,2),dieset,dice,post ),nc=5,nr=n,byrow=FALSE,dimnames=list(1:n,c("proposal","alpha", "E","dice","post")))
  sim<-table(post)/n
  postexact<-h/sum(h)
  barplot(sim,...)
  return(list(iter=res,sim=sim,postexact=postexact,post=post,xtable=xtable(res,dig=1)) )
}
```

#### Call the function, and add better labels to the graph

``` r
  # Call the function
  ans <- coindie(n=10,h=c(0.6,0.4),E2=c(2,3,4,5),
                  
                  # Add X Label to be more descriptive
                  xlab = "Outcome",
                  
                  # Add Y Label to be more descriptive
                  ylab = 'Probability',
                  
                  # Add Title  to be more descriptive
                  main = 'Coin Die Function using MCMC\nDaniel Carpenter',
                  
                  # Add color to indicate post color red
                  col = 'red')
```

![](lab5_files/figure-gfm/2.2-1.png)<!-- -->

<br>

### `2` Use output of `cdbox` as input to `coindie`

-   Input `h` is the prior times the likelihood of the output created
    from the cdbbox2 function. This function estimates the prior and
    then applies the likelihood to get the outcome
-   Output, shows how the outcome of the experiment changes when using
    an updated prior beliefs.

``` r
# Here, get the value of h to use in this example. This objected created in previous code chunk
# When calling outputOfCdbbox2 <- cdbbox2(k=2,lik=dbinom(x=4,size=10,prob=c(0.4,0.8)),theta=c(0.4,0.8),h1="s") 
h = outputOfCdbbox2$h

ans <- coindie(n=10,h=h,E2=c(2,3,4,5),
               
               # Add X Label to be more descriptive
               xlab = "Outcome",
               
               # Add Y Label to be more descriptive
               ylab = 'Probability',
               
               # Add Title  to be more descriptive
               main = 'Coin Die Function using MCMC\nDaniel Carpenter',
               
               # Add color to indicate post color red
               col = 'red')
```

![](lab5_files/figure-gfm/2.2.1-1.png)<!-- -->

------------------------------------------------------------------------

<br>

## Task `3` Discrete Simulation using MCMC

### `3.1` Overview of Algorithm

Core of `simR()`? Take a random sample from the uniform of size 1. If
the proposed value is less than or equal to alpha (or the area at `i`,
`j`), then accept it as the solution. Else, reject it.

### `3.2` Create the Function

``` r
  # This function makes discrete simulations from a posterior with any number of h values
  # n=nu of iterations
  # You can embellish this function
  simR<-function(n=10000, h=c(0.03344302,0.06165627),...){
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
    
    barplot(sim,...)
    postexact<-h/sum(h)
    # The returned output is a list 
    # Use obj$ to obtain whatever interests you
    return(list(iter=res,sim=sim,postexact=postexact,post=post) )
  }

  
  # Create a funciton to establish a Uniform Binomial Experiment
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
  simROut <- simR(n=10000, h=getH(numThetaValues, x, n),
                  
                  # Title
                  main = paste("Histogram of Simulation Output using simR()\n",
                               "Assumes Uniform Prior dist. over ", numThetaValues, 
                               " values of theta.\nProb. ", x, 
                               " with ", n, "bernoulli trials - Daniel Carpenter"),
                  ylab = "Frequency",
                  xlab = "Proposed h's",
                  col = 'lightblue'
                  )
```

![](lab5_files/figure-gfm/3.2-1.png)<!-- -->

------------------------------------------------------------------------

<br>

## Task `4` Discrete Simulation using MCMC

``` r
# What about different proposal distributions
# Again for the discrete case
simRQ<-function(n=1000,init=1, h=c(1,1),pr=c(1,1)/2,...) {
  
  alpha<-c() # holds transition probs
  alpha[1]<-1
  u<-c() # holds uniform values
  u[1]<-1
  post<-c()# post sample
  prop<-c() # vec of proposed states 1s and 2s etc
  prop[1]=init # initial state
  post[1]=prop[1]
  
  q<-function(x){pr[x]}
  
  for(i in 2:n){ # starts at 2 because initial value given above
    
    #make a sample from the proposal
    sample(1:length(h),1,replace=TRUE,prob=pr)->prop[i]
    
    #Calculate alpha adjusting for the proposal being non uniform
    alpha[i]=min(1,h[prop[i]]*q(post[i-1])/(h[post[i-1]]*q(prop[i])))
    
    # now choose the proposal with probability alpha
    u[i]=runif(1)
    if(u[i]<=alpha[i]){post[i]<-prop[i]}
    else{post[i]=post[i-1]}
  }
  res<-matrix(c(prop,u,alpha,post ),nc=4,nr=n,byrow=FALSE,dimnames=list(1:n,c("prop","u","alpha","post")))
  sim<-table(post)/n
  postexact<-h/sum(h)
  barplot(sim,...)
  tmp<-c()
  ifelse(length(res[,1])>=20,tmp<-res[1:20,],tmp<-res)
  
  return(list(iter=tmp,sim=sim,post=postexact) )
}

# Inputs for getH()
numThetaValues = 11

# Function to create a peak distribution - only for odd dist
makePeak <- function(numThetaValues) {
  peak = round(numThetaValues / 2)  # Peak of distribution
  beg  = 1:(peak-1)                 # up until the peak
  end  = (peak-1):1                 # After the peak
  return(c(beg, peak, end))         # Combine it to make mountain
}
  
prInput = makePeak(numThetaValues)  # proposal input that is a peak distribution (Q)
hInput = getH(numThetaValues, x, n) # Uniform prior

# Call the function and store
simRQOut <- simRQ(n=10000,h=hInput,pr=prInput,
        
        # Title
        main = paste("Hist. Simulation Output - Assumes Uniform Prior \ndist. over ", numThetaValues, 
                     " values of theta. Prob. ", x, 
                     " with ", n, "bernoulli trials. \nUses Peak Proposal Dist. - Daniel Carpenter"),
        ylab = "Frequency",
        xlab = "Proposed h's",
        col = 'tomato3'
)
```

![](lab5_files/figure-gfm/4-1.png)<!-- -->

``` r
# Show first 20 iterations
simRQOut$iter
```

    ##    prop          u       alpha post
    ## 1     1 1.00000000 1.000000000    1
    ## 2     4 0.47603527 1.000000000    4
    ## 3     6 0.43003151 0.683180600    6
    ## 4     7 0.05079937 0.652298158    7
    ## 5     7 0.87348373 1.000000000    7
    ## 6     8 0.08245497 0.412158966    8
    ## 7     8 0.02671783 1.000000000    8
    ## 8     8 0.21522358 1.000000000    8
    ## 9    10 0.95887817 0.007496876    8
    ## 10    6 0.59950306 1.000000000    6
    ## 11    7 0.67460047 0.652298158    6
    ## 12    9 0.21151735 0.053687091    6
    ## 13    6 0.91096293 1.000000000    6
    ## 14    5 0.63854286 1.000000000    5
    ## 15    5 0.04229324 1.000000000    5
    ## 16    6 0.88116786 0.681351678    5
    ## 17    7 0.75751424 0.444444444    5
    ## 18    8 0.57761170 0.183181763    5
    ## 19    9 0.31458162 0.036579790    5
    ## 20    3 0.16280485 0.585276635    3

------------------------------------------------------------------------

<br>

## Task `5` Continuous Similation using MCMC

### `Part 1-4` Create Function with Beta Proposal

-   Adjusts the output to plot Proposal, Prior, Likelihood, and the
    Posterior
-   Default parameters set to problem for ease

``` r
  ### Using a beta proposal
  ### You can change the proposal to whatever you require
  ## a,b are the parameters of the Beta proposal
  ## a=b=1 is a uniform
  simRC<-function(n=10,init=0.5,a=3,b=4,
                  h = function(theta)
                  { prior = dunif(theta)
                      lik = dbinom(x=4,size=10,prob=theta)
                        h = prior * lik
                        return(c('prior'=prior,'lik'=lik,'h'=h))}
                  )
  {
    #dbeta(x, shape1, shape2, ncp = 0, log = FALSE)
    alpha<-c() # holds transition probs
    alpha[1]<-1
    u<-c() # holds uniform values
    u[1]<-1
    lik<-c()     # liklihood sample
    hOfProp<-c() # prior times lik sample
    post<-c()    # post sample
    prop<-c()    # vec of proposed states 1s and 2s
    prop[1]=init # initial state
    lik[1]=prop[1]
    hOfProp[1]=prop[1]
    post[1]=prop[1]

    q = function(x){dbeta(x,a,b)}
    
    for(i in 2:n){ # starts at 2 because initial value given above
      rbeta(1,a,b)->prop[i]
      
      thisH = h(prop[i])
      
      # Store the prior and liklihood for plotting later
      lik[i]     = thisH['lik']
      hOfProp[i] = thisH['h']
      
      alpha[i]=min(1,hOfProp[i] * q(post[i-1]) / (hOfProp[i-1] * q(prop[i])))
      u[i]=runif(1)
      ifelse(u[i]<=alpha[i],post[i]<-prop[i],post[i]<-post[i-1])
    }
    
    res<-matrix(c(prop,u,alpha,post ),nc=4,nr=n,byrow=FALSE,
                dimnames=list(1:n,c("prop","u","alpha","post")))
    
    
    # Function to generally plot a histogram
    myHist <- function(data, nameOnGraph, color, useFreq=FALSE) {
      hist(data, freq=useFreq, 
           main = paste(nameOnGraph, '\nDaniel Carpenter'), 
           xlab = nameOnGraph, col = color)
    }
    
    # Plot the Proposal
    nameOnGraph = 'Proposal'
    color       = 'darkseagreen3'
    myHist(data=prop, nameOnGraph=nameOnGraph, color=color)
    
    # Plot the Estimated Prior
    nameOnGraph = 'Estimated Prior'
    color       = 'grey90'
    myHist(data=u, nameOnGraph=nameOnGraph, color=color)
    
    # Plot the Estimated Likelihood
    nameOnGraph = 'Estimated Likelihood'
    color       = 'lightblue'
    myHist(data=lik, nameOnGraph=nameOnGraph, color=color)
    
    # Plot the Posterior
    nameOnGraph = 'Posterior'
    color       = 'tomato3'
    myHist(data=post, nameOnGraph=nameOnGraph, color=color)
    
    return(list(matrix=res,summary=summary(post)) )
  }
  
  # Call the function - Default parameters adjusted for part 3 (FYI)
  simRCOut <- simRC(n=10000)
```

![](lab5_files/figure-gfm/5-1.png)<!-- -->![](lab5_files/figure-gfm/5-2.png)<!-- -->![](lab5_files/figure-gfm/5-3.png)<!-- -->![](lab5_files/figure-gfm/5-4.png)<!-- -->

``` r
  # Show the summary output
  simRCOut$summary
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ## 0.02847 0.31309 0.41278 0.41653 0.51625 0.92927

## Task `6` Gibbs Sampling via MCMC

### `6.1` Gibbs Sampling MCMC and `OpenBUGS`

#### What is Gibbs Sampling?

Gibbs sampling is the most popular MCMC algorithm. It allows us to get a
MCMC from a multivariate density. When having access to the full
conditionals, you can use Gibbs sampling to get draws from sophisticated
joint distributions. The process of getting these samples from the joint
distribution are done iteratively.

<br>

#### What is the Gibbs Algorithm?

Assume Bivariate Normal target distribution with correlation
![\\rho](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Crho "\rho")
<br> 1. Initialize:
![\\left(x\_{0}, y\_{0}\\right):=(0,0)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cleft%28x_%7B0%7D%2C%20y_%7B0%7D%5Cright%29%3A%3D%280%2C0%29 "\left(x_{0}, y_{0}\right):=(0,0)")
and set
![t:=0](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;t%3A%3D0 "t:=0")  
2. Draw
![x\_{t}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;x_%7Bt%7D "x_{t}")
from the conditional distribution
![X\_{t} \\mid\\left(Y\_{t-1}=y\_{t-1}\\right) \\sim N\\left(\\rho y\_{t-1}, 1-\\rho^{2}\\right)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;X_%7Bt%7D%20%5Cmid%5Cleft%28Y_%7Bt-1%7D%3Dy_%7Bt-1%7D%5Cright%29%20%5Csim%20N%5Cleft%28%5Crho%20y_%7Bt-1%7D%2C%201-%5Crho%5E%7B2%7D%5Cright%29 "X_{t} \mid\left(Y_{t-1}=y_{t-1}\right) \sim N\left(\rho y_{t-1}, 1-\rho^{2}\right)").  
3. Draw
![y\_{t}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;y_%7Bt%7D "y_{t}")
from the conditional distribution
![Y\_{t} \\mid\\left(X\_{t}=x\_{t}\\right) \\sim N\\left(\\rho x\_{t}, 1-\\rho^{2}\\right)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;Y_%7Bt%7D%20%5Cmid%5Cleft%28X_%7Bt%7D%3Dx_%7Bt%7D%5Cright%29%20%5Csim%20N%5Cleft%28%5Crho%20x_%7Bt%7D%2C%201-%5Crho%5E%7B2%7D%5Cright%29 "Y_{t} \mid\left(X_{t}=x_{t}\right) \sim N\left(\rho x_{t}, 1-\rho^{2}\right)").  
4. Increment
![t:=t+1](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;t%3A%3Dt%2B1 "t:=t+1")  
5. Return to Step 2, etc.

### `6.2-3` - OpenBUGS Model and Pretty Print Output

![Figure: Jags Model - Daniel Carpenter](openBugsModel1.png)

### `6.4-6` Basic SLR Model using JAGS

-   See analysis (part `a.` and `b.`) and the trace and history plots
    after code execution.

``` r
# Sampling MCMC model using SLR

# Optional generic preliminaries:
graphics.off() # This closes all of R's graphics windows.
# rm(list=ls())  # Careful! This clears all of R's memory!

# Load the functions used below:
source("DBDA2E-utilities.R") # Must be in R's current working directory.
```

    ## 
    ## *********************************************************************
    ## Kruschke, J. K. (2015). Doing Bayesian Data Analysis, Second Edition:
    ## A Tutorial with R, JAGS, and Stan. Academic Press / Elsevier.
    ## *********************************************************************

    ## Warning: package 'coda' was built under R version 4.1.3

``` r
require(rjags)               # Must have previously installed package rjags.

fileNameRoot="JAGS_Lab5_Task6_Ouput_Folder/" # For output file names.
dir.create(fileNameRoot)
```

    ## Warning in dir.create(fileNameRoot): 'JAGS_Lab5_Task6_Ouput_Folder' already
    ## exists

``` r
# Load the data:
myData = read.csv("SPRUCE.csv") # Read data file; must be in curr. work. dir.
y = myData$Height        # The y values are in the column named y.
x = myData$BHDiameter        # The y values are in the column named y.
Ntotal = length(y)  # Compute the total number of flips.
dataList = list(    # Put the information into a list.
  x = x,
  y = y,
  n = Ntotal 
)

# Define the model: (from pretty print OpenBUGS model)
modelString = "
model{
    for( i in 1 : n ) {
        y[i] ~ dnorm(mu[i], tau)
        mu[i] <- beta0 + beta1 * x[i]
    }
    beta0 ~ dnorm(0.0, 1.0E-6)
    beta1 ~ dnorm(0.0, 1.0E-6)
    sigma ~ dunif(0, 1000)
    tau <- pow(sigma,  -2)
}
" # close quote for modelString
writeLines( modelString , con="TEMPmodel.txt" )

# Initialize the chains based on MLE of data.
# Option: Use function that generates random values for each chain:
initsList = list(beta0=0, beta1=0, sigma=10)

# Run the chains:'
jagsModel = jags.model( file="TEMPmodel.txt" , data=dataList , inits=initsList , 
                        n.chains=3 , n.adapt=500 )
```

    ## Compiling model graph
    ##    Resolving undeclared variables
    ##    Allocating nodes
    ## Graph information:
    ##    Observed stochastic nodes: 36
    ##    Unobserved stochastic nodes: 3
    ##    Total graph size: 140
    ## 
    ## Initializing model

``` r
# Don't use the first 500 iterations
update( jagsModel , n.iter=500 )

codaSamples = coda.samples( jagsModel , variable.names=c("beta0", "beta1", "sigma"),
                            n.iter=33340 )
save( codaSamples , file=paste0(fileNameRoot,"Mcmc.Rdata") )

# PLOTS =========================================================

  # Examine the chains -----------------------------------------------
    # Beta 0
    filename = paste0(fileNameRoot,"beta0_diagMCMC")
    diagMCMC( codaObject = codaSamples, parName = c("beta0"))
    saveGraph( file=filename , type="png" )
```

![](lab5_files/figure-gfm/6.4-6-1.png)<!-- -->

``` r
    # Beta 1
    filename = paste0(fileNameRoot,"beta1_diagMCMC")
    diagMCMC( codaObject = codaSamples, parName = c("beta1"))
    saveGraph( file=filename , type="png" )
```

![](lab5_files/figure-gfm/6.4-6-2.png)<!-- -->

``` r
    # Sigma
    filename = paste0(fileNameRoot,"sigma_diagMCMC")
    diagMCMC( codaObject = codaSamples, parName = c("sigma"))
    saveGraph( file=filename , type="png" )
```

![](lab5_files/figure-gfm/6.4-6-3.png)<!-- -->

``` r
  # Example the point Estimates --------------------------------------
    # Beta 0
    openGraph()
    par( mar=c(3.5,0.5,2.5,0.5) , mgp=c(2.25,0.7,0) )
    plotPost( codaSamples[,"beta0"] , main="beta0" , xlab=bquote(beta[0]) )
```

![](lab5_files/figure-gfm/6.4-6-4.png)<!-- -->

    ##            ESS     mean   median     mode hdiMass   hdiLow  hdiHigh compVal
    ## beta[0] 3184.6 9.140372 9.141447 9.082752    0.95 6.879379 11.45215      NA
    ##         pGtCompVal ROPElow ROPEhigh pLtROPE pInROPE pGtROPE
    ## beta[0]         NA      NA       NA      NA      NA      NA

``` r
    saveGraph( file=filename , type="png" )
    
    # Beta 1
    openGraph()
    par( mar=c(3.5,0.5,2.5,0.5) , mgp=c(2.25,0.7,0) )
    plotPost( codaSamples[,"beta1"] , main="beta1" , xlab=bquote(beta[1]) )
```

![](lab5_files/figure-gfm/6.4-6-5.png)<!-- -->

    ##              ESS      mean    median     mode hdiMass    hdiLow  hdiHigh
    ## beta[1] 3273.644 0.4818818 0.4817267 0.477457    0.95 0.3604904 0.603814
    ##         compVal pGtCompVal ROPElow ROPEhigh pLtROPE pInROPE pGtROPE
    ## beta[1]      NA         NA      NA       NA      NA      NA      NA

``` r
    saveGraph( file=paste0(fileNameRoot,"beta1_Post") , type="png" )
    
    # Sigma
    openGraph()
    par( mar=c(3.5,0.5,2.5,0.5) , mgp=c(2.25,0.7,0) )
    plotPost( codaSamples[,"sigma"] , main="sigma" , xlab=bquote(sigma) )
```

![](lab5_files/figure-gfm/6.4-6-6.png)<!-- -->

    ##            ESS     mean   median     mode hdiMass   hdiLow  hdiHigh compVal
    ## sigma 41731.17 1.740305 1.718906 1.679205    0.95 1.336861 2.175738      NA
    ##       pGtCompVal ROPElow ROPEhigh pLtROPE pInROPE pGtROPE
    ## sigma         NA      NA       NA      NA      NA      NA

``` r
    saveGraph( file=paste0(fileNameRoot,"sigma_Post") , type="png" )
    

# Summary of all the models (Bayesian Point interval estimates)
summary(codaSamples)
```

    ## 
    ## Iterations = 1001:34340
    ## Thinning interval = 1 
    ## Number of chains = 3 
    ## Sample size per chain = 33340 
    ## 
    ## 1. Empirical mean and standard deviation for each variable,
    ##    plus standard error of the mean:
    ## 
    ##         Mean      SD  Naive SE Time-series SE
    ## beta0 9.1404 1.16248 0.0036757       0.020496
    ## beta1 0.4819 0.06188 0.0001957       0.001087
    ## sigma 1.7403 0.21988 0.0006952       0.001073
    ## 
    ## 2. Quantiles for each variable:
    ## 
    ##         2.5%    25%    50%   75%   97.5%
    ## beta0 6.8556 8.3713 9.1414 9.909 11.4300
    ## beta1 0.3598 0.4408 0.4817 0.523  0.6033
    ## sigma 1.3721 1.5851 1.7189 1.871  2.2291

<br>

#### **Analysis:** For all variables:

-   Param. Values seem to be around the mean value, which is stationary
-   Autocorrelation drops quickly for
    ![\\sigma](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Csigma "\sigma"),
    which provides good estimates. However, this does not occur with
    ![\\beta_0](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cbeta_0 "\beta_0")
    or
    ![\\beta_1](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cbeta_1 "\beta_1")
-   Shrink factor evens out
-   ![\\sigma](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Csigma "\sigma")’s
    3 densities overlay each other, so they are superimposed. Slight
    skewness for
    ![\\beta](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cbeta "\beta")’s

### `6.7` Compare classical tests using the `lm()` function

``` r
# Form the linear model using SPRUCE Dataset
est <- lm(Height~BHDiameter, data = myData)

# Get point estimates
summary(est)
```

    ## 
    ## Call:
    ## lm(formula = Height ~ BHDiameter, data = myData)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.9394 -0.9763  0.2829  0.9950  2.6644 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  9.14684    1.12131   8.157 1.63e-09 ***
    ## BHDiameter   0.48147    0.05967   8.069 2.09e-09 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.678 on 34 degrees of freedom
    ## Multiple R-squared:  0.6569, Adjusted R-squared:  0.6468 
    ## F-statistic:  65.1 on 1 and 34 DF,  p-value: 2.089e-09

``` r
# Confidence Intervals
library(s20x)
ciReg(est)
```

    ##             95 % C.I.lower    95 % C.I.upper
    ## (Intercept)        6.86806          11.42562
    ## BHDiameter         0.36020           0.60275

<br>

#### **Analysis:** Point Estimates:

-   Classical estimated mean and standard error’s for
    ![beta](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;beta "beta")
    values fall very close to the Bayesian estimates.

<br>

### `6.8` Adding ![beta^{2}\_{2}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;beta%5E%7B2%7D_%7B2%7D "beta^{2}_{2}") to Model

#### Open Bugs Model Pretty Print Output using ![beta^{2}\_{2}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;beta%5E%7B2%7D_%7B2%7D "beta^{2}_{2}")

![Figure: Jags Model using
![beta^{2}\_{2}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;beta%5E%7B2%7D_%7B2%7D "beta^{2}_{2}") -
Daniel Carpenter](openBugsModel2.png)

<br>

#### Jags Function and Ouptut

``` r
# Sampling MCMC model using SLR with squared value

# Optional generic preliminaries:
graphics.off() # This closes all of R's graphics windows.

fileNameRoot="JAGS_Lab5_Task6.8_Ouput_Folder/" # For output file names.
dir.create(fileNameRoot)
```

    ## Warning in dir.create(fileNameRoot): 'JAGS_Lab5_Task6.8_Ouput_Folder' already
    ## exists

``` r
# Load the data:
# myData = read.csv("SPRUCE.csv") # Read data file; must be in curr. work. dir.
# y = myData$Height        # The y values are in the column named y.
# x = myData$BHDiameter        # The y values are in the column named y.
Ntotal = length(y)  # Compute the total number of flips.
dataList = list(    # Put the information into a list.
  x = x,
  y = y,
  n = Ntotal 
)

# Define the model: (from pretty print OpenBUGS model)
modelString = "
model{
    for( i in 1 : n ) {
        y[i] ~ dnorm(mu[i], tau)
        mu[i] <- beta0 + beta1 * x[i] + beta2 * x[i]^2
    }
    beta0 ~ dnorm(0.0, 1.0E-6)
    beta1 ~ dnorm(0.0, 1.0E-6)
    beta2 ~ dnorm(0.0, 1.0E-6)
    sigma ~ dunif(0, 1000)
    tau <- pow(sigma,  -2)
}

" # close quote for modelString
writeLines( modelString , con="TEMPmodel.txt" )

# Initialize the chains based on MLE of data.
# Option: Use function that generates random values for each chain:
initsList = list(beta0=0, beta1=0, beta2=0, sigma=10)

# Run the chains:'
jagsModel = jags.model( file="TEMPmodel.txt" , data=dataList , inits=initsList , 
                        n.chains=3 , n.adapt=500 )
```

    ## Compiling model graph
    ##    Resolving undeclared variables
    ##    Allocating nodes
    ## Graph information:
    ##    Observed stochastic nodes: 36
    ##    Unobserved stochastic nodes: 4
    ##    Total graph size: 199
    ## 
    ## Initializing model

``` r
# Don't use the first 500 iterations
update( jagsModel , n.iter=500 )

codaSamples = coda.samples( jagsModel , variable.names=c("beta0", "beta1", "beta2", "sigma"),
                            n.iter=33340 )
save( codaSamples , file=paste0(fileNameRoot,"Mcmc.Rdata") )

# PLOTS =========================================================

  # Examine the chains -----------------------------------------------
    # Beta 0
    filename = paste0(fileNameRoot,"beta0_diagMCMC")
    diagMCMC( codaObject = codaSamples, parName = c("beta0"))
    saveGraph( file=filename , type="png" )
```

![](lab5_files/figure-gfm/6.5-1.png)<!-- -->

``` r
    # Beta 1
    filename = paste0(fileNameRoot,"beta1_diagMCMC")
    diagMCMC( codaObject = codaSamples, parName = c("beta1"))
    saveGraph( file=filename , type="png" )
```

![](lab5_files/figure-gfm/6.5-2.png)<!-- -->

``` r
    # Beta 2
    filename = paste0(fileNameRoot,"beta2_diagMCMC")
    diagMCMC( codaObject = codaSamples, parName = c("beta2"))
    saveGraph( file=filename , type="png" )
```

![](lab5_files/figure-gfm/6.5-3.png)<!-- -->

``` r
    # Sigma
    filename = paste0(fileNameRoot,"sigma_diagMCMC")
    diagMCMC( codaObject = codaSamples, parName = c("sigma"))
    saveGraph( file=filename , type="png" )
```

![](lab5_files/figure-gfm/6.5-4.png)<!-- -->

``` r
  # Example the point Estimates --------------------------------------
    # Beta 0
    openGraph()
    par( mar=c(3.5,0.5,2.5,0.5) , mgp=c(2.25,0.7,0) )
    plotPost( codaSamples[,"beta0"] , main="beta0" , xlab=bquote(beta[0]) )
```

![](lab5_files/figure-gfm/6.5-5.png)<!-- -->

    ##              ESS     mean   median      mode hdiMass    hdiLow  hdiHigh compVal
    ## beta[0] 210.9158 1.065652 1.028113 0.7746207    0.95 -3.230195 5.650779      NA
    ##         pGtCompVal ROPElow ROPEhigh pLtROPE pInROPE pGtROPE
    ## beta[0]         NA      NA       NA      NA      NA      NA

``` r
    saveGraph( file=filename , type="png" )
    
    # Beta 1
    openGraph()
    par( mar=c(3.5,0.5,2.5,0.5) , mgp=c(2.25,0.7,0) )
    plotPost( codaSamples[,"beta1"] , main="beta1" , xlab=bquote(beta[1]) )
```

![](lab5_files/figure-gfm/6.5-6.png)<!-- -->

    ##              ESS     mean   median     mode hdiMass    hdiLow  hdiHigh compVal
    ## beta[1] 181.7403 1.445823 1.449469 1.495487    0.95 0.9338267 1.920567      NA
    ##         pGtCompVal ROPElow ROPEhigh pLtROPE pInROPE pGtROPE
    ## beta[1]         NA      NA       NA      NA      NA      NA

``` r
    saveGraph( file=paste0(fileNameRoot,"beta1_Post") , type="png" )
    
    # Beta 2
    openGraph()
    par( mar=c(3.5,0.5,2.5,0.5) , mgp=c(2.25,0.7,0) )
    plotPost( codaSamples[,"beta2"] , main="beta2" , xlab=bquote(beta[1]) )
```

![](lab5_files/figure-gfm/6.5-7.png)<!-- -->

    ##              ESS        mean      median        mode hdiMass     hdiLow
    ## beta[1] 197.7463 -0.02681243 -0.02685869 -0.02568459    0.95 -0.0396694
    ##             hdiHigh compVal pGtCompVal ROPElow ROPEhigh pLtROPE pInROPE pGtROPE
    ## beta[1] -0.01288126      NA         NA      NA       NA      NA      NA      NA

``` r
    saveGraph( file=paste0(fileNameRoot,"beta2_Post") , type="png" )
    
    # Sigma
    openGraph()
    par( mar=c(3.5,0.5,2.5,0.5) , mgp=c(2.25,0.7,0) )
    plotPost( codaSamples[,"sigma"] , main="sigma" , xlab=bquote(sigma) )
```

![](lab5_files/figure-gfm/6.5-8.png)<!-- -->

    ##           ESS    mean   median     mode hdiMass   hdiLow  hdiHigh compVal
    ## sigma 17044.6 1.43615 1.417396 1.378303    0.95 1.098391 1.811598      NA
    ##       pGtCompVal ROPElow ROPEhigh pLtROPE pInROPE pGtROPE
    ## sigma         NA      NA       NA      NA      NA      NA

``` r
    saveGraph( file=paste0(fileNameRoot,"sigma_Post") , type="png" )
    

# Summary of all the models (Bayesian Point interval estimates)
summary(codaSamples)
```

    ## 
    ## Iterations = 1001:34340
    ## Thinning interval = 1 
    ## Number of chains = 3 
    ## Sample size per chain = 33340 
    ## 
    ## 1. Empirical mean and standard deviation for each variable,
    ##    plus standard error of the mean:
    ## 
    ##           Mean       SD  Naive SE Time-series SE
    ## beta0  1.06565 2.242803 7.092e-03      0.1490877
    ## beta1  1.44582 0.247735 7.833e-04      0.0186260
    ## beta2 -0.02681 0.006755 2.136e-05      0.0004748
    ## sigma  1.43615 0.186252 5.889e-04      0.0013565
    ## 
    ## 2. Quantiles for each variable:
    ## 
    ##           2.5%      25%      50%      75%    97.5%
    ## beta0 -3.33065 -0.39136  1.02811  2.53529  5.55626
    ## beta1  0.94183  1.28516  1.44947  1.60851  1.93014
    ## beta2 -0.03999 -0.03127 -0.02686 -0.02244 -0.01317
    ## sigma  1.12664  1.30453  1.41740  1.54726  1.85399

<br>

#### Clasical Methods using `lm()`

``` r
# Form the linear model using SPRUCE Dataset
est2 <- lm(Height ~ BHDiameter + I(BHDiameter^2), data = myData)

# Get point estimates
summary(est2)
```

    ## 
    ## Call:
    ## lm(formula = Height ~ BHDiameter + I(BHDiameter^2), data = myData)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.2966 -0.6245 -0.0707  0.7442  3.2541 
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)      0.860896   2.205022   0.390 0.698731    
    ## BHDiameter       1.469592   0.243786   6.028 8.88e-07 ***
    ## I(BHDiameter^2) -0.027457   0.006635  -4.138 0.000227 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.382 on 33 degrees of freedom
    ## Multiple R-squared:  0.7741, Adjusted R-squared:  0.7604 
    ## F-statistic: 56.55 on 2 and 33 DF,  p-value: 2.182e-11

``` r
# Confidence Intervals
ciReg(est2)
```

    ##                 95 % C.I.lower    95 % C.I.upper
    ## (Intercept)           -3.62525           5.34705
    ## BHDiameter             0.97361           1.96558
    ## I(BHDiameter^2)       -0.04096          -0.01396

<br>

### `6.9` Comparison of results

-   Both the classical and the Bayesian estimate are very close to each
    other when comparing mean and standard deviation estimations.
-   Interestingly, the new model including
    ![beta^{2}\_{2}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;beta%5E%7B2%7D_%7B2%7D "beta^{2}_{2}"),
    sees that the Bayesian estimate experiences a high level of
    autocorrelation with both
    ![beta\_{1}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;beta_%7B1%7D "beta_{1}")
    and
    ![beta^{2}\_{2}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;beta%5E%7B2%7D_%7B2%7D "beta^{2}_{2}").
