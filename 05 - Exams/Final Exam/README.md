Comprehensive Final Exam
================
Daniel Carpenter
May 2022

-   [Question 1](#question-1)
    -   [1. (a) General Posterior](#1-a-general-posterior)
    -   [1. (d) If a further experiment is made after the first using
        the same coin, this time with n = 20, x = 12, what prior should
        the researcher use in the absence of any other information than
        what is given in this
        problem?](#1-d-if-a-further-experiment-is-made-after-the-first-using-the-same-coin-this-time-with-n--20-x--12-what-prior-should-the-researcher-use-in-the-absence-of-any-other-information-than-what-is-given-in-this-problem)
    -   [1. (e) Find the posterior after the second sequential
        experiment.](#1-e-find-the-posterior-after-the-second-sequential-experiment)
    -   [1. (f) The researcher now wishes to plot the posterior (that
        which is formed from the two experiments). Fill in the gaps of
        the code so that the correct plot is
        created.](#1-f-the-researcher-now-wishes-to-plot-the-posterior-that-which-is-formed-from-the-two-experiments-fill-in-the-gaps-of-the-code-so-that-the-correct-plot-is-created)
-   [Question 2 - Probability](#question-2---probability)
    -   [2. a) P(X \> 0.7\|α = 2,
        ![\\beta](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cbeta "\beta")
        = 3)](#2-a-px--07α--2-beta--3)
    -   [2. b) P(X \< 0.2\|α = 1,
        ![\\beta](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cbeta "\beta")
        = 1)](#2-b-px--02α--1-beta--1)
    -   [2. c) Find the value of X with lower tail probability 0.04. X ∼
        Beta(5,
        4)](#2-c-find-the-value-of-x-with-lower-tail-probability-004-x--beta5-4)
    -   [2. d) Find the equal tail interval that contains 95% of the
        distribution, X ∼ Beta(4,
        8)](#2-d-find-the-equal-tail-interval-that-contains-95-of-the-distribution-x--beta4-8)
    -   [2. e) Generate a random sample of X values of size 20 with X ∼
        Beta(5,
        7)](#2-e-generate-a-random-sample-of-x-values-of-size-20-with-x--beta5-7)
-   [Question 3 - Interpret
    Posteriors](#question-3---interpret-posteriors)
    -   [3. a) Give the 95% HDI for
        ![\\theta](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Ctheta "\theta")
        the probability of a
        success.](#3-a-give-the-95-hdi-for-theta-the-probability-of-a-success)
    -   [3. b) What is the posterior probability that
        ![\\theta](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Ctheta "\theta")
        \< 0.5](#3-b-what-is-the-posterior-probability-that-theta--05)
    -   [3. c) If H0 :
        ![\\theta](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Ctheta "\theta")
        = 0.5 should we reject it (the NULL),
        yes/no?](#3-c-if-h0--theta--05-should-we-reject-it-the-null-yesno)
    -   [3. d) The posterior probability that
        ![\\theta](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Ctheta "\theta")
        lies in (0.203, 0.413) is 0.90. True or
        False?](#3-d-the-posterior-probability-that-theta-lies-in-0203-0413-is-090-true-or-false)
    -   [3. e) What does ROPE stand for?](#3-e-what-does-rope-stand-for)
-   [Question 4 - GLM](#question-4---glm)
    -   [4. a) Link function?](#4-a-link-function)
    -   [4. b) What numerical values will replace the y
        variable?](#4-b-what-numerical-values-will-replace-the-y-variable)
    -   [4. c) High impact priors?](#4-c-high-impact-priors)
    -   [4. d) Interpret Point/Interval
        Estimates](#4-d-interpret-pointinterval-estimates)
-   [Question 5 - Exponential Family](#question-5---exponential-family)
    -   [5. a) Show
        ![E(Y) = b^{\\prime}(\\theta)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;E%28Y%29%20%3D%20b%5E%7B%5Cprime%7D%28%5Ctheta%29 "E(Y) = b^{\prime}(\theta)")](#5-a-show-ey--bprimetheta)
    -   [5. b) Show
        ![V(Y) = b^{\\prime \\prime}(\\theta) a(\\phi)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;V%28Y%29%20%3D%20b%5E%7B%5Cprime%20%5Cprime%7D%28%5Ctheta%29%20a%28%5Cphi%29 "V(Y) = b^{\prime \prime}(\theta) a(\phi)")](#5-b-show-vy--bprime-primetheta-aphi)
    -   [5. c) Show that the `Binomial` distribution belongs to the
        `exponential`
        family](#5-c-show-that-the-binomial-distribution-belongs-to-the-exponential-family)
    -   [5. d) Show
        ![E(Y) = np](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;E%28Y%29%20%3D%20np "E(Y) = np")](#5-d-show-ey--np)
    -   [5. e) Show
        ![V(Y) = np(1−p)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;V%28Y%29%20%3D%20np%281%E2%88%92p%29 "V(Y) = np(1−p)")](#5-e-show-vy--np1p)
    -   [

        ![ V(Y) = n p (1 - p)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%20V%28Y%29%20%3D%20n%20p%20%281%20-%20p%29 " V(Y) = n p (1 - p)")

        ](#-vy--n-p-1---p)
-   [Question 6 - Derive Evidence](#question-6---derive-evidence)
-   [Question 7 - 2 State MCMC](#question-7---2-state-mcmc)
    -   [7. a) Find the value of `A`](#7-a-find-the-value-of-a)
    -   [7. b) Find the value of `B`](#7-b-find-the-value-of-b)
    -   [7. c) Find the value of `C`](#7-c-find-the-value-of-c)
    -   [7. d) Formula for the acceptance
        probability](#7-d-formula-for-the-acceptance-probability)
-   [Question 8 - SLR JAGS](#question-8---slr-jags)
    -   [8. a) SLR Doodle Bug](#8-a-slr-doodle-bug)
    -   [8. b) JAGS Code](#8-b-jags-code)
    -   [8. c) Logical Node?](#8-c-logical-node)
    -   [8. d) Linear Predictors?](#8-d-linear-predictors)
-   [Question 9 - Change Point
    Regression](#question-9---change-point-regression)
    -   [9. b) JAGS Piecewise](#9-b-jags-piecewise)
-   [Question 10 - Centering Data](#question-10---centering-data)
    -   [10. a) Plot data](#10-a-plot-data)
    -   [10. b) 2 SLR Jags Models](#10-b-2-slr-jags-models)
    -   [10. c) Posterior Density Plots](#10-c-posterior-density-plots)
    -   [10. d) Differing Intervals
        Interpretation](#10-d-differing-intervals-interpretation)
    -   [10. e) Classical estimates of the two
        models](#10-e-classical-estimates-of-the-two-models)

------------------------------------------------------------------------

<br>

# Question 1

## 1. (a) General Posterior

*Adapted from JK’s book - page 132 Doing Bayesian Data Analysis:* <br>

If
![\\theta \\sim \\operatorname{Beta}(\\alpha, \\beta)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Ctheta%20%5Csim%20%5Coperatorname%7BBeta%7D%28%5Calpha%2C%20%5Cbeta%29 "\theta \sim \operatorname{Beta}(\alpha, \beta)")
and
![X \\sim \\operatorname{Bin}(n, \\theta)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;X%20%5Csim%20%5Coperatorname%7BBin%7D%28n%2C%20%5Ctheta%29 "X \sim \operatorname{Bin}(n, \theta)"),
prove that
![\\theta \\mid X \\sim \\operatorname{Beta}(x+\\alpha, n-x+\\beta)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Ctheta%20%5Cmid%20X%20%5Csim%20%5Coperatorname%7BBeta%7D%28x%2B%5Calpha%2C%20n-x%2B%5Cbeta%29 "\theta \mid X \sim \operatorname{Beta}(x+\alpha, n-x+\beta)")
through proof below:

![p(\\theta \\mid x) \\propto p(\\theta) p(x \\mid \\theta) = \\frac{1}{B(\\alpha, \\beta)} \\theta^{\\alpha-1}(1-\\theta)^{\\beta-1}\\left(\\begin{array}{l}n \\\\ x\\end{array}\\right) \\theta^{x}(1-\\theta)^{n-x}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;p%28%5Ctheta%20%5Cmid%20x%29%20%5Cpropto%20p%28%5Ctheta%29%20p%28x%20%5Cmid%20%5Ctheta%29%20%3D%20%5Cfrac%7B1%7D%7BB%28%5Calpha%2C%20%5Cbeta%29%7D%20%5Ctheta%5E%7B%5Calpha-1%7D%281-%5Ctheta%29%5E%7B%5Cbeta-1%7D%5Cleft%28%5Cbegin%7Barray%7D%7Bl%7Dn%20%5C%5C%20x%5Cend%7Barray%7D%5Cright%29%20%5Ctheta%5E%7Bx%7D%281-%5Ctheta%29%5E%7Bn-x%7D "p(\theta \mid x) \propto p(\theta) p(x \mid \theta) = \frac{1}{B(\alpha, \beta)} \theta^{\alpha-1}(1-\theta)^{\beta-1}\left(\begin{array}{l}n \\ x\end{array}\right) \theta^{x}(1-\theta)^{n-x}")

Note Bayes’ rule  
![\\underbrace{p(\\theta \\mid x)}\_{Posterior} \\propto \\underbrace{p(\\theta)}\_{Prior} \\underbrace{p(x \\mid \\theta)}\_{Lik.} = \\frac{p(x, n \\mid \\theta) p(\\theta)}{p(x, n)} = p(\\theta \\mid x, n)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cunderbrace%7Bp%28%5Ctheta%20%5Cmid%20x%29%7D_%7BPosterior%7D%20%5Cpropto%20%5Cunderbrace%7Bp%28%5Ctheta%29%7D_%7BPrior%7D%20%5Cunderbrace%7Bp%28x%20%5Cmid%20%5Ctheta%29%7D_%7BLik.%7D%20%3D%20%5Cfrac%7Bp%28x%2C%20n%20%5Cmid%20%5Ctheta%29%20p%28%5Ctheta%29%7D%7Bp%28x%2C%20n%29%7D%20%3D%20p%28%5Ctheta%20%5Cmid%20x%2C%20n%29 "\underbrace{p(\theta \mid x)}_{Posterior} \propto \underbrace{p(\theta)}_{Prior} \underbrace{p(x \mid \theta)}_{Lik.} = \frac{p(x, n \mid \theta) p(\theta)}{p(x, n)} = p(\theta \mid x, n)")
<br>

Define Bernoulli and beta distributions  
![=\\underbrace{\\theta^{x}(1-\\theta)^{(n-x)}}\_{Bernoulli \\ Lik.} \\underbrace{\\frac{\\theta^{(\\alpha-1)}(1-\\theta)^{(\\beta-1)}}{B(\\alpha, \\beta)}}\_{Beta \\ Prior} / p(x, n)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%3D%5Cunderbrace%7B%5Ctheta%5E%7Bx%7D%281-%5Ctheta%29%5E%7B%28n-x%29%7D%7D_%7BBernoulli%20%5C%20Lik.%7D%20%5Cunderbrace%7B%5Cfrac%7B%5Ctheta%5E%7B%28%5Calpha-1%29%7D%281-%5Ctheta%29%5E%7B%28%5Cbeta-1%29%7D%7D%7BB%28%5Calpha%2C%20%5Cbeta%29%7D%7D_%7BBeta%20%5C%20Prior%7D%20%2F%20p%28x%2C%20n%29 "=\underbrace{\theta^{x}(1-\theta)^{(n-x)}}_{Bernoulli \ Lik.} \underbrace{\frac{\theta^{(\alpha-1)}(1-\theta)^{(\beta-1)}}{B(\alpha, \beta)}}_{Beta \ Prior} / p(x, n)")
<br>

Rearrange factors  
![=\\frac{1}{B(\\alpha, \\beta) p(x, n)} \\theta^{(\\alpha-1)} (1-\\theta)^{(\\beta-1)} \\theta^{x}(1-\\theta)^{(n-x)}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%3D%5Cfrac%7B1%7D%7BB%28%5Calpha%2C%20%5Cbeta%29%20p%28x%2C%20n%29%7D%20%5Ctheta%5E%7B%28%5Calpha-1%29%7D%20%281-%5Ctheta%29%5E%7B%28%5Cbeta-1%29%7D%20%5Ctheta%5E%7Bx%7D%281-%5Ctheta%29%5E%7B%28n-x%29%7D "=\frac{1}{B(\alpha, \beta) p(x, n)} \theta^{(\alpha-1)} (1-\theta)^{(\beta-1)} \theta^{x}(1-\theta)^{(n-x)}")
<br>

![=\\frac{1}{B(\\alpha, \\beta)} \\theta^{(\\alpha-1)} (1-\\theta)^{(\\beta-1)} \\left(\\begin{array}{l}\\frac{1}{p(x, n)}\\end{array}\\right)\\theta^{x}(1-\\theta)^{(n-x)}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%3D%5Cfrac%7B1%7D%7BB%28%5Calpha%2C%20%5Cbeta%29%7D%20%5Ctheta%5E%7B%28%5Calpha-1%29%7D%20%281-%5Ctheta%29%5E%7B%28%5Cbeta-1%29%7D%20%5Cleft%28%5Cbegin%7Barray%7D%7Bl%7D%5Cfrac%7B1%7D%7Bp%28x%2C%20n%29%7D%5Cend%7Barray%7D%5Cright%29%5Ctheta%5E%7Bx%7D%281-%5Ctheta%29%5E%7B%28n-x%29%7D "=\frac{1}{B(\alpha, \beta)} \theta^{(\alpha-1)} (1-\theta)^{(\beta-1)} \left(\begin{array}{l}\frac{1}{p(x, n)}\end{array}\right)\theta^{x}(1-\theta)^{(n-x)}")
<br>

By definition of the binomial coefficient, which we arrive at the
**solution**:  
![=\\frac{1}{B(\\alpha, \\beta)} \\theta^{(\\alpha-1)} (1-\\theta)^{(\\beta-1)} \\left(\\begin{array}{l}n \\\\ x\\end{array}\\right) \\theta^{x}(1-\\theta)^{(n-x)}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%3D%5Cfrac%7B1%7D%7BB%28%5Calpha%2C%20%5Cbeta%29%7D%20%5Ctheta%5E%7B%28%5Calpha-1%29%7D%20%281-%5Ctheta%29%5E%7B%28%5Cbeta-1%29%7D%20%5Cleft%28%5Cbegin%7Barray%7D%7Bl%7Dn%20%5C%5C%20x%5Cend%7Barray%7D%5Cright%29%20%5Ctheta%5E%7Bx%7D%281-%5Ctheta%29%5E%7B%28n-x%29%7D "=\frac{1}{B(\alpha, \beta)} \theta^{(\alpha-1)} (1-\theta)^{(\beta-1)} \left(\begin{array}{l}n \\ x\end{array}\right) \theta^{x}(1-\theta)^{(n-x)}")
<br>

------------------------------------------------------------------------

<br>

## 1. (b) If ![\\alpha](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Calpha "\alpha") = ![\\beta](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cbeta "\beta") = 1 what prior distribution does this correspond to?

-   Shape 1 and Shape two with values of 1 create a `uniform`
    distribution.

## 1. (c) Suppose she uses ![\\alpha](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Calpha "\alpha") = ![\\beta](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cbeta "\beta") = 5 for the prior what is the posterior distribution?

-   Mixing a beta-prior and bernoulli likelihood Creates a `Beta` prior
    and posterior\`

-   n=10, x=4,
    ![\\alpha](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Calpha "\alpha")
    =
    ![\\beta](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cbeta "\beta")
    = 5

``` r
# These need to be in your document directory to load
source("DBDA2E-utilities.R")  # Load definitions of graphics functions etc.
source("BernBeta.R")          # Load the definition of the BernBeta function

# Specify the prior:
t = 4              # Specify the prior MODE.
n = 10             # Specify the effective prior sample size.
a = 5              # Convert to beta shape parameter a.
b = 5              # Convert to beta shape parameter b.

Prior = c(a,b)     # Specify Prior as vector with the two shape parameters.

# Specify the data:
N = 10                        # The total number of flips.
x = 4                         # The number of heads.
Data = c(rep(0,N-x),rep(1,x)) # Convert N and z into vector of 0's and 1's.


# Plot the three graphs, and >> VIEW THE POSTERIOR <<:
posterior = BernBeta( priorBetaAB=Prior, Data=Data , plotType="Bars" , 
                      showCentTend="Mode" , showHDI=TRUE , showpD=FALSE )
```

![](FinalExam_files/figure-gfm/1c-1.png)<!-- -->

## 1. (d) If a further experiment is made after the first using the same coin, this time with n = 20, x = 12, what prior should the researcher use in the absence of any other information than what is given in this problem?

-   Since there is no additional information, use hyper parameters
    ![\\alpha](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Calpha "\alpha")
    =
    ![\\beta](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cbeta "\beta")
    = 1 for this experiment.
-   In question 1(f) we will mix the betas to get a more informed result

## 1. (e) Find the posterior after the second sequential experiment.

-   Mixing a beta-prior and bernoulli likelihood Creates a
    `Beta posterior`
-   n=20, x=12,
    ![\\alpha](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Calpha "\alpha")
    =
    ![\\beta](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cbeta "\beta")
    = 1

``` r
# These need to be in your document directory to load
source("DBDA2E-utilities.R")  # Load definitions of graphics functions etc.
source("BernBeta.R")          # Load the definition of the BernBeta function

# Specify the prior:
t = 12              # Specify the prior MODE.
n = 20             # Specify the effective prior sample size.
a = 1              # Convert to beta shape parameter a.
b = 1              # Convert to beta shape parameter b.

Prior = c(a,b)     # Specify Prior as vector with the two shape parameters.

# Specify the data:
N = 20                        # The total number of flips.
x = 12                         # The number of heads.
Data = c(rep(0,N-x),rep(1,x)) # Convert N and z into vector of 0's and 1's.


# Plot the three graphs, and >> VIEW THE POSTERIOR <<:
posterior2 = BernBeta( priorBetaAB=Prior, Data=Data , plotType="Bars" , 
                       showCentTend="Mode" , showHDI=TRUE , showpD=FALSE )
```

![](FinalExam_files/figure-gfm/1e-1.png)<!-- -->

## 1. (f) The researcher now wishes to plot the posterior (that which is formed from the two experiments). Fill in the gaps of the code so that the correct plot is created.

### Mixed beta Calculation:

![
w \\times dbeta(x, a1, b1) + (1-w) \\times dbeta(x, a2,b2)
](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0Aw%20%5Ctimes%20dbeta%28x%2C%20a1%2C%20b1%29%20%2B%20%281-w%29%20%5Ctimes%20dbeta%28x%2C%20a2%2Cb2%29%0A "
w \times dbeta(x, a1, b1) + (1-w) \times dbeta(x, a2,b2)
")

### Mixed Beta function

``` r
Mymixbeta <- function(w=0.5, n=10, x, a1=1, a2=1, b1=1, b2=1) {
  # Specify the prior:
  t = x                          # Specify the prior MODE.
  
  # Specify the data:
  N = n                          # The total number of flips.
  z = x                          # The number of heads.
  Data = c(rep(0,N-z),rep(1,z))  # Convert N and z into vector of 0's and 1's.
  
  # Create summary values of Data:
  z = sum( Data ) # number of 1's in Data
  N = length( Data ) 
  
  Theta = seq(0.001,0.999,by=0.001)                                                 # points for plotting
  pTheta = w*dbeta(Theta,a1,b1) +(1-w)*dbeta(Theta,a2,b2)                           # prior for plotting
  pThetaGivenData = w*dbeta(Theta, a1+z, b1+N-z) + (1-w)*dbeta(Theta, a2+z, b2+N-z) # posterior for plotting
  pDataGivenTheta = Theta^z * (1-Theta)^(N-z)                                       # likelihood for plotting
  
  
  # Plot Layout
  layout( matrix( c( 1,2,3 ) ,nrow=3 ,ncol=1 ,byrow=FALSE ) ) # 3x1 panels
  par( mar=c(3,3,1,0) , mgp=c(2,0.7,0) , mai=c(0.5,0.5,0.3,0.1) ) # margins
  cexAxis = 1.33
  cexLab = 1.75
  
  # convert plotType to notation used by plot:
  plotType="h"
  dotsize = 5 # how big to make the plotted dots
  barsize = 5 # how wide to make the bar lines   
  
  # y limits for prior and posterior:
  yLim = c(0,1.1*max(c(pTheta,pThetaGivenData)))
  
  
  # Plot the Prior
  plot( Theta , pTheta , type=plotType , 
        pch="." , cex=dotsize , lwd=barsize ,
        xlim=c(0,1) , ylim=yLim , cex.axis=cexAxis ,
        xlab=bquote(theta) , ylab='Mixed Beta' , 
        cex.lab=cexLab ,
        main="Prior (beta) - Daniel Carpenter" , cex.main=1.5 , col="skyblue")
  
  # Plot the likelihood
  plot( Theta , pDataGivenTheta , type=plotType , 
        pch="." , cex=dotsize , lwd=barsize ,
        xlim=c(0,1) , ylim=c(0,1.1*max(pDataGivenTheta)) , cex.axis=cexAxis ,
        xlab=bquote(theta) , ylab=bquote( "p(D|" * theta * ")" ) , 
        cex.lab=cexLab ,
        main="Likelihood (Bernoulli)" , cex.main=1.5 , col="skyblue" )
  
  # Plot the posterior.
  plot( Theta , pThetaGivenData , type=plotType , 
        pch="." , cex=dotsize , lwd=barsize ,
        xlim=c(0,1) , ylim=yLim , cex.axis=cexAxis ,
        xlab=bquote(theta) , ylab='Mixed Beta Posterior' , 
        cex.lab=cexLab ,
        main="Posterior (beta)" , cex.main=1.5 , col="skyblue" )
}

# Call the function and print the prior (mixed beta), likelihood and posterior)
Mymixbeta(w=0.5, n=20, x=12, a1=1, a2=1, b1=5, b2=5)
```

![](FinalExam_files/figure-gfm/1f-1.png)<!-- -->

<!-- ### 1. f, i) A = ... -->
<!-- ### 1. f, ii) B = ... -->

------------------------------------------------------------------------

<br>

# Question 2 - Probability

## 2. a) P(X \> 0.7\|α = 2, ![\\beta](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cbeta "\beta") = 3)

``` r
pbeta(q = 0.7, 
      shape1 = 2, shape2 = 3,
      lower.tail = FALSE) # >
```

    ## [1] 0.0837

## 2. b) P(X \< 0.2\|α = 1, ![\\beta](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cbeta "\beta") = 1)

``` r
pbeta(q = 0.2, 
      shape1 = 1, shape2 = 1,
      lower.tail = TRUE) # <
```

    ## [1] 0.2

## 2. c) Find the value of X with lower tail probability 0.04. X ∼ Beta(5, 4)

``` r
qbeta(p = 0.04, 
      shape1 = 5, shape2 = 4, 
      lower.tail = TRUE) # <
```

    ## [1] 0.2739266

## 2. d) Find the equal tail interval that contains 95% of the distribution, X ∼ Beta(4, 8)

``` r
alphaCI = 0.05 / 2  # 2.5% upper/lower
a = 4 # shape 1 
b = 8 # shape 2

# Upper tail 97.5%
upperTail = qbeta(p = alphaCI, 
                  shape1=a, shape2=b, 
                  lower.tail = FALSE) # <

# Lower tail 2.5%
lowerTail = qbeta(p = alphaCI, 
                  shape1=a, shape2=b, 
                  lower.tail = TRUE) # >
# Equal tail interval: 95% confidence between 2.5% and 97.5%
paste0('There is a ', (1 - alphaCI * 2) * 100, '% Probability that the value will fall between ', 
      round(lowerTail, 3),' and ', round(upperTail, 3))
```

    ## [1] "There is a 95% Probability that the value will fall between 0.109 and 0.61"

## 2. e) Generate a random sample of X values of size 20 with X ∼ Beta(5, 7)

``` r
randBetaDist <- rbeta(n = 20, 
                      shape1 = 5,
                      shape2 = 7)

# Print the data
library(knitr)
kable(randBetaDist)
```

|         x |
|----------:|
| 0.4894074 |
| 0.2980459 |
| 0.6436076 |
| 0.3092375 |
| 0.5896615 |
| 0.6643829 |
| 0.6308936 |
| 0.2119625 |
| 0.3140222 |
| 0.2703963 |
| 0.5026362 |
| 0.3362534 |
| 0.6083700 |
| 0.5858480 |
| 0.4687701 |
| 0.4072835 |
| 0.7100258 |
| 0.2199426 |
| 0.5482064 |
| 0.5088132 |

------------------------------------------------------------------------

<br>

# Question 3 - Interpret Posteriors

## 3. a) Give the 95% HDI for ![\\theta](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Ctheta "\theta") the probability of a success.

-   There is a probability of 95% that the value will fall between 0.184
    and 0.433

## 3. b) What is the posterior probability that ![\\theta](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Ctheta "\theta") \< 0.5

-   The posterior probability that
    ![\\theta](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Ctheta "\theta")
    \< 0.5 is
    ![99.7](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;99.7 "99.7")%

## 3. c) If H0 : ![\\theta](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Ctheta "\theta") = 0.5 should we reject it (the NULL), yes/no?

-   Yes, we should reject it because 0.5 falls outside the 95% and 90%
    HDI

## 3. d) The posterior probability that ![\\theta](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Ctheta "\theta") lies in (0.203, 0.413) is 0.90. True or False?

-   It is `TRUE` that there is a probability of 90% that the value will
    fall between 0.203 and 0.413

## 3. e) What does ROPE stand for?

-   ROPE stands for “Region Of Practical Equivalence”.
-   In bayesian analysis, if the ROPE is entirely within the HDI for
    ![\\theta](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Ctheta "\theta")
    then we should reject the NULL for practical purposes.

------------------------------------------------------------------------

<br>

# Question 4 - GLM

## 4. a) Link function?

-   There is a logit link function used. Model:

![
logit(\\theta) = log (\\frac{\\theta}{(1 − \\theta)})
](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0Alogit%28%5Ctheta%29%20%3D%20log%20%28%5Cfrac%7B%5Ctheta%7D%7B%281%20%E2%88%92%20%5Ctheta%29%7D%29%0A "
logit(\theta) = log (\frac{\theta}{(1 − \theta)})
")

## 4. b) What numerical values will replace the y variable?

-   The value of y is transformed onto a new scale, and that transformed
    value is modeled as a linear combination of predictors.
-   Maps theta *from* `0-1` *to* the real value between
    ![-\\infty](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;-%5Cinfty "-\infty")
    and
    ![\\infty](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cinfty "\infty")

## 4. c) High impact priors?

-   No, they are `low` impact priors
-   It means that the prior distribution will not impact the posterior
    significantly

## 4. d) Interpret Point/Interval Estimates

### 4. d, 1) ![\\beta_3](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cbeta_3 "\beta_3") Point Estimate

-   The point estimate for
    ![\\beta_3](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cbeta_3 "\beta_3")
    is -0.75905 with a standard deviation of 0.199376.
-   I.e. a male is more likely to die than a female.

### 4. d, 2) ![\\beta_1](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cbeta_1 "\beta_1") Interval Estimate

-   There is a 95% probability that the values of
    ![\\beta_1](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cbeta_1 "\beta_1")
    will fall between 0.025778 and 0.6118

------------------------------------------------------------------------

<br>

# Question 5 - Exponential Family

The exponential family can be defined as any density of the following
form:

![
f(y \\mid \\theta, \\phi)=\\exp \\left(\\frac{y \\theta-b(\\theta)}{a(\\phi)}+c(y, \\phi)\\right)
](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0Af%28y%20%5Cmid%20%5Ctheta%2C%20%5Cphi%29%3D%5Cexp%20%5Cleft%28%5Cfrac%7By%20%5Ctheta-b%28%5Ctheta%29%7D%7Ba%28%5Cphi%29%7D%2Bc%28y%2C%20%5Cphi%29%5Cright%29%0A "
f(y \mid \theta, \phi)=\exp \left(\frac{y \theta-b(\theta)}{a(\phi)}+c(y, \phi)\right)
")

<br>

## 5. a) Show ![E(Y) = b^{\\prime}(\\theta)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;E%28Y%29%20%3D%20b%5E%7B%5Cprime%7D%28%5Ctheta%29 "E(Y) = b^{\prime}(\theta)")

### Sum of distribution is 1

![
\\int_y f(y)dy = 1 \\\\
](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0A%5Cint_y%20f%28y%29dy%20%3D%201%20%5C%5C%0A "
\int_y f(y)dy = 1 \\
")

### Differentiate above equation

![
\\frac{d}{d\\theta} \\int_y f(y)dy = \\frac{d}{d\\theta} 1 = 0
](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0A%5Cfrac%7Bd%7D%7Bd%5Ctheta%7D%20%5Cint_y%20f%28y%29dy%20%3D%20%5Cfrac%7Bd%7D%7Bd%5Ctheta%7D%201%20%3D%200%0A "
\frac{d}{d\theta} \int_y f(y)dy = \frac{d}{d\theta} 1 = 0
")

### Take inside the integral

![
\\int_y \\frac{df}{d\\theta} dy = 0
](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0A%5Cint_y%20%5Cfrac%7Bdf%7D%7Bd%5Ctheta%7D%20dy%20%3D%200%0A "
\int_y \frac{df}{d\theta} dy = 0
")

### Differentiate with ![f](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;f "f") being the exponential

![
\\frac{df}{d\\theta} = 
\\frac{y -b^{\\prime}(\\theta)}{a(\\phi)} \\ 
\\exp \\left(\\frac{y \\theta-b(\\theta)}{a(\\phi)}
+ c(y, \\phi)\\right)
](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0A%5Cfrac%7Bdf%7D%7Bd%5Ctheta%7D%20%3D%20%0A%5Cfrac%7By%20-b%5E%7B%5Cprime%7D%28%5Ctheta%29%7D%7Ba%28%5Cphi%29%7D%20%5C%20%0A%5Cexp%20%5Cleft%28%5Cfrac%7By%20%5Ctheta-b%28%5Ctheta%29%7D%7Ba%28%5Cphi%29%7D%0A%2B%20c%28y%2C%20%5Cphi%29%5Cright%29%0A "
\frac{df}{d\theta} = 
\frac{y -b^{\prime}(\theta)}{a(\phi)} \ 
\exp \left(\frac{y \theta-b(\theta)}{a(\phi)}
+ c(y, \phi)\right)
")

### Replace with ![f](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;f "f") since it is the expontial

![
\\frac{df}{d\\theta} = 
\\frac{y -b^{\\prime}(\\theta)}{a(\\phi)} \\ f
](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0A%5Cfrac%7Bdf%7D%7Bd%5Ctheta%7D%20%3D%20%0A%5Cfrac%7By%20-b%5E%7B%5Cprime%7D%28%5Ctheta%29%7D%7Ba%28%5Cphi%29%7D%20%5C%20f%0A "
\frac{df}{d\theta} = 
\frac{y -b^{\prime}(\theta)}{a(\phi)} \ f
")

### Plug ![\\int_y \\frac{df}{d\\theta} dy = 0](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cint_y%20%5Cfrac%7Bdf%7D%7Bd%5Ctheta%7D%20dy%20%3D%200 "\int_y \frac{df}{d\theta} dy = 0") into above ![\\frac{df}{d\\theta}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cfrac%7Bdf%7D%7Bd%5Ctheta%7D "\frac{df}{d\theta}")

![
\\int_y \\begin{bmatrix} \\frac{y -b^{\\prime}(\\theta)}{a(\\phi)} \\end{bmatrix} \\ f \\ dy = 0
](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0A%5Cint_y%20%5Cbegin%7Bbmatrix%7D%20%5Cfrac%7By%20-b%5E%7B%5Cprime%7D%28%5Ctheta%29%7D%7Ba%28%5Cphi%29%7D%20%5Cend%7Bbmatrix%7D%20%5C%20f%20%5C%20dy%20%3D%200%0A "
\int_y \begin{bmatrix} \frac{y -b^{\prime}(\theta)}{a(\phi)} \end{bmatrix} \ f \ dy = 0
")

### Distribute ![f](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;f "f")

![
\\int_y \\begin{bmatrix} \\frac{yf -b^{\\prime}(\\theta)f}{a(\\phi)} \\end{bmatrix} \\ f \\ dy = 0
](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0A%5Cint_y%20%5Cbegin%7Bbmatrix%7D%20%5Cfrac%7Byf%20-b%5E%7B%5Cprime%7D%28%5Ctheta%29f%7D%7Ba%28%5Cphi%29%7D%20%5Cend%7Bbmatrix%7D%20%5C%20f%20%5C%20dy%20%3D%200%0A "
\int_y \begin{bmatrix} \frac{yf -b^{\prime}(\theta)f}{a(\phi)} \end{bmatrix} \ f \ dy = 0
")

### Multiply by ![a(\\phi)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;a%28%5Cphi%29 "a(\phi)") to get rid of it

![
\\int_y yf \\ dy 
- \\int_y b^{\\prime}(\\theta)f \\ dy= 0
](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0A%5Cint_y%20yf%20%5C%20dy%20%0A-%20%5Cint_y%20b%5E%7B%5Cprime%7D%28%5Ctheta%29f%20%5C%20dy%3D%200%0A "
\int_y yf \ dy 
- \int_y b^{\prime}(\theta)f \ dy= 0
")

### Writing above in terms of E(Y)

![
E(Y) - b^{\\prime}(\\theta) \\ \\int_yf \\ dy= 0
](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0AE%28Y%29%20-%20b%5E%7B%5Cprime%7D%28%5Ctheta%29%20%5C%20%5Cint_yf%20%5C%20dy%3D%200%0A "
E(Y) - b^{\prime}(\theta) \ \int_yf \ dy= 0
")

### *Solution*: Therefore, the expected value of Y ![E(Y)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;E%28Y%29 "E(Y)") is ![b^{\\prime}(\\theta)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;b%5E%7B%5Cprime%7D%28%5Ctheta%29 "b^{\prime}(\theta)")

![
E(Y) = b^{\\prime}(\\theta)
](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0AE%28Y%29%20%3D%20b%5E%7B%5Cprime%7D%28%5Ctheta%29%0A "
E(Y) = b^{\prime}(\theta)
")

------------------------------------------------------------------------

<br>

## 5. b) Show ![V(Y) = b^{\\prime \\prime}(\\theta) a(\\phi)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;V%28Y%29%20%3D%20b%5E%7B%5Cprime%20%5Cprime%7D%28%5Ctheta%29%20a%28%5Cphi%29 "V(Y) = b^{\prime \prime}(\theta) a(\phi)")

### From the prior proof above

![
\\frac{df}{d\\theta} = 
\\begin{bmatrix} \\frac{y -b^{\\prime}(\\theta)}{a(\\phi)} \\end{bmatrix} \\ f
](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0A%5Cfrac%7Bdf%7D%7Bd%5Ctheta%7D%20%3D%20%0A%5Cbegin%7Bbmatrix%7D%20%5Cfrac%7By%20-b%5E%7B%5Cprime%7D%28%5Ctheta%29%7D%7Ba%28%5Cphi%29%7D%20%5Cend%7Bbmatrix%7D%20%5C%20f%0A "
\frac{df}{d\theta} = 
\begin{bmatrix} \frac{y -b^{\prime}(\theta)}{a(\phi)} \end{bmatrix} \ f
")

### Get the second derivative of above

![
\\frac{d^2f}{d\\theta^2} = 
\\frac{-b^{\\prime \\prime}(\\theta)}{a(\\phi)} \\ f
+ \\frac{y -b^{\\prime}(\\theta)}{a(\\phi)} \\ f^{\\prime}
](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0A%5Cfrac%7Bd%5E2f%7D%7Bd%5Ctheta%5E2%7D%20%3D%20%0A%5Cfrac%7B-b%5E%7B%5Cprime%20%5Cprime%7D%28%5Ctheta%29%7D%7Ba%28%5Cphi%29%7D%20%5C%20f%0A%2B%20%5Cfrac%7By%20-b%5E%7B%5Cprime%7D%28%5Ctheta%29%7D%7Ba%28%5Cphi%29%7D%20%5C%20f%5E%7B%5Cprime%7D%0A "
\frac{d^2f}{d\theta^2} = 
\frac{-b^{\prime \prime}(\theta)}{a(\phi)} \ f
+ \frac{y -b^{\prime}(\theta)}{a(\phi)} \ f^{\prime}
")

### Replace above ![f^{\\prime}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;f%5E%7B%5Cprime%7D "f^{\prime}") with ![f](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;f "f") from prior proof (two above points)

![
\\frac{d^2f}{d\\theta^2} = 
\\frac{-b^{\\prime \\prime}(\\theta)}{a(\\phi)} \\ f
+ \\begin{bmatrix} \\frac{y -b^{\\prime}(\\theta)}{a(\\phi)} \\end{bmatrix}^2 \\ f
](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0A%5Cfrac%7Bd%5E2f%7D%7Bd%5Ctheta%5E2%7D%20%3D%20%0A%5Cfrac%7B-b%5E%7B%5Cprime%20%5Cprime%7D%28%5Ctheta%29%7D%7Ba%28%5Cphi%29%7D%20%5C%20f%0A%2B%20%5Cbegin%7Bbmatrix%7D%20%5Cfrac%7By%20-b%5E%7B%5Cprime%7D%28%5Ctheta%29%7D%7Ba%28%5Cphi%29%7D%20%5Cend%7Bbmatrix%7D%5E2%20%5C%20f%0A "
\frac{d^2f}{d\theta^2} = 
\frac{-b^{\prime \prime}(\theta)}{a(\phi)} \ f
+ \begin{bmatrix} \frac{y -b^{\prime}(\theta)}{a(\phi)} \end{bmatrix}^2 \ f
")

### Note sum is 1 for all densities

![
\\int_y dy = 1 \\\\
](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0A%5Cint_y%20dy%20%3D%201%20%5C%5C%0A "
\int_y dy = 1 \\
")

### Take 2nd dertivative using above notes to get 0

![
\\int_y \\frac{d^2f}{d\\theta^2} \\ dy = 0
](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0A%5Cint_y%20%5Cfrac%7Bd%5E2f%7D%7Bd%5Ctheta%5E2%7D%20%5C%20dy%20%3D%200%0A "
\int_y \frac{d^2f}{d\theta^2} \ dy = 0
")

### Plug ![\\frac{d^2f}{d\\theta^2}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cfrac%7Bd%5E2f%7D%7Bd%5Ctheta%5E2%7D "\frac{d^2f}{d\theta^2}") into above equation

![
\\int_y
\\begin{pmatrix}
\\frac{-b^{\\prime \\prime}(\\theta)}{a(\\phi)} \\ f
+ \\begin{bmatrix} \\frac{y -b^{\\prime}(\\theta)}{a(\\phi)} \\end{bmatrix}^2 \\ f
\\end{pmatrix} \\
dy = 0
](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0A%5Cint_y%0A%5Cbegin%7Bpmatrix%7D%0A%5Cfrac%7B-b%5E%7B%5Cprime%20%5Cprime%7D%28%5Ctheta%29%7D%7Ba%28%5Cphi%29%7D%20%5C%20f%0A%2B%20%5Cbegin%7Bbmatrix%7D%20%5Cfrac%7By%20-b%5E%7B%5Cprime%7D%28%5Ctheta%29%7D%7Ba%28%5Cphi%29%7D%20%5Cend%7Bbmatrix%7D%5E2%20%5C%20f%0A%5Cend%7Bpmatrix%7D%20%5C%0Ady%20%3D%200%0A "
\int_y
\begin{pmatrix}
\frac{-b^{\prime \prime}(\theta)}{a(\phi)} \ f
+ \begin{bmatrix} \frac{y -b^{\prime}(\theta)}{a(\phi)} \end{bmatrix}^2 \ f
\end{pmatrix} \
dy = 0
")

### Simplify prior equation and isolate ![\\frac{-b^{\\prime \\prime}(\\theta)}{a(\\phi)}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cfrac%7B-b%5E%7B%5Cprime%20%5Cprime%7D%28%5Ctheta%29%7D%7Ba%28%5Cphi%29%7D "\frac{-b^{\prime \prime}(\theta)}{a(\phi)}")

![
\\frac{-b^{\\prime \\prime}(\\theta)}{a(\\phi)} \\ 
\\int_y f \\ dy
+ \\int_y  \\frac{(y -b^{\\prime}(\\theta))^2}{a(\\phi)^2} 
\\ f \\ dy = 0
](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0A%5Cfrac%7B-b%5E%7B%5Cprime%20%5Cprime%7D%28%5Ctheta%29%7D%7Ba%28%5Cphi%29%7D%20%5C%20%0A%5Cint_y%20f%20%5C%20dy%0A%2B%20%5Cint_y%20%20%5Cfrac%7B%28y%20-b%5E%7B%5Cprime%7D%28%5Ctheta%29%29%5E2%7D%7Ba%28%5Cphi%29%5E2%7D%20%0A%5C%20f%20%5C%20dy%20%3D%200%0A "
\frac{-b^{\prime \prime}(\theta)}{a(\phi)} \ 
\int_y f \ dy
+ \int_y  \frac{(y -b^{\prime}(\theta))^2}{a(\phi)^2} 
\ f \ dy = 0
")

### Plug expected value ![E(Y)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;E%28Y%29 "E(Y)") from prior proof knowledge that ![E(Y)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;E%28Y%29 "E(Y)") = ![b^{\\prime}(\\theta)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;b%5E%7B%5Cprime%7D%28%5Ctheta%29 "b^{\prime}(\theta)")

![
\\frac{-b^{\\prime \\prime}(\\theta)}{a(\\phi)} 
+     \\frac{1}{a(\\phi)^2} \\ 
\\int_y (y - E(Y))^2
\\ f \\ dy = 0
](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0A%5Cfrac%7B-b%5E%7B%5Cprime%20%5Cprime%7D%28%5Ctheta%29%7D%7Ba%28%5Cphi%29%7D%20%0A%2B%20%20%20%20%20%5Cfrac%7B1%7D%7Ba%28%5Cphi%29%5E2%7D%20%5C%20%0A%5Cint_y%20%28y%20-%20E%28Y%29%29%5E2%0A%5C%20f%20%5C%20dy%20%3D%200%0A "
\frac{-b^{\prime \prime}(\theta)}{a(\phi)} 
+     \frac{1}{a(\phi)^2} \ 
\int_y (y - E(Y))^2
\ f \ dy = 0
")

### Subsititute Variance ![\\sigma^2](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Csigma%5E2 "\sigma^2") for ![(y - E(Y))^2](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%28y%20-%20E%28Y%29%29%5E2 "(y - E(Y))^2")

![
\\frac{-b^{\\prime \\prime}(\\theta)}{a(\\phi)} 
+     \\frac{1}{a(\\phi)^2} \\ 
\\sigma^2 = 0
](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0A%5Cfrac%7B-b%5E%7B%5Cprime%20%5Cprime%7D%28%5Ctheta%29%7D%7Ba%28%5Cphi%29%7D%20%0A%2B%20%20%20%20%20%5Cfrac%7B1%7D%7Ba%28%5Cphi%29%5E2%7D%20%5C%20%0A%5Csigma%5E2%20%3D%200%0A "
\frac{-b^{\prime \prime}(\theta)}{a(\phi)} 
+     \frac{1}{a(\phi)^2} \ 
\sigma^2 = 0
")

### Simplify by Multiplying by ![a(\\phi)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;a%28%5Cphi%29 "a(\phi)")

![
-b^{\\prime \\prime}(\\theta) \\ a(\\phi) + \\sigma^2 = 0
](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0A-b%5E%7B%5Cprime%20%5Cprime%7D%28%5Ctheta%29%20%5C%20a%28%5Cphi%29%20%2B%20%5Csigma%5E2%20%3D%200%0A "
-b^{\prime \prime}(\theta) \ a(\phi) + \sigma^2 = 0
")

### *Solution*: Therefore, we get see that the variance ![\\sigma^2](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Csigma%5E2 "\sigma^2") is ![b^{\\prime \\prime}a(\\phi)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;b%5E%7B%5Cprime%20%5Cprime%7Da%28%5Cphi%29 "b^{\prime \prime}a(\phi)")

![
V(Y) = \\sigma^2 = b^{\\prime \\prime}a(\\phi)
](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0AV%28Y%29%20%3D%20%5Csigma%5E2%20%3D%20b%5E%7B%5Cprime%20%5Cprime%7Da%28%5Cphi%29%0A "
V(Y) = \sigma^2 = b^{\prime \prime}a(\phi)
")

------------------------------------------------------------------------

<br>

## 5. c) Show that the `Binomial` distribution belongs to the `exponential` family

### Given the binomial definition, rearrange:

![
\\begin{aligned}
p(Y = y) &=\\left(\\begin{array}{c}
n \\\\ y
\\end{array}\\right) p^{y}(1-p)^{n-y} \\\\
&=\\left(\\begin{array}{c}
n \\\\ y
\\end{array}\\right)(1-p)^{n}\\left(\\frac{p}{1-p}\\right)^{y} \\\\
\\end{aligned}
](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0A%5Cbegin%7Baligned%7D%0Ap%28Y%20%3D%20y%29%20%26%3D%5Cleft%28%5Cbegin%7Barray%7D%7Bc%7D%0An%20%5C%5C%20y%0A%5Cend%7Barray%7D%5Cright%29%20p%5E%7By%7D%281-p%29%5E%7Bn-y%7D%20%5C%5C%0A%26%3D%5Cleft%28%5Cbegin%7Barray%7D%7Bc%7D%0An%20%5C%5C%20y%0A%5Cend%7Barray%7D%5Cright%29%281-p%29%5E%7Bn%7D%5Cleft%28%5Cfrac%7Bp%7D%7B1-p%7D%5Cright%29%5E%7By%7D%20%5C%5C%0A%5Cend%7Baligned%7D%0A "
\begin{aligned}
p(Y = y) &=\left(\begin{array}{c}
n \\ y
\end{array}\right) p^{y}(1-p)^{n-y} \\
&=\left(\begin{array}{c}
n \\ y
\end{array}\right)(1-p)^{n}\left(\frac{p}{1-p}\right)^{y} \\
\end{aligned}
")

### Therefore we see it is apart of the exponential family

![
p(Y = y) = 
\\left(\\begin{array}{l} n \\\\ y
\\end{array}\\right)(1-p)^{n} \\exp \\left(\\log \\left(\\frac{p}{1-p}\\right) y\\right)
](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0Ap%28Y%20%3D%20y%29%20%3D%20%0A%5Cleft%28%5Cbegin%7Barray%7D%7Bl%7D%20n%20%5C%5C%20y%0A%5Cend%7Barray%7D%5Cright%29%281-p%29%5E%7Bn%7D%20%5Cexp%20%5Cleft%28%5Clog%20%5Cleft%28%5Cfrac%7Bp%7D%7B1-p%7D%5Cright%29%20y%5Cright%29%0A "
p(Y = y) = 
\left(\begin{array}{l} n \\ y
\end{array}\right)(1-p)^{n} \exp \left(\log \left(\frac{p}{1-p}\right) y\right)
")

------------------------------------------------------------------------

<br>

## 5. d) Show ![E(Y) = np](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;E%28Y%29%20%3D%20np "E(Y) = np")

### Using the definition of a Moment Generating Function

![
b(y ; n, p)=\\frac{n !}{y !(n-y) !} p^{y} q^{n-y} \\quad \\text { with } \\quad q=1-p .
](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0Ab%28y%20%3B%20n%2C%20p%29%3D%5Cfrac%7Bn%20%21%7D%7By%20%21%28n-y%29%20%21%7D%20p%5E%7By%7D%20q%5E%7Bn-y%7D%20%5Cquad%20%5Ctext%20%7B%20with%20%7D%20%5Cquad%20q%3D1-p%20.%0A "
b(y ; n, p)=\frac{n !}{y !(n-y) !} p^{y} q^{n-y} \quad \text { with } \quad q=1-p .
")

### MGF Given by:

![
\\begin{aligned}
M(y, t) &=\\sum\_{y=0}^{n} e^{y t} \\frac{n !}{y !(n-y) !} p^{y} q^{n-y} \\\\
&=\\sum\_{y=0}^{n} \\frac{n !}{y !(n-y) !}\\left(p e^{t}\\right)^{y} q^{n-y} \\\\
&=\\left(p e^{t}+q\\right)^{n}
\\end{aligned}
](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0A%5Cbegin%7Baligned%7D%0AM%28y%2C%20t%29%20%26%3D%5Csum_%7By%3D0%7D%5E%7Bn%7D%20e%5E%7By%20t%7D%20%5Cfrac%7Bn%20%21%7D%7By%20%21%28n-y%29%20%21%7D%20p%5E%7By%7D%20q%5E%7Bn-y%7D%20%5C%5C%0A%26%3D%5Csum_%7By%3D0%7D%5E%7Bn%7D%20%5Cfrac%7Bn%20%21%7D%7By%20%21%28n-y%29%20%21%7D%5Cleft%28p%20e%5E%7Bt%7D%5Cright%29%5E%7By%7D%20q%5E%7Bn-y%7D%20%5C%5C%0A%26%3D%5Cleft%28p%20e%5E%7Bt%7D%2Bq%5Cright%29%5E%7Bn%7D%0A%5Cend%7Baligned%7D%0A "
\begin{aligned}
M(y, t) &=\sum_{y=0}^{n} e^{y t} \frac{n !}{y !(n-y) !} p^{y} q^{n-y} \\
&=\sum_{y=0}^{n} \frac{n !}{y !(n-y) !}\left(p e^{t}\right)^{y} q^{n-y} \\
&=\left(p e^{t}+q\right)^{n}
\end{aligned}
")

### Differentiate the MGF with respect to ![t](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;t "t") using the function-of-a-function rule:

![
\\begin{aligned}
\\frac{d M(y, t)}{d t} &=n\\left(q+p e^{t}\\right)^{n-1} p e^{t} \\\\
&=n p e^{t}\\left(p e^{t}+q\\right)^{n-1}
\\end{aligned}
](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0A%5Cbegin%7Baligned%7D%0A%5Cfrac%7Bd%20M%28y%2C%20t%29%7D%7Bd%20t%7D%20%26%3Dn%5Cleft%28q%2Bp%20e%5E%7Bt%7D%5Cright%29%5E%7Bn-1%7D%20p%20e%5E%7Bt%7D%20%5C%5C%0A%26%3Dn%20p%20e%5E%7Bt%7D%5Cleft%28p%20e%5E%7Bt%7D%2Bq%5Cright%29%5E%7Bn-1%7D%0A%5Cend%7Baligned%7D%0A "
\begin{aligned}
\frac{d M(y, t)}{d t} &=n\left(q+p e^{t}\right)^{n-1} p e^{t} \\
&=n p e^{t}\left(p e^{t}+q\right)^{n-1}
\end{aligned}
")

### *Solution*: Now use ![t=0](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;t%3D0 "t=0") to get ![E(Y)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;E%28Y%29 "E(Y)"), which is = ![n p](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;n%20p "n p")

![
\\begin{aligned}
E(Y) &=n p(p+q)^{n-1} \\\\
&=n p 
\\end{aligned}
](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0A%5Cbegin%7Baligned%7D%0AE%28Y%29%20%26%3Dn%20p%28p%2Bq%29%5E%7Bn-1%7D%20%5C%5C%0A%26%3Dn%20p%20%0A%5Cend%7Baligned%7D%0A "
\begin{aligned}
E(Y) &=n p(p+q)^{n-1} \\
&=n p 
\end{aligned}
")

------------------------------------------------------------------------

<br>

## 5. e) Show ![V(Y) = np(1−p)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;V%28Y%29%20%3D%20np%281%E2%88%92p%29 "V(Y) = np(1−p)")

### Find the second moment using product rule:

![
\\frac{d u v}{d y}=u \\frac{d v}{d y}+v \\frac{d u}{d y}
](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0A%5Cfrac%7Bd%20u%20v%7D%7Bd%20y%7D%3Du%20%5Cfrac%7Bd%20v%7D%7Bd%20y%7D%2Bv%20%5Cfrac%7Bd%20u%7D%7Bd%20y%7D%0A "
\frac{d u v}{d y}=u \frac{d v}{d y}+v \frac{d u}{d y}
")

![
\\begin{aligned}
\\frac{d^{2} M(y, t)}{d t^{2}} &=n p e^{t}\\left\\{(n-1)\\left(p e^{t}+q\\right)^{n-2} p e^{t}\\right\\}+\\left(p e^{t}+q\\right)^{n-1}\\left\\{n p e^{t}\\right\\} \\\\
&=n p e^{t}\\left(p e^{t}+q\\right)^{n-2}\\left\\{(n-1) p e^{t}+\\left(p e^{t}+q\\right)\\right\\} \\\\
&=n p e^{t}\\left(p e^{t}+q\\right)^{n-2}\\left\\{q+n p e^{t}\\right\\} .
\\end{aligned}
](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0A%5Cbegin%7Baligned%7D%0A%5Cfrac%7Bd%5E%7B2%7D%20M%28y%2C%20t%29%7D%7Bd%20t%5E%7B2%7D%7D%20%26%3Dn%20p%20e%5E%7Bt%7D%5Cleft%5C%7B%28n-1%29%5Cleft%28p%20e%5E%7Bt%7D%2Bq%5Cright%29%5E%7Bn-2%7D%20p%20e%5E%7Bt%7D%5Cright%5C%7D%2B%5Cleft%28p%20e%5E%7Bt%7D%2Bq%5Cright%29%5E%7Bn-1%7D%5Cleft%5C%7Bn%20p%20e%5E%7Bt%7D%5Cright%5C%7D%20%5C%5C%0A%26%3Dn%20p%20e%5E%7Bt%7D%5Cleft%28p%20e%5E%7Bt%7D%2Bq%5Cright%29%5E%7Bn-2%7D%5Cleft%5C%7B%28n-1%29%20p%20e%5E%7Bt%7D%2B%5Cleft%28p%20e%5E%7Bt%7D%2Bq%5Cright%29%5Cright%5C%7D%20%5C%5C%0A%26%3Dn%20p%20e%5E%7Bt%7D%5Cleft%28p%20e%5E%7Bt%7D%2Bq%5Cright%29%5E%7Bn-2%7D%5Cleft%5C%7Bq%2Bn%20p%20e%5E%7Bt%7D%5Cright%5C%7D%20.%0A%5Cend%7Baligned%7D%0A "
\begin{aligned}
\frac{d^{2} M(y, t)}{d t^{2}} &=n p e^{t}\left\{(n-1)\left(p e^{t}+q\right)^{n-2} p e^{t}\right\}+\left(p e^{t}+q\right)^{n-1}\left\{n p e^{t}\right\} \\
&=n p e^{t}\left(p e^{t}+q\right)^{n-2}\left\{(n-1) p e^{t}+\left(p e^{t}+q\right)\right\} \\
&=n p e^{t}\left(p e^{t}+q\right)^{n-2}\left\{q+n p e^{t}\right\} .
\end{aligned}
")

### Use ![t=0](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;t%3D0 "t=0") again:

![
\\begin{aligned}
E\\left(y^{2}\\right) &=n p(p+q)^{n-2}(n p+q) \\\\
&=n p(n p+q)
\\end{aligned}
](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0A%5Cbegin%7Baligned%7D%0AE%5Cleft%28y%5E%7B2%7D%5Cright%29%20%26%3Dn%20p%28p%2Bq%29%5E%7Bn-2%7D%28n%20p%2Bq%29%20%5C%5C%0A%26%3Dn%20p%28n%20p%2Bq%29%0A%5Cend%7Baligned%7D%0A "
\begin{aligned}
E\left(y^{2}\right) &=n p(p+q)^{n-2}(n p+q) \\
&=n p(n p+q)
\end{aligned}
")

### Derive ![V(Y)=E(Y-\\mu)^{2}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;V%28Y%29%3DE%28Y-%5Cmu%29%5E%7B2%7D "V(Y)=E(Y-\mu)^{2}"), in turn this means that ![V(Y) = np(1 - p)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;V%28Y%29%20%3D%20np%281%20-%20p%29 "V(Y) = np(1 - p)")

> You could also write it as
> ![V(Y) = n p q](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;V%28Y%29%20%3D%20n%20p%20q "V(Y) = n p q")
> where
> ![q=1-p](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;q%3D1-p "q=1-p")

![
\\begin{aligned}
E(Y) &=E\\left(y^{2}\\right)-\\{E(Y)\\}^{2} \\\\
&=n p(n p+q)-n^{2} p^{2} \\\\
&=n p q \\\\
\\end{aligned}
](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0A%5Cbegin%7Baligned%7D%0AE%28Y%29%20%26%3DE%5Cleft%28y%5E%7B2%7D%5Cright%29-%5C%7BE%28Y%29%5C%7D%5E%7B2%7D%20%5C%5C%0A%26%3Dn%20p%28n%20p%2Bq%29-n%5E%7B2%7D%20p%5E%7B2%7D%20%5C%5C%0A%26%3Dn%20p%20q%20%5C%5C%0A%5Cend%7Baligned%7D%0A "
\begin{aligned}
E(Y) &=E\left(y^{2}\right)-\{E(Y)\}^{2} \\
&=n p(n p+q)-n^{2} p^{2} \\
&=n p q \\
\end{aligned}
")

### *Solution*: Knowing that ![q = 1 - p](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;q%20%3D%201%20-%20p "q = 1 - p"), we know ![V(Y) = n p (1 - p)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;V%28Y%29%20%3D%20n%20p%20%281%20-%20p%29 "V(Y) = n p (1 - p)") from above ![n p q](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;n%20p%20q "n p q")

## 

![ V(Y) = n p (1 - p)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%20V%28Y%29%20%3D%20n%20p%20%281%20-%20p%29 " V(Y) = n p (1 - p)")

<br>

# Question 6 - Derive Evidence

*Some of the derivation below may look similar to question 1 (a):*
<br>  
*Adapted from JK’s book - page 132 Doing Bayesian Data Analysis:* <br>

If
![\\theta \\sim \\operatorname{Beta}(\\alpha, \\beta)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Ctheta%20%5Csim%20%5Coperatorname%7BBeta%7D%28%5Calpha%2C%20%5Cbeta%29 "\theta \sim \operatorname{Beta}(\alpha, \beta)")
and
![X \\sim \\operatorname{Bin}(n, \\theta)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;X%20%5Csim%20%5Coperatorname%7BBin%7D%28n%2C%20%5Ctheta%29 "X \sim \operatorname{Bin}(n, \theta)"),
prove that
![\\theta \\mid X \\sim \\operatorname{Beta}(x+\\alpha, n-x+\\beta)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Ctheta%20%5Cmid%20X%20%5Csim%20%5Coperatorname%7BBeta%7D%28x%2B%5Calpha%2C%20n-x%2B%5Cbeta%29 "\theta \mid X \sim \operatorname{Beta}(x+\alpha, n-x+\beta)")
through proof below:

![p(\\theta \\mid x) \\propto p(\\theta) p(x \\mid \\theta) = \\frac{1}{B(\\alpha, \\beta)} \\theta^{\\alpha-1}(1-\\theta)^{\\beta-1}\\left(\\begin{array}{l}n \\\\ x\\end{array}\\right) \\theta^{x}(1-\\theta)^{n-x}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;p%28%5Ctheta%20%5Cmid%20x%29%20%5Cpropto%20p%28%5Ctheta%29%20p%28x%20%5Cmid%20%5Ctheta%29%20%3D%20%5Cfrac%7B1%7D%7BB%28%5Calpha%2C%20%5Cbeta%29%7D%20%5Ctheta%5E%7B%5Calpha-1%7D%281-%5Ctheta%29%5E%7B%5Cbeta-1%7D%5Cleft%28%5Cbegin%7Barray%7D%7Bl%7Dn%20%5C%5C%20x%5Cend%7Barray%7D%5Cright%29%20%5Ctheta%5E%7Bx%7D%281-%5Ctheta%29%5E%7Bn-x%7D "p(\theta \mid x) \propto p(\theta) p(x \mid \theta) = \frac{1}{B(\alpha, \beta)} \theta^{\alpha-1}(1-\theta)^{\beta-1}\left(\begin{array}{l}n \\ x\end{array}\right) \theta^{x}(1-\theta)^{n-x}")

Note Bayes’ rule  

![
\\underbrace{p(\\theta \\mid x)}\_{Posterior} \\propto 
\\underbrace{p(\\theta)}\_{Prior} \\underbrace{p(x \\mid \\theta)}\_{Lik.} 
\\ / \\underbrace{p(x)}\_{Evidence} 
= \\frac{p(x, n \\mid \\theta) p(\\theta)}{p(x, n)} / p(x) = p(\\theta \\mid x, n) / p(x)
](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0A%5Cunderbrace%7Bp%28%5Ctheta%20%5Cmid%20x%29%7D_%7BPosterior%7D%20%5Cpropto%20%0A%5Cunderbrace%7Bp%28%5Ctheta%29%7D_%7BPrior%7D%20%5Cunderbrace%7Bp%28x%20%5Cmid%20%5Ctheta%29%7D_%7BLik.%7D%20%0A%5C%20%2F%20%5Cunderbrace%7Bp%28x%29%7D_%7BEvidence%7D%20%0A%3D%20%5Cfrac%7Bp%28x%2C%20n%20%5Cmid%20%5Ctheta%29%20p%28%5Ctheta%29%7D%7Bp%28x%2C%20n%29%7D%20%2F%20p%28x%29%20%3D%20p%28%5Ctheta%20%5Cmid%20x%2C%20n%29%20%2F%20p%28x%29%0A "
\underbrace{p(\theta \mid x)}_{Posterior} \propto 
\underbrace{p(\theta)}_{Prior} \underbrace{p(x \mid \theta)}_{Lik.} 
\ / \underbrace{p(x)}_{Evidence} 
= \frac{p(x, n \mid \theta) p(\theta)}{p(x, n)} / p(x) = p(\theta \mid x, n) / p(x)
")

<br>

Define Bernoulli and beta distributions  
![=\\underbrace{\\theta^{x}(1-\\theta)^{(n-x)}}\_{Bernoulli \\ Lik.} \\underbrace{\\frac{\\theta^{(\\alpha-1)}(1-\\theta)^{(\\beta-1)}}{B(\\alpha, \\beta)}}\_{Beta \\ Prior} / p(x, n) / p(x)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%3D%5Cunderbrace%7B%5Ctheta%5E%7Bx%7D%281-%5Ctheta%29%5E%7B%28n-x%29%7D%7D_%7BBernoulli%20%5C%20Lik.%7D%20%5Cunderbrace%7B%5Cfrac%7B%5Ctheta%5E%7B%28%5Calpha-1%29%7D%281-%5Ctheta%29%5E%7B%28%5Cbeta-1%29%7D%7D%7BB%28%5Calpha%2C%20%5Cbeta%29%7D%7D_%7BBeta%20%5C%20Prior%7D%20%2F%20p%28x%2C%20n%29%20%2F%20p%28x%29 "=\underbrace{\theta^{x}(1-\theta)^{(n-x)}}_{Bernoulli \ Lik.} \underbrace{\frac{\theta^{(\alpha-1)}(1-\theta)^{(\beta-1)}}{B(\alpha, \beta)}}_{Beta \ Prior} / p(x, n) / p(x)")
<br>

Rearrange factors  
![=\\frac{1}{B(\\alpha, \\beta) p(x, n)} \\theta^{(\\alpha-1)} (1-\\theta)^{(\\beta-1)} \\theta^{x}(1-\\theta)^{(n-x)} / p(x)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%3D%5Cfrac%7B1%7D%7BB%28%5Calpha%2C%20%5Cbeta%29%20p%28x%2C%20n%29%7D%20%5Ctheta%5E%7B%28%5Calpha-1%29%7D%20%281-%5Ctheta%29%5E%7B%28%5Cbeta-1%29%7D%20%5Ctheta%5E%7Bx%7D%281-%5Ctheta%29%5E%7B%28n-x%29%7D%20%2F%20p%28x%29 "=\frac{1}{B(\alpha, \beta) p(x, n)} \theta^{(\alpha-1)} (1-\theta)^{(\beta-1)} \theta^{x}(1-\theta)^{(n-x)} / p(x)")
<br>

![=\\frac{1}{B(\\alpha, \\beta)} \\theta^{(\\alpha-1)} (1-\\theta)^{(\\beta-1)} \\left(\\begin{array}{l}\\frac{1}{p(x, n)}\\end{array}\\right)\\theta^{x}(1-\\theta)^{(n-x)} / p(x)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%3D%5Cfrac%7B1%7D%7BB%28%5Calpha%2C%20%5Cbeta%29%7D%20%5Ctheta%5E%7B%28%5Calpha-1%29%7D%20%281-%5Ctheta%29%5E%7B%28%5Cbeta-1%29%7D%20%5Cleft%28%5Cbegin%7Barray%7D%7Bl%7D%5Cfrac%7B1%7D%7Bp%28x%2C%20n%29%7D%5Cend%7Barray%7D%5Cright%29%5Ctheta%5E%7Bx%7D%281-%5Ctheta%29%5E%7B%28n-x%29%7D%20%2F%20p%28x%29 "=\frac{1}{B(\alpha, \beta)} \theta^{(\alpha-1)} (1-\theta)^{(\beta-1)} \left(\begin{array}{l}\frac{1}{p(x, n)}\end{array}\right)\theta^{x}(1-\theta)^{(n-x)} / p(x)")
<br>

By definition of the binomial coefficient, which we arrive at the
solution *in terms of `posterior`*:  

![
\\underbrace{p(\\theta \\mid x)}\_{Posterior} = 
\\frac{\\frac{1}{B(\\alpha, \\beta)} \\theta^{(\\alpha-1)} (1-\\theta)^{(\\beta-1)} \\left(\\begin{array}{l}n \\\\ x\\end{array}\\right) \\theta^{x}(1-\\theta)^{(n-x)}} 
{\\underbrace{p(x)}\_{Evidence}}
](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0A%5Cunderbrace%7Bp%28%5Ctheta%20%5Cmid%20x%29%7D_%7BPosterior%7D%20%3D%20%0A%5Cfrac%7B%5Cfrac%7B1%7D%7BB%28%5Calpha%2C%20%5Cbeta%29%7D%20%5Ctheta%5E%7B%28%5Calpha-1%29%7D%20%281-%5Ctheta%29%5E%7B%28%5Cbeta-1%29%7D%20%5Cleft%28%5Cbegin%7Barray%7D%7Bl%7Dn%20%5C%5C%20x%5Cend%7Barray%7D%5Cright%29%20%5Ctheta%5E%7Bx%7D%281-%5Ctheta%29%5E%7B%28n-x%29%7D%7D%20%0A%7B%5Cunderbrace%7Bp%28x%29%7D_%7BEvidence%7D%7D%0A "
\underbrace{p(\theta \mid x)}_{Posterior} = 
\frac{\frac{1}{B(\alpha, \beta)} \theta^{(\alpha-1)} (1-\theta)^{(\beta-1)} \left(\begin{array}{l}n \\ x\end{array}\right) \theta^{x}(1-\theta)^{(n-x)}} 
{\underbrace{p(x)}_{Evidence}}
")

<br>

***Final Solution***: By rearranging to be *in terms of the `evidence`*

![
\\underbrace{p(x)}\_{Evidence} = \\frac{\\frac{1}{B(\\alpha, \\beta)} \\theta^{(\\alpha-1)} (1-\\theta)^{(\\beta-1)} \\left(\\begin{array}{l}n \\\\ x\\end{array}\\right) \\theta^{x}(1-\\theta)^{(n-x)}} {\\underbrace{p(\\theta \\mid x)}\_{Posterior}}
](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0A%5Cunderbrace%7Bp%28x%29%7D_%7BEvidence%7D%20%3D%20%5Cfrac%7B%5Cfrac%7B1%7D%7BB%28%5Calpha%2C%20%5Cbeta%29%7D%20%5Ctheta%5E%7B%28%5Calpha-1%29%7D%20%281-%5Ctheta%29%5E%7B%28%5Cbeta-1%29%7D%20%5Cleft%28%5Cbegin%7Barray%7D%7Bl%7Dn%20%5C%5C%20x%5Cend%7Barray%7D%5Cright%29%20%5Ctheta%5E%7Bx%7D%281-%5Ctheta%29%5E%7B%28n-x%29%7D%7D%20%7B%5Cunderbrace%7Bp%28%5Ctheta%20%5Cmid%20x%29%7D_%7BPosterior%7D%7D%0A "
\underbrace{p(x)}_{Evidence} = \frac{\frac{1}{B(\alpha, \beta)} \theta^{(\alpha-1)} (1-\theta)^{(\beta-1)} \left(\begin{array}{l}n \\ x\end{array}\right) \theta^{x}(1-\theta)^{(n-x)}} {\underbrace{p(\theta \mid x)}_{Posterior}}
")

------------------------------------------------------------------------

<br>

# Question 7 - 2 State MCMC

## 7. a) Find the value of `A`

-   Since the die landed on 6, and 6 is
    ![\\notin](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cnotin "\notin")
    `E1` (current acceptance set), then
-   Reject the proposal of 2 and stay at state 1
-   Therefore, `A` = State `1`

## 7. b) Find the value of `B`

-   Since we are at state `2` and the proposal state is state `1`, the
    value of alpha (or `B`) is `0.67`

## 7. c) Find the value of `C`

-   Since the die landed on 6, and 6 is
    ![\\in](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cin "\in")
    `E2` (current acceptance set), then
-   Accept the proposal and change to at state 1
-   Therefore, `C` = State `1`

## 7. d) Formula for the acceptance probability

Note below is from page 33 of *Teaching MCMC* by Stewart & Stewart:

![
\\alpha\_{i, j}=\\min \\left\\{1, \\frac{h\_{j}}{h\_{i}}\\right\\}
](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0A%5Calpha_%7Bi%2C%20j%7D%3D%5Cmin%20%5Cleft%5C%7B1%2C%20%5Cfrac%7Bh_%7Bj%7D%7D%7Bh_%7Bi%7D%7D%5Cright%5C%7D%0A "
\alpha_{i, j}=\min \left\{1, \frac{h_{j}}{h_{i}}\right\}
")

where
![\\alpha\_{i, j}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Calpha_%7Bi%2C%20j%7D "\alpha_{i, j}")
is the probability of accepting state
![j](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;j "j")
given that the sampler is at state
![i](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;i "i").

------------------------------------------------------------------------

<br>

# Question 8 - SLR JAGS

## 8. a) SLR Doodle Bug

<img src="openBugsSLR.png" style="width:80.0%" />

## 8. b) JAGS Code

> Jags code for the model using uniform’s on the sigma’s

-   The data came as a data frame of x and y values with a total of n
    rows

``` r
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
```

## 8. c) Logical Node?

-   `tau` is considered a logical node,
-   Because `tau` is logically related to `sigma`
-   `tau` is a function of sigma.

## 8. d) Linear Predictors?

The linear predictor variables are:
![\\beta_0 +\\beta_1 x](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cbeta_0%20%2B%5Cbeta_1%20x "\beta_0 +\beta_1 x")

------------------------------------------------------------------------

<br>

# Question 9 - Change Point Regression

``` r
dataList = list(x = c(-10, -9, -8, -7, -6, -5, -4, -3, -2, -1, 0, 1, 2, 
3, 4, 5, 6, 7, 8, 9, 10), y = c(21.72, 20.41, 14.06, 14.6, 13.8, 
10.4, 8.22, 6.79, 4.92, 0.45, 4.57, 1.95, 10.14, 13.28, 21.88, 
18.88, 22.91, 23.53, 34.99, 35.07, 39.17), N = 21)
```

``` r
df = data.frame(x = dataList$x, y=dataList$y)
head(df)
```

    ##     x     y
    ## 1 -10 21.72
    ## 2  -9 20.41
    ## 3  -8 14.06
    ## 4  -7 14.60
    ## 5  -6 13.80
    ## 6  -5 10.40

``` r
library(ggplot2)
basePlot <- ggplot(df, aes(x=x, y=y)) +
              geom_smooth(method = 'loess', se = TRUE, 
                          color = 'steelblue3', fill = 'grey90', alpha = 0.5) + 
              geom_point(color ='tomato3', alpha = 0.5, size = 3) +
              theme_minimal() +
              labs(title = 'Plot of Data',
                   subtitle = 'Daniel Carpenter')
basePlot
```

![](FinalExam_files/figure-gfm/plotPiece-1.png)<!-- -->

## 9. b) JAGS Piecewise

### 9. b,i) Jags Code

``` r
library(rjags)
#Define the model:
modelString = "
model {
  for (i in 1:N) {
    y[i]    ~ dnorm(mu[i], tau)
    mu[i]  <- beta0 + beta[1]*x[i] + beta[2]*(x[i] - theta)
            * step(x[i] - theta)
  }
  tau       ~ dgamma(0.001, 0.001)
  beta0     ~ dnorm(0.0, 1.0E-6)
  
  for (j in 1:2) {
    beta[j] ~ dnorm(0.0, 1.0E-6)
  }
  sigma    <- 1/sqrt(tau)
  theta     ~ dunif(-1.3, 1.1)
  
  int2 <- beta0 - beta[1]*theta
  slope2 <- beta[1] + beta[2]
}
" # close quote for modelString
writeLines( modelString , con="TEMPmodel.txt" )

initsList = list(tau = 1, beta0 = 2, beta = c(1, 2), theta = 1)

# Run the chains:
jagsModel = jags.model( file="TEMPmodel.txt" , data=dataList , inits=initsList , 
                        n.chains=3 , n.adapt=500 )
```

    ## Compiling model graph
    ##    Resolving undeclared variables
    ##    Allocating nodes
    ## Graph information:
    ##    Observed stochastic nodes: 21
    ##    Unobserved stochastic nodes: 5
    ##    Total graph size: 165
    ## 
    ## Initializing model

``` r
update( jagsModel , n.iter=500 )
codaSamples = coda.samples( jagsModel , variable.names=c("tau", "sigma",
                                                         'beta', 'beta0', 'theta',
                                                         'int2', 'slope2'),
                            n.iter=33340 )
save( codaSamples , file=paste0("FinalExamJags","Mcmc.Rdata") )
```

### 9. b,ii) Interpret Summary Output

``` r
# Uncomment to see some diagnostics - left here because high autocorrelation
# diagMCMC( codaObject=codaSamples , parName="beta0" )
# diagMCMC( codaObject=codaSamples , parName="beta[1]" )
# diagMCMC( codaObject=codaSamples , parName="int2" )
# diagMCMC( codaObject=codaSamples , parName="slope2" )

# Quick check
library(ggmcmc)
s = ggs(codaSamples)
ggs_density(s)
```

![](FinalExam_files/figure-gfm/summaryPiece-1.png)<!-- -->

``` r
# Output of model
su = summary(codaSamples)
su
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
    ##             Mean     SD  Naive SE Time-series SE
    ## beta[1] -2.11610 0.2878 0.0009101      0.0052203
    ## beta[2]  5.80429 0.3670 0.0011604      0.0047173
    ## beta0   -0.03299 1.8007 0.0056938      0.0336505
    ## int2    -0.87171 2.6439 0.0083599      0.0504985
    ## sigma    2.49377 0.4544 0.0014369      0.0023305
    ## slope2   3.68820 0.2610 0.0008253      0.0028760
    ## tau      0.17601 0.0600 0.0001897      0.0002979
    ## theta   -0.35613 0.4397 0.0013904      0.0074103
    ## 
    ## 2. Quantiles for each variable:
    ## 
    ##             2.5%     25%      50%      75%   97.5%
    ## beta[1] -2.66999 -2.3101 -2.12318 -1.92573 -1.5355
    ## beta[2]  5.07492  5.5667  5.80521  6.04378  6.5297
    ## beta0   -3.47302 -1.2610 -0.07798  1.16685  3.5780
    ## int2    -5.97417 -2.6779 -0.94293  0.93422  4.3215
    ## sigma    1.79235  2.1718  2.43145  2.74680  3.5561
    ## slope2   3.20662  3.5128  3.67466  3.85070  4.2412
    ## tau      0.07908  0.1325  0.16915  0.21201  0.3113
    ## theta   -1.11506 -0.6674 -0.40226 -0.08898  0.6087

#### Interpret the value of theta using both the point estimate and 95% BCI

-   Point estimate: the mean value of theta is -0.35806 with a standard
    deviation of 0.44068
-   Inteval estimate: There is a 95% probability that the value of theta
    will fall between -1.12416 and 0.6013
-   Above values rounded since each MCMC run varies in results (since it
    is an estimation)

### 9. b,iii) Plot Estimating Lines over Data

``` r
# Create objects using summary stats
beta0  = su$statistics[,'Mean']['beta0']
beta1  = su$statistics[,'Mean']['beta[1]']
beta2  = su$statistics[,'Mean']['beta[2]']
int2   = su$statistics[,'Mean']['int2']
slope2 = su$statistics[,'Mean']['slope2']
theta  = su$statistics[,'Mean']['theta']

# Plot the two estimated linear pieces with the cuttoff point
basePlot + 
  
  # Piece 1
  geom_abline(intercept = beta0, slope = beta1 , 
              linetype = 'dotdash', size = 1, color = 'grey40')  +  
  
  
  # Piece 2
  geom_abline(intercept = int2,  slope = slope2, 
              linetype = 'dotdash', size = 1, color = 'grey40') + # Piece 2
  geom_vline(xintercept = theta)                 + # Intersection 
  labs(title = 'Plot of data with Piece-Wise Linear Regression Estimates using MCMC')
```

![](FinalExam_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

### Final Remarks on Enhancements to model

-   Note the intercept of the second piece may or may not reflect the
    data (See graph)
-   Note high autocorelation among the variables.
-   Centering will not fix this issue, but I anticipate that a potential
    enhancement to the model would be to determine a fix for the
    intercept issue.

------------------------------------------------------------------------

<br>

# Question 10 - Centering Data

Below shows the data

``` r
# For x and y
x=50:80
set.seed(24)
y=10+2*x+rnorm(31,0,10)

# For xx and yy (notice centered around 0)
xx=-15:15
set.seed(24)
yy=10+2*xx+rnorm(31,0,10)
```

## 10. a) Plot data

``` r
df2 = data.frame(x=x, 
                y=y,
                
                xx=xx,
                yy=yy)
head(df2)
```

    ##    x        y  xx        yy
    ## 1 50 104.5412 -15 -25.45881
    ## 2 51 117.3659 -14 -12.63415
    ## 3 52 118.1962 -13 -11.80377
    ## 4 53 110.1637 -12 -19.83627
    ## 5 54 126.4746 -11  -3.52540
    ## 6 55 122.6602 -10  -7.33978

``` r
require(ggplot2)
ggplot(df2) +
  
  # Emphasize the x and y axis
  geom_hline(aes(yintercept = 0), size = 1, color = 'grey60') +
  geom_vline(aes(xintercept = 0), size = 1, color = 'grey60') + 
  
  # Two grouping
  geom_point(aes(x=x,  y=y ), color ='darkseagreen', alpha = 0.75, size = 3) +
  geom_point(aes(x=xx, y=yy), color ='steelblue3',    alpha = 0.75, size = 3) +
  
  theme_minimal() +
  labs(title = 'Plot of Data',
       subtitle = 'Daniel Carpenter')
```

![](FinalExam_files/figure-gfm/plot10-1.png)<!-- -->

## 10. b) 2 SLR Jags Models

### This is the base function for each SLR Model to run

``` r
MyJagsSLR <- function(y, x ) {
  
  Ntotal = length(y)  # Compute the total number of x,y pairs.
  dataList = list(    # Put the information into a list.
    x = x,
    y = y ,
    Ntotal = Ntotal 
  )
  
  modelStringSLR = "
  model{
      for( i in 1 : Ntotal ) {
          y[i] ~ dnorm(mu[i], tau)
          mu[i] <- beta0 + beta1 * x[i]
      }
      beta0 ~ dnorm(0.0, 1.0E-6)
      beta1 ~ dnorm(0.0, 1.0E-6)
      sigma ~ dunif(0, 1000)
      tau <- pow(sigma,  -2)
  }
  " # close quote for modelString
  writeLines( modelStringSLR , con="slrModel.txt" )
  
  # Initialize the chains based on MLE of data.
  initsList = list(beta0 = 0, beta1 = 0, sigma =10)
  
  # Run the chains:
  jagsModel = jags.model( file="slrModel.txt" , data=dataList , inits=initsList , 
                          n.chains=3 , n.adapt=500 )
  update( jagsModel , n.iter=500 )
  codaSamples = coda.samples( jagsModel , variable.names=c("beta0", "beta1", "sigma") ,
                              n.iter=33340 )
  
  return(list('codaSamples' = codaSamples,
              'summary'     = summary(codaSamples)
              )
        )
}
```

### 10. b, i) Jags Model 1

``` r
# Call SLR Jags model (for x and y)
codaSamples1 <- MyJagsSLR(y=y, x=x)
```

    ## Compiling model graph
    ##    Resolving undeclared variables
    ##    Allocating nodes
    ## Graph information:
    ##    Observed stochastic nodes: 31
    ##    Unobserved stochastic nodes: 3
    ##    Total graph size: 134
    ## 
    ## Initializing model

``` r
# Retrieve the summary stats (for x and y)
codaSamples1$summary
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
    ##         Mean     SD  Naive SE Time-series SE
    ## beta0 15.737 9.7588 0.0308569       0.314148
    ## beta1  1.886 0.1488 0.0004706       0.004806
    ## sigma  7.453 1.0371 0.0032793       0.005776
    ## 
    ## 2. Quantiles for each variable:
    ## 
    ##         2.5%   25%    50%    75%  97.5%
    ## beta0 -3.356 9.217 15.733 22.338 35.016
    ## beta1  1.593 1.786  1.887  1.985  2.178
    ## sigma  5.751 6.719  7.345  8.062  9.808

### 10. b, ii) Jages Model 2

``` r
# Call SLR Jags model (for xx and yy)
codaSamples2 <- MyJagsSLR(y=yy, x=xx)
```

    ## Compiling model graph
    ##    Resolving undeclared variables
    ##    Allocating nodes
    ## Graph information:
    ##    Observed stochastic nodes: 31
    ##    Unobserved stochastic nodes: 3
    ##    Total graph size: 134
    ## 
    ## Initializing model

``` r
# Retrieve the summary stats (for xx and yy)
codaSamples2$summary
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
    ##        Mean    SD  Naive SE Time-series SE
    ## beta0 8.342 1.348 0.0042633      0.0042374
    ## beta1 1.887 0.151 0.0004774      0.0004755
    ## sigma 7.465 1.040 0.0032874      0.0047396
    ## 
    ## 2. Quantiles for each variable:
    ## 
    ##        2.5%   25%   50%   75%  97.5%
    ## beta0 5.676 7.453 8.345 9.230 11.004
    ## beta1 1.590 1.787 1.886 1.985  2.186
    ## sigma 5.755 6.731 7.351 8.070  9.821

## 10. c) Posterior Density Plots

``` r
library(ggmcmc)

# Density Plots 
ggs_density(ggs(codaSamples1$codaSamples)) # (for x  and y )
```

![](FinalExam_files/figure-gfm/postDense-1.png)<!-- -->

``` r
ggs_density(ggs(codaSamples2$codaSamples)) # (for xx and yy)
```

![](FinalExam_files/figure-gfm/postDense-2.png)<!-- -->

## 10. d) Differing Intervals Interpretation

### Explain why the intervals for ![\\beta_0](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cbeta_0 "\beta_0") are so different.

-   Since the `xx` and `yy` dataset are centered around 0, it allows
    for:
    -   convergence among the chains
    -   Low levels of cross-correlation
    -   Extremely stable results
-   It is obvious that the `x` and `y` model is not centered (visually
    and from the summary results.)
-   To overcome this, we could just make a Jags script to center the
    data around 0

### Make reference to the density plots also.

-   For the non-centered model:
    -   You can see that there are “fatter” tails for the non-centered
        model. Not stationary.
    -   Chains do not represent each other as well as the centered model

## 10. e) Classical estimates of the two models

### Use the function `lm()` and make classical 95% confidence interval estimates for the

``` r
# Non-centered data
est.nonCenter <- lm(y  ~ x , data = df2) 
summary(est.nonCenter)
```

    ## 
    ## Call:
    ## lm(formula = y ~ x, data = df2)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -14.656  -4.718   1.471   4.614  17.237 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  15.7178     9.3954   1.673    0.105    
    ## x             1.8867     0.1432  13.176 9.05e-14 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 7.131 on 29 degrees of freedom
    ## Multiple R-squared:  0.8569, Adjusted R-squared:  0.8519 
    ## F-statistic: 173.6 on 1 and 29 DF,  p-value: 9.052e-14

``` r
# Centered data
est.centered  <- lm(yy ~ xx, data = df2) 
summary(est.centered)
```

    ## 
    ## Call:
    ## lm(formula = yy ~ xx, data = df2)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -14.656  -4.718   1.471   4.614  17.237 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   8.3525     1.2808   6.521 3.85e-07 ***
    ## xx            1.8867     0.1432  13.176 9.05e-14 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 7.131 on 29 degrees of freedom
    ## Multiple R-squared:  0.8569, Adjusted R-squared:  0.8519 
    ## F-statistic: 173.6 on 1 and 29 DF,  p-value: 9.052e-14

### Compare to bayesian estimates

-   The models come up with same results, which is concerning!
-   Proves the point that you could get bad results without centering
    your data
