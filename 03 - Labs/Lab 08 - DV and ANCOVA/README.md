Lab 8 - Dummy variables and ANCOVA
================
Daniel Carpenter
March 2022

-   [Data Overview](#data-overview)
-   [Task 1: Model - (Unequal Salary
    Dataset)](#task-1-model---unequal-salary-dataset)
-   [Task 2: Interpretation - (Unequal Salary
    Dataset)](#task-2-interpretation---unequal-salary-dataset)
    -   [Interpretation of Interaction
        Results:](#interpretation-of-interaction-results)
-   [Task 3: Plot Model (Unequal Salary
    Dataset)](#task-3-plot-model-unequal-salary-dataset)
-   [Task 4: Repeat Task 1-3 on *Equal*
    Dataset](#task-4-repeat-task-1-3-on-equal-dataset)
    -   [Task 1: Model - (Equal Salary
        Dataset)](#task-1-model---equal-salary-dataset)
    -   [Task 2: Interpretation - (Equal Salary
        Dataset)](#task-2-interpretation---equal-salary-dataset)
        -   [Interpretation of Interaction
            Results:](#interpretation-of-interaction-results-1)
    -   [Task 3 Plot Model (Equal Salary
        Dataset](#task-3-plot-model-equal-salary-dataset)

> Use an `interaction` model to predict salary based on wage and
> gender  
> Goal could be to identify discrepencies in linear models, which
> magnify changes over time.

# Data Overview

Source of Data:

-   <https://onlinecourses.science.psu.edu/stat502/node/188>

-   <https://onlinecourses.science.psu.edu/stat502/node/187>

``` r
df = read.csv("salary-unequal.csv")
head(df)
```

    ##   gender salary years
    ## 1   Male     42     1
    ## 2   Male     62     2
    ## 3   Male     92     3
    ## 4   Male    112     4
    ## 5   Male    142     5
    ## 6 Female     80     5

``` r
df2 = read.csv("salary-equal.csv")
head(df2)
```

    ##   gender salary years
    ## 1   Male     78     3
    ## 2   Male     43     1
    ## 3   Male    103     5
    ## 4   Male     48     2
    ## 5   Male     80     4
    ## 6 Female     80     5

<br>

------------------------------------------------------------------------

# Task 1: Model - (Unequal Salary Dataset)

> Analyze Slopes via `Interaction` Variables

``` r
require(rjags)               # Must have previously installed package rjags.
fileNameRoot="tut12" # For output file names.

#df = read.table(file="salary-unequal.txt", sep = "\t", header =TRUE)
df = read.csv("salary-unequal.csv")
df
```

    ##    gender salary years
    ## 1    Male     42     1
    ## 2    Male     62     2
    ## 3    Male     92     3
    ## 4    Male    112     4
    ## 5    Male    142     5
    ## 6  Female     80     5
    ## 7  Female     50     3
    ## 8  Female     30     2
    ## 9  Female     20     1
    ## 10 Female     60     4

``` r
salary = df$salary
gender = df$gender
years = df$years
GM = ifelse(gender == "Male", 1,0)




Ntotal = length(salary)  # Compute the total number of data rows
dataList = list(    # Put the information into a list.
  years = years,
  salary = salary ,
  GM = GM,
  Ntotal = Ntotal 
)

#Define the model:
modelString = "
model{
  for(i in 1:Ntotal)
  {
    mu[i]<- beta0 + beta1*years[i] + beta2*GM[i] + beta3*years[i]*GM[i]
    salary[i] ~ dnorm(mu[i], tau)
  }
  
  beta0 ~ dnorm(0.0, 1.0E-6)
  beta1 ~ dnorm(0.0, 1.0E-6)
  beta2 ~ dnorm(0.0, 1.0E-6)
  beta3 ~ dnorm(0.0, 1.0E-6)
  sigma ~ dunif(0, 1000)
  tau <- pow(sigma,  -2)
}

" # close quote for modelString
writeLines( modelString , con="TEMPmodel.txt" )


#  initsList = list( theta=thetaInit )

initsList = list(beta0 = 0, beta1 = 0, beta2=0, beta3=0, sigma =10)

# Run the chains:
jagsModel = jags.model( file="TEMPmodel.txt" , data=dataList , inits=initsList , 
                        n.chains=3 , n.adapt=500 )
```

    ## Compiling model graph
    ##    Resolving undeclared variables
    ##    Allocating nodes
    ## Graph information:
    ##    Observed stochastic nodes: 10
    ##    Unobserved stochastic nodes: 5
    ##    Total graph size: 69
    ## 
    ## Initializing model

``` r
update( jagsModel , n.iter=500 )
codaSamples = coda.samples( jagsModel , variable.names=c("beta0", "beta1", "beta2" ,"beta3","sigma") ,
                            n.iter=33340 )
save( codaSamples , file=paste0(fileNameRoot,"Mcmc.Rdata") )

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
    ##         Mean    SD Naive SE Time-series SE
    ## beta0  3.056 4.636 0.014659        0.07893
    ## beta1 14.986 1.399 0.004422        0.02390
    ## beta2 11.938 6.553 0.020720        0.10993
    ## beta3 10.016 1.969 0.006227        0.03319
    ## sigma  4.095 1.699 0.005373        0.02286
    ## 
    ## 2. Quantiles for each variable:
    ## 
    ##         2.5%    25%    50%    75%  97.5%
    ## beta0 -6.138  0.424  3.021  5.645 12.360
    ## beta1 12.183 14.207 14.996 15.776 17.791
    ## beta2 -1.274  8.299 11.963 15.667 24.965
    ## beta3  6.086  8.900 10.001 11.108 13.996
    ## sigma  2.163  2.994  3.694  4.712  8.395

``` r
library(ggmcmc)
s = ggs(codaSamples)
ggs_density(s)
```

![](Lab8_files/figure-gfm/jags-1.png)<!-- -->

``` r
ggs_crosscorrelation(s)
```

![](Lab8_files/figure-gfm/jags-2.png)<!-- -->

# Task 2: Interpretation - (Unequal Salary Dataset)

Find parameter point and interval estimates. Interpret them!

``` r
est.lm <- lm(salary ~ years + GM + years:GM)
summary(est.lm)
```

    ## 
    ## Call:
    ## lm(formula = salary ~ years + GM + years:GM)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ##     -3     -3      2      2      2 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    3.000      3.317   0.905 0.400572    
    ## years         15.000      1.000  15.000 5.53e-06 ***
    ## GM            12.000      4.690   2.558 0.043001 *  
    ## years:GM      10.000      1.414   7.071 0.000401 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.162 on 6 degrees of freedom
    ## Multiple R-squared:  0.9954, Adjusted R-squared:  0.9931 
    ## F-statistic: 430.3 on 3 and 6 DF,  p-value: 2.162e-07

## Interpretation of Interaction Results:

#### 1. If `x` is increased by one unit what happens to the mean value of `y`?

1.  For each year increased, a male is paid the value of
    ![beta3](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;beta3 "beta3")
    (which on the iteration before this was 10.098, it changes since it
    is MCMC) more than a female (thousand dollars? units not known since
    link is broken to data).
2.  E.g., in year 2, a male will make 10.098
    ![\\times](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Ctimes "\times")
    2 more than the female on average.

#### 2. Will the lines ever `intersect` over the range of the data?

1.  No, the lines will never intersect

#### 3. Interpret the slope:

1.  The slope of the interaction term means the amount more that is paid
    to males than females, given years of experience.
2.  The slope of years is the amount paid to males, given years of
    experience.
3.  This should hover around 10. See
    ![beta3](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;beta3 "beta3")
    normal density plot to get a sense of the potential variation.

#### 4. Use `point interval estimation` (classical medthods) to answer question 1:

1.  For each year increased, a male is paid 10.00 more than a female.

#### 5. Interpret `Bayesian Credibility Intervals`

1.  There is a 95% probability that a male is paid 14.210 or less than a
    female (since MCMC likely to vary from above output).

# Task 3: Plot Model (Unequal Salary Dataset)

Plot the data and the estimating lines

``` r
# Plot salary on years of experience
plot(salary~years)

# List of coefficients from model above (classical)
cf2 = coef(est.lm)

# Plot the slope of each male and female
abline(coef = c(cf2[1],cf2[2]), col="darkseagreen4", lwd =3)
abline(coef = c(cf2[1]+cf2[3], cf2[2]+cf2[4]), col = "steelblue", lwd = 3)
legend("topleft",legend = c("Female","Male"),fill =c("darkseagreen4","steelblue"))
title("Male vs. Female Salaries - Unequal Dataset | Daniel Carpenter")
```

![](Lab8_files/figure-gfm/3-1.png)<!-- -->

<br>

------------------------------------------------------------------------

# Task 4: Repeat Task 1-3 on *Equal* Dataset

## Task 1: Model - (Equal Salary Dataset)

> Analyze Slopes via `Interaction` Variables

``` r
fileNameRoot="tut12" # For output file names.

#df.equal = read.table(file="salary-unequal.txt", sep = "\t", header =TRUE)
df.equal = read.csv("salary-equal.csv")
df.equal
```

    ##    gender salary years
    ## 1    Male     78     3
    ## 2    Male     43     1
    ## 3    Male    103     5
    ## 4    Male     48     2
    ## 5    Male     80     4
    ## 6  Female     80     5
    ## 7  Female     50     3
    ## 8  Female     30     2
    ## 9  Female     20     1
    ## 10 Female     60     4

``` r
salary.equal = df.equal$salary
gender.equal = df.equal$gender
years.equal = df.equal$years
GM.equal = ifelse(gender.equal == "Male", 1,0)




Ntotal.equal = length(salary.equal)  # Compute the total number of data rows
dataList = list(    # Put the information into a list.
  years = years.equal,
  salary = salary.equal ,
  GM = GM.equal,
  Ntotal = Ntotal.equal 
)

#Define the model:
modelString = "
model{
  for(i in 1:Ntotal)
  {
    mu[i]<- beta0 + beta1*years[i] + beta2*GM[i] + beta3*years[i]*GM[i]
    salary[i] ~ dnorm(mu[i], tau)
  }
  
  beta0 ~ dnorm(0.0, 1.0E-6)
  beta1 ~ dnorm(0.0, 1.0E-6)
  beta2 ~ dnorm(0.0, 1.0E-6)
  beta3 ~ dnorm(0.0, 1.0E-6)
  sigma ~ dunif(0, 1000)
  tau <- pow(sigma,  -2)
}

" # close quote for modelString
writeLines( modelString , con="TEMPmodel.equal.txt" )


#  initsList = list( theta=thetaInit )

initsList = list(beta0 = 0, beta1 = 0, beta2=0, beta3=0, sigma =10)

# Run the chains:
jagsModel.equal = jags.model( file="TEMPmodel.equal.txt" , data=dataList , inits=initsList , 
                        n.chains=3 , n.adapt=500 )
```

    ## Compiling model graph
    ##    Resolving undeclared variables
    ##    Allocating nodes
    ## Graph information:
    ##    Observed stochastic nodes: 10
    ##    Unobserved stochastic nodes: 5
    ##    Total graph size: 69
    ## 
    ## Initializing model

``` r
update( jagsModel.equal , n.iter=500 )
codaSamples.equal = coda.samples( jagsModel.equal , variable.names=c("beta0", "beta1", "beta2" ,"beta3","sigma") ,
                            n.iter=33340 )
save( codaSamples.equal , file=paste0(fileNameRoot,"Mcmc.Rdata") )

summary(codaSamples.equal)
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
    ##          Mean     SD Naive SE Time-series SE
    ## beta0  2.7665  8.154 0.025782        0.13783
    ## beta1 15.0649  2.460 0.007779        0.04179
    ## beta2 22.1133 11.413 0.036086        0.19849
    ## beta3  0.1145  3.447 0.010900        0.05951
    ## sigma  7.2156  2.909 0.009199        0.03424
    ## 
    ## 2. Quantiles for each variable:
    ## 
    ##           2.5%    25%     50%    75%  97.5%
    ## beta0 -13.6778 -1.832  2.8449  7.503 18.736
    ## beta1  10.1933 13.642 15.0429 16.465 20.020
    ## beta2  -0.4358 15.447 21.9867 28.629 45.040
    ## beta3  -6.7849 -1.839  0.1519  2.115  6.924
    ## sigma   3.8171  5.289  6.5309  8.311 14.742

``` r
library(ggmcmc)
s = ggs(codaSamples.equal)
ggs_density(s)
```

![](Lab8_files/figure-gfm/jags2-1.png)<!-- -->

``` r
ggs_crosscorrelation(s)
```

![](Lab8_files/figure-gfm/jags2-2.png)<!-- -->

## Task 2: Interpretation - (Equal Salary Dataset)

Find parameter point and interval estimates. Interpret them!

``` r
est.lm.equal <- lm(salary.equal ~ years.equal + GM + years.equal:GM.equal)
summary(est.lm.equal)
```

    ## 
    ## Call:
    ## lm(formula = salary.equal ~ years.equal + GM + years.equal:GM.equal)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ##  -7.20  -3.00   2.00   2.15   7.60 
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)             3.000      5.821   0.515 0.624702    
    ## years.equal            15.000      1.755   8.547 0.000141 ***
    ## GM                     21.800      8.232   2.648 0.038114 *  
    ## years.equal:GM.equal    0.200      2.482   0.081 0.938395    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 5.55 on 6 degrees of freedom
    ## Multiple R-squared:  0.9692, Adjusted R-squared:  0.9538 
    ## F-statistic: 62.93 on 3 and 6 DF,  p-value: 6.318e-05

### Interpretation of Interaction Results:

#### 1. If `x` is increased by one unit what happens to the mean value of `y`?

1.  For each year increased, a male is paid
    ![beta3](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;beta3 "beta3")
    (which on the iteration before this was 0.1916, it changes since it
    is MCMC) more than a female (thousand dollars? units not known since
    link is broken to data).
2.  E.g., in year 2, a male will make 0.1916
    ![\\times](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Ctimes "\times")
    2 more than the female on average.
3.  This should hover around 0. See
    ![beta3](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;beta3 "beta3")
    normal density plot to get a sense of the potential variation.

#### 2. Will the lines ever intersect over the range of the data?

1.  No, the lines will never intersect, but they run nearly parallel,
    hence “equal”

#### 3. What meaning does the slope have?

1.  The slope of the interaction term means the amount more that is paid
    to males than females, given years of experience.
2.  The slope of years is the amount paid to males, given years of
    experience.

#### 4. Use `point interval estimation` (classical medthods) to answer question 1:

1.  For each year increased, a male is paid 0.200 more than a female.

#### 5. Interpret BCI’s (Bayesian credible intervals) – these are probability intervals and NOT Confidence Intervals.

1.  There is a 95% probability that a male is paid 0.9383 or less than a
    female (since MCMC likely to vary from above output).

## Task 3 Plot Model (Equal Salary Dataset

Plot the data and the estimating lines

``` r
# Plot salary on years of experience
plot(salary.equal~years.equal)

# List of coefficients from model above (classical)
cf2.equal = coef(est.lm.equal)
cf2.equal
```

    ##          (Intercept)          years.equal                   GM 
    ##                  3.0                 15.0                 21.8 
    ## years.equal:GM.equal 
    ##                  0.2

``` r
# Plot the slope of each male and female
abline(coef = c(cf2.equal[1],cf2.equal[2]), col="darkseagreen4", lwd =3)
abline(coef = c(cf2.equal[1]+cf2.equal[3], cf2.equal[2]+cf2.equal[4]), col = "steelblue", lwd = 3)
legend("topleft",legend = c("Female","Male"),fill =c("darkseagreen4","steelblue"))
title("Male vs. Female Salaries - Equal Dataset | Daniel Carpenter")
```

![](Lab8_files/figure-gfm/3b-1.png)<!-- -->
