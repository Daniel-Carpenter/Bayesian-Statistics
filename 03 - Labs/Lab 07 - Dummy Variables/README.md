Laboratory 7 - Dummy Variables
================
Daniel Carpenter
March 2022

-   [1 `Classical Estimation` with Categorical
    Valiables](#classical-estimation-with-categorical-valiables)
    -   [1.1 Categorical Variables
        Overview](#categorical-variables-overview)
    -   [1.2 Using classical methods for Linear
        Model](#using-classical-methods-for-linear-model)
    -   [1.3 Model Matrix and Dummy
        Variables](#model-matrix-and-dummy-variables)
    -   [1.4 Linear Model `LaTeX`
        Expression](#linear-model-latex-expression)
-   [2 `Bayesian Estimation` with Categorical
    Valiables](#bayesian-estimation-with-categorical-valiables)
    -   [2.1 Task 1: `OpenBUGS` Linear Model with `mtcars`
        dataset](#task-1-openbugs-linear-model-with-mtcars-dataset)
    -   [2.2 Task 2: *Pretty Print* Model into `JAGS`
        Script](#task-2-pretty-print-model-into-jags-script)
        -   [2.2.1 JAGS Script with Pretty Printed
            Model](#jags-script-with-pretty-printed-model)
        -   [2.2.2 Interpretation of MCMC
            Results:](#interpretation-of-mcmc-results)

> **Goal**: Compare `Classical` and `Bayesian` approaches to estimate
> linear model containing dummy variables (categorical).  
> Useful techniques in setting up dummy variable via `model.matrix()`

# 1 `Classical Estimation` with Categorical Valiables

## 1.1 Categorical Variables Overview

In many applications we will need to incorporate categorical variables
into our regression. How is this to be done?

``` r
# Show the Data
data(mtcars)
head(mtcars)
```

    ##                    mpg cyl disp  hp drat    wt  qsec vs am gear carb
    ## Mazda RX4         21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
    ## Mazda RX4 Wag     21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
    ## Datsun 710        22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
    ## Hornet 4 Drive    21.4   6  258 110 3.08 3.215 19.44  1  0    3    1
    ## Hornet Sportabout 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2
    ## Valiant           18.1   6  225 105 2.76 3.460 20.22  1  0    3    1

``` r
# Show the number of rows in mtcars
dim(mtcars)[1]
```

    ## [1] 32

We will define some variables to be factors

``` r
# Convert categorical variables to factor class, store in `mycars`
mycars = within(mtcars, {
  cylF<-factor(cyl)
  vsF <- factor(vs)
  amF <- factor(am)
  gearF <- factor(gear)
  carbF <- factor(carb)
})

# See mycars after converting to factor class on cat. vars.
head(mycars)
```

    ##                    mpg cyl disp  hp drat    wt  qsec vs am gear carb carbF
    ## Mazda RX4         21.0   6  160 110 3.90 2.620 16.46  0  1    4    4     4
    ## Mazda RX4 Wag     21.0   6  160 110 3.90 2.875 17.02  0  1    4    4     4
    ## Datsun 710        22.8   4  108  93 3.85 2.320 18.61  1  1    4    1     1
    ## Hornet 4 Drive    21.4   6  258 110 3.08 3.215 19.44  1  0    3    1     1
    ## Hornet Sportabout 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2     2
    ## Valiant           18.1   6  225 105 2.76 3.460 20.22  1  0    3    1     1
    ##                   gearF amF vsF cylF
    ## Mazda RX4             4   1   0    6
    ## Mazda RX4 Wag         4   1   0    6
    ## Datsun 710            4   1   1    4
    ## Hornet 4 Drive        3   0   1    6
    ## Hornet Sportabout     3   0   0    8
    ## Valiant               3   0   1    6

``` r
# Notice how many cars in sample have x number of carbs
table(mycars$carbF)
```

    ## 
    ##  1  2  3  4  6  8 
    ##  7 10  3 10  1  1

## 1.2 Using classical methods for Linear Model

``` r
# Create a linear model: How does displacement and the number of carborators effect mpg
# Store in new var
mpglm = lm(mpg~disp + carbF, data =mycars )
summary(mpglm)
```

    ## 
    ## Call:
    ## lm(formula = mpg ~ disp + carbF, data = mycars)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.3359 -1.6135  0.0321  1.1194  6.3592 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 30.014542   1.350232  22.229  < 2e-16 ***
    ## disp        -0.034793   0.005309  -6.554 7.26e-07 ***
    ## carbF2      -0.372064   1.545757  -0.241   0.8118    
    ## carbF3      -4.118676   2.224379  -1.852   0.0759 .  
    ## carbF4      -3.479816   1.759031  -1.978   0.0590 .  
    ## carbF6      -5.269580   3.243950  -1.624   0.1168    
    ## carbF8      -4.541896   3.362060  -1.351   0.1888    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.034 on 25 degrees of freedom
    ## Multiple R-squared:  0.7956, Adjusted R-squared:  0.7466 
    ## F-statistic: 16.22 on 6 and 25 DF,  p-value: 1.544e-07

## 1.3 Model Matrix and Dummy Variables

We can extract the `design matrix` by using `model.matrix()`:

``` r
# Create a model matrix, which converts the linear model object
# into a form which outlines the dummy variables for each factor in carborator
# IOnterpret as:
  # if 1, then it has x number of carborators,
  # Else if all 0, then the base (1 carborators, notice not in the file) is present in car
mm=model.matrix(mpglm)
df = as.data.frame(mm)

lst = as.list(df)
df
```

    ##                     (Intercept)  disp carbF2 carbF3 carbF4 carbF6 carbF8
    ## Mazda RX4                     1 160.0      0      0      1      0      0
    ## Mazda RX4 Wag                 1 160.0      0      0      1      0      0
    ## Datsun 710                    1 108.0      0      0      0      0      0
    ## Hornet 4 Drive                1 258.0      0      0      0      0      0
    ## Hornet Sportabout             1 360.0      1      0      0      0      0
    ## Valiant                       1 225.0      0      0      0      0      0
    ## Duster 360                    1 360.0      0      0      1      0      0
    ## Merc 240D                     1 146.7      1      0      0      0      0
    ## Merc 230                      1 140.8      1      0      0      0      0
    ## Merc 280                      1 167.6      0      0      1      0      0
    ## Merc 280C                     1 167.6      0      0      1      0      0
    ## Merc 450SE                    1 275.8      0      1      0      0      0
    ## Merc 450SL                    1 275.8      0      1      0      0      0
    ## Merc 450SLC                   1 275.8      0      1      0      0      0
    ## Cadillac Fleetwood            1 472.0      0      0      1      0      0
    ## Lincoln Continental           1 460.0      0      0      1      0      0
    ## Chrysler Imperial             1 440.0      0      0      1      0      0
    ## Fiat 128                      1  78.7      0      0      0      0      0
    ## Honda Civic                   1  75.7      1      0      0      0      0
    ## Toyota Corolla                1  71.1      0      0      0      0      0
    ## Toyota Corona                 1 120.1      0      0      0      0      0
    ## Dodge Challenger              1 318.0      1      0      0      0      0
    ## AMC Javelin                   1 304.0      1      0      0      0      0
    ## Camaro Z28                    1 350.0      0      0      1      0      0
    ## Pontiac Firebird              1 400.0      1      0      0      0      0
    ## Fiat X1-9                     1  79.0      0      0      0      0      0
    ## Porsche 914-2                 1 120.3      1      0      0      0      0
    ## Lotus Europa                  1  95.1      1      0      0      0      0
    ## Ford Pantera L                1 351.0      0      0      1      0      0
    ## Ferrari Dino                  1 145.0      0      0      0      1      0
    ## Maserati Bora                 1 301.0      0      0      0      0      1
    ## Volvo 142E                    1 121.0      1      0      0      0      0

Also see the data used in the model

``` r
# See that you can also remake the original data frame
df2=model.frame(mpglm)
head(df2)
```

    ##                    mpg disp carbF
    ## Mazda RX4         21.0  160     4
    ## Mazda RX4 Wag     21.0  160     4
    ## Datsun 710        22.8  108     1
    ## Hornet 4 Drive    21.4  258     1
    ## Hornet Sportabout 18.7  360     2
    ## Valiant           18.1  225     1

``` r
# Use dput to output the df in the form for JAGS model
# Can use this to encapsulate data and reproduce the data frame
dput(df2)
```

    ## structure(list(mpg = c(21, 21, 22.8, 21.4, 18.7, 18.1, 14.3, 
    ## 24.4, 22.8, 19.2, 17.8, 16.4, 17.3, 15.2, 10.4, 10.4, 14.7, 32.4, 
    ## 30.4, 33.9, 21.5, 15.5, 15.2, 13.3, 19.2, 27.3, 26, 30.4, 15.8, 
    ## 19.7, 15, 21.4), disp = c(160, 160, 108, 258, 360, 225, 360, 
    ## 146.7, 140.8, 167.6, 167.6, 275.8, 275.8, 275.8, 472, 460, 440, 
    ## 78.7, 75.7, 71.1, 120.1, 318, 304, 350, 400, 79, 120.3, 95.1, 
    ## 351, 145, 301, 121), carbF = structure(c(4L, 4L, 1L, 1L, 2L, 
    ## 1L, 4L, 2L, 2L, 4L, 4L, 3L, 3L, 3L, 4L, 4L, 4L, 1L, 2L, 1L, 1L, 
    ## 2L, 2L, 4L, 2L, 1L, 2L, 2L, 4L, 5L, 6L, 2L), .Label = c("1", 
    ## "2", "3", "4", "6", "8"), class = "factor")), terms = mpg ~ disp + 
    ##     carbF, row.names = c("Mazda RX4", "Mazda RX4 Wag", "Datsun 710", 
    ## "Hornet 4 Drive", "Hornet Sportabout", "Valiant", "Duster 360", 
    ## "Merc 240D", "Merc 230", "Merc 280", "Merc 280C", "Merc 450SE", 
    ## "Merc 450SL", "Merc 450SLC", "Cadillac Fleetwood", "Lincoln Continental", 
    ## "Chrysler Imperial", "Fiat 128", "Honda Civic", "Toyota Corolla", 
    ## "Toyota Corona", "Dodge Challenger", "AMC Javelin", "Camaro Z28", 
    ## "Pontiac Firebird", "Fiat X1-9", "Porsche 914-2", "Lotus Europa", 
    ## "Ford Pantera L", "Ferrari Dino", "Maserati Bora", "Volvo 142E"
    ## ), class = "data.frame")

## 1.4 Linear Model `LaTeX` Expression

*y* = *β*<sub>0</sub> + *β*<sub>1</sub>*x* + *β*<sub>2</sub>*c**a**r**b**F*2 + *β*<sub>3</sub>*c**a**r**b**F*3 + *β*<sub>4</sub>*c**a**r**b**F*4 + *β*<sub>5</sub>*c**a**r**b**F*6 + *β*<sub>6</sub>*c**a**r**b**F*8 + *ϵ*

# 2 `Bayesian Estimation` with Categorical Valiables

## 2.1 Task 1: `OpenBUGS` Linear Model with `mtcars` dataset

-   Make up the model file by using OpenBUGS and creating a doodle in
    the doodle editor. The model will be a regression (just modify the
    SLR model to include the dummy variables). Take a picture of the
    doodle once it checks out and place here using
    `![](.png){width=80%}`

<img src="openBugsModel.png" style="width:80.0%" />

## 2.2 Task 2: *Pretty Print* Model into `JAGS` Script

-   Use `pretty print` (this will only work once the model is checked -
    use the menu!) and paste the model code into the modelString below:
    Fill in all other needed parameters and summarize the `codaSamples`
    using summary() – to run the chunk you will need to remove
    `eval=FALSE`

### 2.2.1 JAGS Script with Pretty Printed Model

``` r
# Get rid of the intercept
dput(df[,-1])
```

    ## structure(list(disp = c(160, 160, 108, 258, 360, 225, 360, 146.7, 
    ## 140.8, 167.6, 167.6, 275.8, 275.8, 275.8, 472, 460, 440, 78.7, 
    ## 75.7, 71.1, 120.1, 318, 304, 350, 400, 79, 120.3, 95.1, 351, 
    ## 145, 301, 121), carbF2 = c(0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 0, 
    ## 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 0, 1), 
    ##     carbF3 = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 
    ##     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), carbF4 = c(1, 
    ##     1, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 0, 1, 1, 1, 0, 0, 0, 
    ##     0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0), carbF6 = c(0, 0, 0, 
    ##     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    ##     0, 0, 0, 0, 0, 0, 0, 1, 0, 0), carbF8 = c(0, 0, 0, 0, 0, 
    ##     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    ##     0, 0, 0, 0, 0, 0, 1, 0)), class = "data.frame", row.names = c("Mazda RX4", 
    ## "Mazda RX4 Wag", "Datsun 710", "Hornet 4 Drive", "Hornet Sportabout", 
    ## "Valiant", "Duster 360", "Merc 240D", "Merc 230", "Merc 280", 
    ## "Merc 280C", "Merc 450SE", "Merc 450SL", "Merc 450SLC", "Cadillac Fleetwood", 
    ## "Lincoln Continental", "Chrysler Imperial", "Fiat 128", "Honda Civic", 
    ## "Toyota Corolla", "Toyota Corona", "Dodge Challenger", "AMC Javelin", 
    ## "Camaro Z28", "Pontiac Firebird", "Fiat X1-9", "Porsche 914-2", 
    ## "Lotus Europa", "Ford Pantera L", "Ferrari Dino", "Maserati Bora", 
    ## "Volvo 142E"))

``` r
# BE SURE TO GET RID OF EVAL=FALSE
require(rjags)               # Must have previously installed package rjags.
```

    ## Loading required package: rjags

    ## Loading required package: coda

    ## Linked to JAGS 4.3.0

    ## Loaded modules: basemod,bugs

``` r
Ntotal = dim(mtcars)[1]
fileNameRoot="categotical" # For output file names. (from using dput)
dataList=list(mpg = c(21, 21, 22.8, 21.4, 18.7, 18.1, 14.3, 
24.4, 22.8, 19.2, 17.8, 16.4, 17.3, 15.2, 10.4, 10.4, 14.7, 32.4, 
30.4, 33.9, 21.5, 15.5, 15.2, 13.3, 19.2, 27.3, 26, 30.4, 15.8, 
19.7, 15, 21.4),disp = c(160, 160, 108, 258, 360, 225, 360, 146.7, 
140.8, 167.6, 167.6, 275.8, 275.8, 275.8, 472, 460, 440, 78.7,
75.7, 71.1, 120.1, 318, 304, 350, 400, 79, 120.3, 95.1, 351,
145, 301, 121), carbF2 = c(0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 0,
0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 0, 1),
    carbF3 = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), carbF4 = c(1,
    1, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 0, 1, 1, 1, 0, 0, 0,
    0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0), carbF6 = c(0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 1, 0, 0), carbF8 = c(0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 1, 0), Ntotal = Ntotal)


#Define the model: -- >> define this using OPENBUGS <<
modelString = "
model{
    for( i in 1 : Ntotal ) {
        mpg[i] ~ dnorm(mu[i], tau)
        mu[i] <- beta0 + beta1 * disp[i] + beta2 * carbF2[i] + beta3 * carbF3[i] + beta4 * carbF4[i] + beta5 * carbF6[i] + beta6 * carbF8[i]
    }
    beta0 ~ dnorm(0.0, 1.0E-6)
    beta1 ~ dnorm(0.0, 1.0E-6)
    beta2 ~ dnorm(0.0, 1.0E-6)
    beta3 ~ dnorm(0.0, 1.0E-6)
    beta4 ~ dnorm(0.0, 1.0E-6)
    beta5 ~ dnorm(0.0, 1.0E-6)
    beta6 ~ dnorm(0.0, 1.0E-6)
    sigma ~ dunif(0, 1000)
    tau <- pow(sigma,  -2)
}
" # close quote for modelString
writeLines( modelString , con="TEMPmodel.txt" )

# Initialize the chains based on MLE of data.
# Option: Use single initial value for all chains:
#  thetaInit = sum(y)/length(y)
#  initsList = list( theta=thetaInit )

# >> Make sure that initial values are good <<
initsList = list(beta0 = 0, beta1 = 0, beta2=0, beta3=0, beta4=0, betaq5=0, beta6=0, sigma =10)

# Run the chains:
jagsModel = jags.model( file="TEMPmodel.txt" , data=dataList , inits=initsList , 
                        n.chains=3 , n.adapt=500 )
```

    ## Compiling model graph
    ##    Resolving undeclared variables
    ##    Allocating nodes
    ## Graph information:
    ##    Observed stochastic nodes: 32
    ##    Unobserved stochastic nodes: 8
    ##    Total graph size: 304

    ## Warning in jags.model(file = "TEMPmodel.txt", data = dataList, inits =
    ## initsList, : Unused initial value for "betaq5" in chain 1

    ## Warning in jags.model(file = "TEMPmodel.txt", data = dataList, inits =
    ## initsList, : Unused initial value for "betaq5" in chain 2

    ## Warning in jags.model(file = "TEMPmodel.txt", data = dataList, inits =
    ## initsList, : Unused initial value for "betaq5" in chain 3

    ## Initializing model

``` r
update( jagsModel , n.iter=500 )
codaSamples = coda.samples( jagsModel , variable.names=c("beta0", "beta1", "beta2", 
                                                         "beta3", "beta4", "beta5", "beta6" ,"sigma") ,
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
    ##           Mean       SD  Naive SE Time-series SE
    ## beta0 30.05279 1.427046 4.512e-03      1.392e-02
    ## beta1 -0.03491 0.005607 1.773e-05      5.597e-05
    ## beta2 -0.38778 1.641617 5.191e-03      1.332e-02
    ## beta3 -4.12191 2.364510 7.476e-03      1.567e-02
    ## beta4 -3.47936 1.867338 5.904e-03      1.620e-02
    ## beta5 -5.30286 3.456070 1.093e-02      1.577e-02
    ## beta6 -4.53501 3.582936 1.133e-02      1.848e-02
    ## sigma  3.19872 0.484704 1.533e-03      2.775e-03
    ## 
    ## 2. Quantiles for each variable:
    ## 
    ##            2.5%      25%     50%      75%    97.5%
    ## beta0  27.25197 29.11263 30.0483 30.98162 32.89882
    ## beta1  -0.04611 -0.03858 -0.0349 -0.03123 -0.02385
    ## beta2  -3.65336 -1.45998 -0.3916  0.68415  2.86152
    ## beta3  -8.79233 -5.65959 -4.1191 -2.57569  0.54912
    ## beta4  -7.18842 -4.69434 -3.4847 -2.26070  0.20744
    ## beta5 -12.15800 -7.56399 -5.3022 -3.03173  1.50410
    ## beta6 -11.62527 -6.89336 -4.5541 -2.18801  2.56281
    ## sigma   2.41683  2.85519  3.1412  3.47907  4.29933

### 2.2.2 Interpretation of MCMC Results:

-   Note that the results of the MCMC output result in a very similar
    outcome as the classical methods, as seen in the output of `mpglm`
    object
-   The mean value of *β*<sub>1</sub> is -0.03484 with a standard
    deviation of 0.005609
-   If the car as two carburetors (`carbF2`, or *β*<sub>1</sub>), the
    miles per gallon decreases by -0.03484 miles per gallon.
-   There is a probability of 95% that 2 carburetors will decrease a
    car’s miles per gallon by 0.024 to 0.046 (MPG).
