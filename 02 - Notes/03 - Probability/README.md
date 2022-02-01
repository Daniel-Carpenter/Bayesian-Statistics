# Probability

## Resources

## Outline
1. Distributions
2. Running Probability
3. Plotting Probability Distributions
4. Calculating Probabilities
5. Conditional Probability

## Types of Probabilities
1. Inside the Head (`Personal/Subjective Beliefs`)
2. Outside the Head (`Long Run Frequency`) (calculate the long-run frequency; e.g., simulations or using data) 

## 1. Distributions Overview

Type | Description
-----|--------------
`Bernoulli Distribution` | Probability of getting a *head*
`Binomial Distribution`  | Random variable `X` that takes `N` bernoulli trials

---
<br>

## 2. Running Probability of Success
```R
# Function for running Probability of success (head)
mycoin = function(iter,pr=1/2)
{
  if(!require(ggplot2)) {install.packages(ggplot2)}
  
  # 1 = head (success), 0 = tail
  face = rbinom(iter,size = 1, prob = pr) # size 1 = Bernoulli Dist 
  
  # Cumulative Sum of successes
  x = cumsum(face)
  n = 1:iter
  p = x/n # running probability of a success
  p.df=data.frame(p = p, n = n)
  
  dev.new(noRStudioGD = TRUE)
  
  # Plot the running probability
  g = ggplot(p.df, aes(x= n , y = p, col = p )) + geom_point()
  g = g + geom_hline(yintercept = pr)  + ggtitle("Long run frequency coin tossing")
  
  print(g)
}

# Flip a coin 3000 times and see the running probability of a head (success)
mycoin(3000)
```
> Output  
<img src = "Images/runningProb.png" width = 550> <br>

## 3. Plotting Probability Distributions
> Common plots: `plot()`, `curve()`

### `ggplot2` Method
* Normal distribution and shade between x values of -5 and 10

```R
# plotting areas in ggplot

dev.new(noRStudioGD = TRUE)
library(ggplot2)

q <- ggplot(data.frame(x = c(-15, 25)), 
            aes(x = x)) +
  stat_function(fun = dnorm, 
                args = list(mean=5, sd = 10)) +
  labs(title = "Normal Distribution using `stat_function`")
q

# Shade the function so that you see the range between -5 and 10
shade <- function(x) {
  y <- dnorm(x, mean = 5, sd = 10)
  y[x < -5 | x > 10.00] <- NA
  y
}
# Add to plot and print
q <- q + stat_function(fun=shade, geom="area", fill="green3", alpha=0.5)
print(q)
```

> Output  
<img src = "Images/ggplotRDensity.png" width = 350> <br>

---
<br>

### `base` R Method
* Normal distribution and shade between x values of -5 and 10

```R
# Base R
# Learn how to use polygon()
dev.new(noRStudioGD = TRUE)
curve(dnorm(x, mean= 5 , sd =10), xlim = c(-15,25))
xcurve = seq(-5,10, length = 1000)
ycurve = dnorm(xcurve, mean = 5, sd =10)
polygon(x = c(-5,xcurve,10), y = c(0,ycurve,0), col = "green3")
```

> Output  
<img src = "Images/baseRDensity.png" width = 350> <br>

---
<br>

## 4. Calculating Probabilities

### Probability Density
> Calculates the density of the bin (or interval)

Term                  | Description
----------------------|-------------
Probability `Mass`    |  Probability of falling in a bin (*interval*) on  a continuous scale
Probability `Density` | Ratio of probability mass to the bin (interval)width. *"Stuff per unit of space it takes up"*

<img src = "Images/probDensity.png" width = 550> <br>

### Normal PDF (Probability Density Function)
> Estimation of probability density

<img src = "Images/pdf.png" width = 550> <br>

### Highest Density Interval (HDI)

<img src = "Images/hdi.png" width = 550> <br>
<img src = "Images/hdiExamples.png" width = 550> <br>



### What is the probability `x > 12`, given mean and standard deviation
```R
# Calculating probabilities
# Area under density is the probability
# 
# X ~ N(10,15), want P(X>12)
dev.new(noRStudioGD = TRUE)

# Curves, given mean and standard deviation
curve(dnorm(x, mean= 10 , sd =15), xlim = c(-20,50))
xcurve = seq(12,50, length = 1000)
ycurve = dnorm(xcurve, mean = 10, sd =15)

# Plot area greater than 12
polygon(x = c(12,xcurve,50), y = c(0,ycurve,0), col = "green3")
area = 1- pnorm(12,10,15) # 1-lower tail
area = round(area,4) # round

# Make a tooltip that displays the area on click
text(locator(1), paste("Area=", area)) #locator waits for a left click
```

> Output  
<img src = "Images/probGreater.png" width = 350> <br>

---
<br>

### What is the probabilityof x `between 12 and 22`, given mean and standard deviation
```R
# X ~ N(10,15), want P(12 <X<=22)
dev.new(noRStudioGD = TRUE)

# Curves, given mean and standard deviation
curve(dnorm(x, mean= 10 , sd =15), xlim = c(-20,50))
xcurve = seq(12,22, length = 1000)
ycurve = dnorm(xcurve, mean = 10, sd =15)

# Plot area between 12 and 22
polygon(x = c(12,xcurve,22), y = c(0,ycurve,0), col = "green3")
area = pnorm(22,10,15)-pnorm(12,10,15) # 1-lower tail
area = round(area,4) # round

# Make a tooltip that displays the area on click
text(locator(1), paste("Area=", area)) #locator waits for a left click
```

> Output  
<img src = "Images/probBetween.png" width = 350> <br>

---
<br>

## 5 - Conditional Probability
> Conditional probability of something, given known data
```
Conditional Probability of X given Y = p(x | y)
```

### Example of Conditional Probability (1)

<img src = "Images/condProb.png" width = 600> <br>

* ***Q***: p(`Blond Hair` |` Blue Eyes`)
* ***Reads as*** what is the conditional probability that someone has `blonde hair` given them having `blue eyes`
  ```
  Answer - Conditional Probability that someone has Blonde Hair, given them having blue eyes:
  0.16 / 0.36 = 0.45, or 45%
  ```

### Example of Conditional Probability (2)
* ***Q1***: p(`Red Hair` |` Hazel Eyes`)
* ***Reads as*** what is the conditional probability that someone has `red hair` given them having `hazel eyes`
  ```
  Answer - Conditional Probability that someone has Red Hair, given them having hazel eyes:
  0.02/ 0.16 = 0.125, or 12.5%
  ```

* ***Q2***: Are these two probabilies independant of each other?
* Test: multiply `marginal probabilities` and compare against `joint probability`. 
  * If they are `equal`, they are `independant`
  * If they are `not`, they are `dependant`
  ```
  Answer: 
  Marginal Red * Marginal Hazel = 0.12 * 0.16 = 0.0192
  Joint Red and Hazel = 0.200
  
  Since the do NOT equal, they are dependant
  ```