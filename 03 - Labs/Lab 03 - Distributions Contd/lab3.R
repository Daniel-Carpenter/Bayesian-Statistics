# Bayesian course
# Lab 3
# distributional theory + plots

# Probability calculations
# End points do not matter for continuous variables
#P(X>=3)=P(X>3)

# X ~ N(20,5) P(X>22)?

1-pnorm(22,10,5) # notice options are in order:  x,mean, sd

# P(X<17)

pnorm(17,10,5)

# Base plotting

# Decide on a suitable range for x
# mean +- 3sd
dev.new(noRStudioGD = TRUE) # DO NOT CLOSE THIS WINDOW
curve(dnorm(x,10,5), xlim=c(10-3*5,10+3*5), lwd =2, main = "Wayne's plot")

# Now add area 
# area is the probability (area is the integral)

# Say we need P(8<X<=20)

xcurve = seq(8,20, length=1000) # length is arbitrary 
ycurve = dnorm(xcurve, 10,5)
polygon(c(8,xcurve,20), c(0,ycurve,0), col="green3") # this adds the area

# Now calculate the area (probability)

area=pnorm(20,10,5) - pnorm(8,10,5)
area2=round(area,4)

# Now place this on the above plot

text(12,0.02,substitute(paste("Prob=",area2 ), list(area2 = area2)))

## We can make a function
# Notice this will only solve a problem where the mean can change
# What if you wanted to solve the problem when we have a differfent sigma, ...?

myfun = function(mu)
{
  dev.new(noRStudioGD = TRUE) # DO NOT CLOSE THIS WINDOW
  curve(dnorm(x,mu,5), xlim=c(mu-3*5,mu+3*5), lwd =2, main = "Wayne's plot")
  
  # Now add area 
  # area is the probability (area is the integral)
  
  # Say we need P(8<X<=20)
  
  xcurve = seq(8,20, length=1000) # length is arbitrary 
  ycurve = dnorm(xcurve, mu,5)
  polygon(c(8,xcurve,20), c(0,ycurve,0), col="green3") # this adds the area
  
  # Now calculate the area (probability)
  
  area=pnorm(20,mu,5) - pnorm(8,mu,5)
  area2=round(area,4)
  
  # Now place this on the above plot
  
  text(12,0.02,substitute(paste("Prob=",area2 ), list(area2 = area2)))
  
  list(mu = mu, prob = area2) # this is the last line and will be released to the command line
  
}

obj = myfun(mu = 13)

obj

# Say you want to get the probability from obj

obj$prob


# Plotting with ggplot
# Continuous variables 
if(!require(ggplot2)) install.packages("ggplot2")

g = ggplot(NULL, aes(c(-10,10))) + 
  geom_area(stat = "function", fun = dnorm, args = list(mean = 0, sd = 3),fill = "red", xlim = c(-10, 0)) 
 
g


