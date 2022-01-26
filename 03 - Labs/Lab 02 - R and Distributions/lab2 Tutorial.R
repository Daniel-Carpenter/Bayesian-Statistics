# Bayesian course
# Label 2
# distributional theory


# Binomial
dev.new(noRStudioGD = TRUE) # Comment this out when putting it into RMD
x=0:10
plot(x = x, 
     
     # Density Binomial Distribition for a discrete variable or a vector of discrete vars
     # Since the prob = 0.6, then n * prob = 6 (which is the most probably)
     y = dbinom(x, size=10,prob=0.6))


# rbinom = random sample from the binomial

  ## 100 binomial experiments (n), which you throw a coin 10 times (size)
  x = rbinom(n = 100, size = 10, prob=0.6)
  y = rbinom(n = 80,  size = 10, prob=0.8)
  
  ## Create a data frame that combines x, then the y. Repeat x 100 times and y 80 times
  df = data.frame(Bin =c(x,y), Label=rep(c("x","y"),c(100,80)))
  df


if(!require(ggplot2)) install.packages(ggplot2)

g = ggplot(df, aes(x = Label, y = Bin, fill = Label)) +
  geom_boxplot()
print(g)

## Violin chart will ensure that you are not, for example, having a bi modal distribuition
e = ggplot(df, aes(x = Label, y=Bin, fill = Label)) + 
  geom_violin(aes(y = Bin)) +facet_wrap(~Label)
print(e)
