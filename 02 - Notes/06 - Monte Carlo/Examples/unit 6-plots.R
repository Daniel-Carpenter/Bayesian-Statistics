# Chapter 7 MCMC

# plotting a distribution

windows()
curve(dnorm(x,mean = 10,sd=5), col = "Red", lwd =3, xlim = c(10-4*5, 10+4*5))
curve(dgamma(x, shape = 4,scale = 2), xlim = c(0,20))


# random samples and histograms

x = rnorm(1000, mean = 10, sd = 5)
hist(x, col = "Red")

df = as.data.frame(x=x)
library(ggplot2)

g = ggplot(df, aes(x, fill = I("Red"))) +  geom_histogram(bins = 20)
g


# kernal density

g = ggplot(df, aes(x, fill = I("Red")))  + geom_density(aes(x=x,color = I("Black") ))
g



library(s20x)
x=1:30
y = 4 + 8*x +rnorm(30,0,10)
y.lm = lm(y ~x)
ciReg(y.lm)
coef(y.lm)
summary(y.lm)
