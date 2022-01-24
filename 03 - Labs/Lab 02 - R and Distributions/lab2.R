# Bayesian course
# Lab 2
# distributional theory


# Binomial
dev.new(noRStudioGD = TRUE)
x=0:10
plot(x,dbinom(x, size=10,prob=0.6))



x = rbinom(100, size = 10, prob=0.6)
y = rbinom(80, size = 10, prob=0.8)

df = data.frame(Bin =c(x,y), Lab=rep(c("x","y"),c(100,80)))
df


if(!require(ggplot2)) install.packages(ggplot2)

g = ggplot(df, aes(x = Lab, y = Bin, fill = Lab)) +
  geom_boxplot()
print(g)

e = ggplot(df, aes(x = Lab, y=Bin, fill = Lab)) + 
  geom_violin(aes(y = Bin)) +facet_wrap(~Lab)
print(e)
