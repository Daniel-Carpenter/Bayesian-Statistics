#Lab 4 -- Bayes' box


theta = seq(0,1,length = 30)
prior = rep(1/30, 30)
lik  = dbinom(x=4, size = 10, prob = theta)
h = prior*lik
post = h/sum(h)

mat.bayes = matrix(c(theta, prior, lik, h, post), nr = 30, nc =5, byrow = FALSE)

colnames(mat.bayes) = c("theta", "prior", "lik", "h", "post")


colSums(mat.bayes)

# rbind is a useful function

rownames(mat.bayes)= c(rep("", 30))

mat.bayes

df = as.data.frame(mat.bayes)

if(!require(xtable)) install.packages(xtable)
xtable(df)

#BCI

alpha=0.05
cp=cumsum(post) # cumulative sum
cp # look at the output
L= max(which(cp<alpha/2)) # this gives the max index where  cp < alpha/2
U = min(which(cp > 1-alpha/2))

BCI = df$theta[c(L,U)] # close to the desired BCI
BCI

# This would be used on a random sample from the posterior
alpha=0.05
# theta a posterior random sample example theta <- rbeta(1000, 2,3)
BCI <- quantile(theta, c(alpha/2, 1-alpha/2))
BCI


# Bayesian point estimate is the posterior mean
sum(df$post*...)

if(!require(ggplot2)) install.packages(ggplot2)
dev.new(noRStudioGD = TRUE)
g = ggplot(df, aes(x = theta)) + geom_point(aes(y = prior), col = "red", size = 3) +
  ... + geom_point(aes(y=post), col = "green")
g + ggtitle("Wayne's Plot") + labs() ...
