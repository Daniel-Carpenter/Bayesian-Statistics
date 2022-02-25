# Unit 5 Ch 6

# Plot Beta
dev.new(noRStudioGD = TRUE)
alpha = 2
beta = 2
curve(dbeta(x, alpha, beta), xlim = c(0,1), 
      main = paste("alpha =", alpha,",", "beta=", beta),
      ylab = "Density", lwd =2)


# make a window and cut it with layout
# then populate with a for loop

# 9 plots
graphics.off()
dev.new(noRStudioGD = TRUE)

n=5
layout(matrix(1:((n-1)*(n-1)), nr =(n-1), nc = (n-1), byrow = TRUE))
alpha = 2:n
beta = 2:n

for(i in 1:(n-1))
{
  for(j in 1:(n-1))
  {
    curve(dbeta(x, alpha[i], beta[j]), xlim = c(0,1), 
          main = paste("alpha =", alpha[i],",", "beta=", beta[j]),
          ylab = "Density", lwd =2) 
    
  }
  
}


# Make mixture betas

mixbeta = function(x, r, alpha1,beta1, alpha2, beta2) # r of beta density 1
{
  r*dbeta(x, alpha1, beta1) +(1-r)*dbeta(x, alpha2,beta2)
  
}

dev.new(noRStudioGD = TRUE)
curve(mixbeta(x, 0.5, 10,2,3,50))

curve(mixbeta(x, 0.1, 4,40,2,2))

curve(mixbeta(x, 0.9, 4,40,2,2))


# Make kappa and mu substitutions
# alpha = mu*k, beta = (1-mu)*k

mu = 0.4
k = 100

alpha = mu*k
beta = (1-mu)*k


dev.new(noRStudioGD = TRUE)

curve(dbeta(x, alpha, beta), xlim = c(0,1), 
      main = paste("alpha =", alpha,",", "beta=", beta, ",", "mu =", mu, ",", "k=",k),
      ylab = "Density", lwd =2)



