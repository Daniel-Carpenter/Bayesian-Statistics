# Task 1

# k = number of faces in the event set E for acceptance of proposal (we have set of 2); K=1...6
# lik = likelihood for 2 states
# theta = two states = 0.4 and 0.8
# h1 = small relative to h2
cdbbox<-function(k=1,lik,theta, h1="s")
{
  # rename the first and second components of the likelihood
  lik1<-lik[1]
  lik2<-lik[2]
  # We will now make a prior that has the desired characteristics
  # if h1 small "s" then ... else ...
  ifelse(h1=="s",pi1<-k/6*lik2/(lik1+k/6*lik2), pi1<-lik2/(lik2+k/6*lik1))
  # sum of probs is 1
  prior=c(pi1,1-pi1)
  lik<-c(lik1,lik2)
  h<-prior*lik
  # Bayes
  post=h/sum(h)
}





# 1. Use the function coindie() to make a number of iterations.

  ## a. Use n=10,h=c(0.6,0.4),E2=c(2,3,4,5) to make some MCMC output.
    
    
  ## b. Paste the above simulation output here:
    
    
  ## c. Improve the graphics in some way and say what you did!
    
    


#   2. Use the output of cdbbox() as inputs to the coindie() function that you altered – use any
#      examples you wish – explain the input and output.