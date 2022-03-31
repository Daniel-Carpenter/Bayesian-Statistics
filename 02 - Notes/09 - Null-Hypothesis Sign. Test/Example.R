# ch11
# compare two sample t-tests with Bayesian equivalent
# 

mycomp = function(mu1=15,mu2=20,sd1 = 5, sd2 =10, n1=10,n2=10,iter = 10, alpha = 0.05)
{
  if(!require(BEST)) install.packages(BEST) # check for BEST package
  data1 = rnorm(iter*n1,mean = mu1, sd = sd1)
  data2 = rnorm(iter*n2, mean = mu2, sd = sd2)
  
  mat1 = matrix(data1, nr = n1, nc = iter, byrow = TRUE)
  mat2 = matrix(data2, nr = n2, nc = iter, byrow = TRUE)
  
  matout = matrix(NA, nr=iter, nc = 4, dimnames=list(1:iter,c("BL","BU", "CL","CU")))
  
  for (i in 1:iter)
  {
    bo=BESTmcmc(mat1[,i],mat2[,i])
   
   muDiff <- bo$mu1 - bo$mu2
   matout[i,1]<- quantile(muDiff,alpha/2)
   matout[i,2]<- quantile(muDiff,1-alpha/2)
   tto=t.test(mat1[,i],mat2[,i],var.equal = FALSE) 
   matout[i,3] = tto$conf.int[1]
   matout[i,4] = tto$conf.int[2]
   cat(paste(iter-i, "iterations to go"))
  }
  dev.new(noRStudioGD = TRUE)
  matplot(matout, type ="b")
  segments(1:iter,matout[,1], 1:iter,matout[,2], col = 1:iter)
  segments(1:iter + 0.1,matout[,3], 1:iter + 0.1, matout[,4])
  write.csv(matout, file = "matout.csv", row.names = FALSE)
  list(matout = matout)
}

mycomp(iter = 3)


