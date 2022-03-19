# Random walk
# 
myrwalk = function(x0=5, iter = 1000, p=0.5) 
{
  if(!require(ggplot2)) {install.packages(ggplot2)}
  graphics.off()
  x = c()
  x[1] = x0
  mat = matrix(sample(c(-1,1),size = iter, prob = c(p,1-p), replace = TRUE), nr = iter, nc=1, byrow = TRUE)
  for ( i in 2:iter)
  {
   x[i] = x[i-1] + mat[i,1]
    
  }
  x
  df = data.frame(index = 1:iter, x = x)
  
  dev.new(noRStudioGD = TRUE)
  
 plot(1:iter, x, type = "s")
 
 dev.new(noRStudioGD = TRUE)
 
 g = ggplot(df, aes(x = index, y = x )) + geom_line()
 
 print(g)
 write.csv(df, "df.csv", row.names=FALSE)
 list(x=x, x0 = x0, mat = mat, mean = mean(x))
}
obj = myrwalk(x0=0,iter =1000, p=0.5)



#######################################
#######################################
#######################################

myrwalk2 = function(x0=0, y0=0, iter = 1000, px=0.5, py=0.5) 
{
  if(!require(ggplot2)) {install.packages(ggplot2)}
  graphics.off()
  x = c()
  y = c()
  x[1] = x0
  y[1] = y0
  
  xr = sample(c(-1,1),size = iter, prob = c(px,1-px), replace = TRUE)
  yr = sample(c(-1,1),size = iter, prob = c(py,1-py), replace = TRUE)
  
  mat = matrix(c(xr,yr), nr = iter, nc=2, byrow = TRUE)
  for ( i in 2:iter)
  {
    x[i] = x[i-1] + mat[i,1]
    y[i] = y[i-1] + mat[i,2]
  }
  x
  df = data.frame( x = x, y = y)
  
  dev.new(noRStudioGD = TRUE)
  
  plot( x, y,type = "s", main = "Random walk", las =1)
  text(x0,y0, "Start")
  text(x[iter], y[iter], "End")
  
  dev.new(noRStudioGD = TRUE)
  
  g = ggplot(df, aes(x = x, y = y )) + geom_point(alpha=0.3, color = "Blue")
  g = g + geom_text(aes(x = x0,y = y0, label = "Start"), color = "Green") + geom_text(aes(x = x[iter],y = y[iter], label = "End"), color = "Red")
  g = g + ggtitle("Random walk")
  print(g)
  write.csv(df, "df.csv", row.names=FALSE)
  list(df = df, x0 = x0, y0=y0, mat = mat)
}
obj = myrwalk2(iter =10000)


###########################################
###########################################
###########################################
###########################################
###########################################
# Random walk Island problem FROM JK
# 
myrwmcmc = function( x0=5,iter = 1000,   h = 1:9, p = 0.5) 
{
  graphics.off()
  library(ggplot2)
  hmin = min(h)
  hmax = max(h)
  
  x = c()
  x[1] = x0
  for( i in 2:iter)
  {
    xprop = sample(c(-1,1), size = 1, prob = c(p,1-p), replace = TRUE)
    x[i] = x[i-1] +xprop
    if(x[i]< hmin | x[i]> hmax) {alpha <- 0} else{ alpha <- min(1, h[x[i]]/h[x[i-1]])}
    if(runif(1,0,1)<= alpha ) 
      {
      x[i] <- x[i]
    }
    else
      {
      x[i]<-x[i-1]
    }
  }
  windows()
  barplot(table(factor(x)),col = rainbow(length(h)))
  windows()
  plot(x,1:iter, type = "l")
  df = data.frame(x = x, y = 1:iter  )
  
  list(x=x)
}

myrwmcmc(iter = 100000, h = c(1:20))


## more general MCMC random walk
## must be discrete values and  xo in the domain of xd

myrwmcmcg = function( xd = 1:10, x0=5, iter = 1000, h = function(x) x, p = 0.5) 
{
  graphics.off()
  library(ggplot2)
  

  
  dmin = min(xd)
  dmax = max(xd)
  delta = dmax - xd[length(xd)-1]
  
  x = c()
  x[1] = x0
  for( i in 2:iter)
  {
    xprop = sample(c(-delta,delta), size = 1, prob = c(p,1-p), replace = TRUE)
    x[i] = x[i-1] +xprop
    if(x[i]< dmin | x[i]> dmax) {alpha <- 0} else{ alpha <- min(1, h(x[i])/h(x[i-1]))}
    if(runif(1,0,1)<= alpha ) 
    {
      x[i] <- x[i]
    }
    else
    {
      x[i]<-x[i-1]
    }
  }
  windows()
  barplot(table(factor(x, levels = xd))/iter,col = rainbow(length(xd)),las = 1)
  
  windows()
  plot(x,1:iter, type = "l")
  df = data.frame(x = x, y = 1:iter  )
  windows()
  g = ggplot(df, aes(x=x, fill = I("Blue"))) + geom_bar(color = I("Red"))
  print(g)
  list(x=x)
}

obj = myrwmcmcg(xd = seq(0,1,by = 0.1),iter = 1000,x0=0.5,h = function(x) dbeta(x,3,1)*dbinom(4,size=10,prob=x)) 
hist(obj$x, breaks = seq(0,1,by = 0.1),freq=FALSE )


## Bayes box

bbox=function(theta,prior,lik){  # Three vectors
  
  post=prior*lik/sum(prior*lik) # post by discrete Bayes
  
  # tmp (temp) contains the matrix which will be the Bbox
  tmp=matrix(c(theta,prior,lik,prior*lik,post),nr=length(prior),nc=5,byrow=FALSE)
  colnames(tmp)=c("theta","prior","lik","prior*lik","post")
  lr=c(NA,sum(prior),NA ,sum(prior*lik),sum(post)) # a last row of NA's and sums
  tmp=rbind(tmp,lr) # bind it to the existing matrix
  # Make a window for graphics
  if(.Platform$OS.type == "windows") { 
    windows()
  } else {
    quartz()
  }
  
  # plot prior posyt and lik
  plot(theta,post,type="b",lwd=2,pch=19,main="Bayes box",ylim=c(0,max(prior,lik,post)),col="Red")
  lines(theta,lik,type="b",lwd=2,col="Blue")
  lines(theta,prior,type="b",lwd=2,col="black")
  legend("topleft",legend=c("Prior","Posterior","Likelihood"),fill=c("black","red","blue"))
  
  # the last tmp will release the matrix to the command line.
  tmp
}
### End of function

theta=seq(0,1,length=10)
theta
prior=rep(1,10)/10
#prior=c(0,rep(1,8),0)/8
#prior=c(0,rep(1,6),0,0,0)/6
prior
lik=dbinom(x=3,prob=theta,size=10)  # binomial likelihood
bbox(theta,prior,lik)    # invoke the function 

# This function does a little more -- ignore for now 
# You could run it and see what it does (extra for experts)

mybboxp = function(n,x,theta,prior) # incomplete
{
  graphics.off()
  lik = dbinom(x=x,size=n,prob=theta) #f(x|theta)
  h = prior * lik
  post = h/sum(h) # Bayes rule
  hh = post * lik
  tab = matrix(c(theta, prior, lik, h, post, hh), nc=6, byrow = FALSE) # by column
  colnames(tab) = c("theta", "prior", "lik", "h","post", "hh")
  tabf = rbind(tab, c(NA,sum(prior),NA, sum(h),sum(post), sum(hh)))
  
  tabr = round(tabf,4) # tabr = rounded table 
  
  tabdf = as.data.frame(tab)
  
  dev.new(noRStudioGD = TRUE)
  plot(post ~ theta, type = "b", 
       xlab = expression(theta), 
       ylab = "Probability", 
       ylim = c(0, max(c(post, prior, lik))),
       col = "Blue", lwd=3,
       data = tabdf)
  lines( prior ~ theta , data = tabdf, type = "b", col = "Red", lwd = 3)
  lines (lik ~ theta, data = tabdf, type = "b", col = "Green", lwd = 3)
  legtxt = c("prior", "post", "lik",paste("prior pred. = ", round(sum(h),4)), 
             paste( "post pred.= ", round(sum(hh),4)))
  legend("topright", legend = legtxt, 
         col = c("Red", "Blue", "Green", NA, NA), 
         lwd = c(3,3,3,3,3))
  title("Prior, Likelihood and Posterior Compared")
  #text(locator(1), paste("Prior predictive is ", round(sum(h),4)))
  list(tabr = tabr, tabdf = tabdf, priorpred = sum(h), postpred = sum(hh)) # release to commandline
}

obj = mybboxp(n=10,x=4, theta = seq(0,1,by=0.1), prior = rep(1,11)/11)

obj = mybbox(n=10,x=4, theta = seq(0,1,length=100), prior = rep(1,100)/100)
obj





