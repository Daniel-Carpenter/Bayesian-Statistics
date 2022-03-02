
# TASK 1 =======================================================================

  # 1 (i-ii)
  # Make a coin-die bbox
  # k = number of faces in the event set E for acceptance of proposal
  # Lik = likelihood for 2 states of theta
  # theta = two states
  # h1 = small relative to h2
  cdbbox<-function(k=1,lik,theta, h1="s")
  { 
    # K=1...6
    # xtable is a library which has functions useful for latex output
    library(xtable)
    
    # rename the first and second components of the likelihood
    lik1<-lik[1]
    lik2<-lik[2]
    
    # We will now make a prior that has the desired characteristics
    # See if you can prove the following
    # if h1 small "s" then ... else ...
    ifelse(h1=="s",
           pi1<-k/6*lik2/(lik1+k/6*lik2), 
           pi1<-lik2/(lik2+k/6*lik1)
    )
    
    # sum of probs is 1
    prior=c(pi1,1-pi1)
    
    #lik<-c(lik1,lik2)
    h<-prior*lik
    
    # Bayes
    post=h/sum(h)
    
    # Make a matrix for the Bayes box
    mat<-cbind(theta,prior,lik,h,post)
    rownames(mat)<-1:length(lik)
    Totals=c(NA,sum(prior),NA,sum(h),sum(post))
    mat2=rbind(mat,Totals)
    
    # Now make some plots useful in explaining the procedure
    graphics.off()
    dev.new(noRStudioGD = TRUE)
    layout(matrix(c(1,2),nr=1,nc=2,byrow=TRUE))
    barplot(matrix(c(0.5,0.5),nc=2,nr=1,byrow=TRUE,dimnames=list(c("Coin"),theta)),ylim=c(0,1),las=1,main="Proposal\n Uniform")
    barplot(matrix(h,nc=2,nr=1,byrow=TRUE,dimnames=list(c("Coin"),theta)),ylim=c(0,max(h)+0.5*max(h)),las=1,main="Proportional to target\n h")
    # Return a list of useful objects
    return(list(bbox=mat2,latex=xtable(mat2,digits=6),mat=mat,h=h,h1=h1,k=k))
  }
  
  cdbbox(k=2,lik=dbinom(x=4,size=10,prob=c(0.4,0.8)),theta=c(0.4,0.8),h1="s")->ans 
  ans$it
  ans
  ans$bbox
  
  
  # 1 (iii)
  # Accept it with a probability. Need an acceptance and rejection set. 
  # Anything lower than the probability can be in the acceptance set. For Example,
  # if 4/6, then can accept {1,2,3,4}
  
  
  # 1 b
  ## Impoved Graphic
  cdbbox2<-function(k=1,lik,theta, h1="s")
  { 
    # K=1...6
    # xtable is a library which has functions useful for latex output
    library(xtable)
    
    # rename the first and second components of the likelihood
    lik1<-lik[1]
    lik2<-lik[2]
    
    # We will now make a prior that has the desired characteristics
    # See if you can prove the following
    # if h1 small "s" then ... else ...
    ifelse(h1=="s",
           pi1 <- k/6 * lik2 / (lik1 + k/6 * lik2), 
           pi1 <-       lik2 / (lik2 + k/6 * lik1)
    )
    
    # sum of probs is 1
    prior=c(pi1,1-pi1)
    
    #lik<-c(lik1,lik2)
    h<-prior*lik
    
    # Bayes
    post=h/sum(h)
    
    # Make a matrix for the Bayes box
    mat<-cbind(theta,prior,lik,h,post)
    rownames(mat)<-1:length(lik)
    Totals=c(NA,sum(prior),NA,sum(h),sum(post))
    mat2=rbind(mat,Totals)
    
    # Now make some plots useful in explaining the procedure
    graphics.off()
    dev.new(noRStudioGD = TRUE)
    layout(matrix(c(1,2),nr=1,nc=2,byrow=TRUE))
    barplot(matrix(c(0.5,0.5),nc=2,nr=1,byrow=TRUE,dimnames=list(c("Coin"),theta)),ylim=c(0,1),las=1,main="Proposal Distribution\n (Uniform)",
            col = 'grey20',
            ylab = 'Probabilities',
            xlab = 'Outcome')
    barplot(matrix(h,nc=2,nr=1,byrow=TRUE,dimnames=list(c("Coin"),theta)),ylim=c(0,max(h)+0.5*max(h)),las=1,main="Proportional to target\n h",
            col = 'red',
            ylab = 'Probabilities',
            xlab = 'Outcome')
    # Return a list of useful objects
    return(list(bbox=mat2,latex=xtable(mat2,digits=6),mat=mat,h=h,h1=h1,k=k))
  }
  
  cdbbox2(k=2,lik=dbinom(x=4,size=10,prob=c(0.4,0.8)),theta=c(0.4,0.8),h1="s")->ans 
  

# TASK 2 =======================================================================
  
  # This function relies on getting h vectors that match E2
  # Use cdbbox() to get the correct h vectors.
  # Notice the ... - this corresponds to the ... in the barplot
  # You can use additional arguments which pass to the barplot
  coindie<-function(n=100, h=c(1/4,3/4),E2=c(5,6),init=1,...){
    library(xtable)
    dieset<-c()
    dieset[1]<-"E1"
    die<-function(n=1){
    sample(1:6,size=n,replace=TRUE)
    }
    
    coin<-function(n=1){
    sample(1:2,size=n,replace=TRUE)
    }
    face<-c()
    alpha<-c() # holds acceptance probs
    alpha[1]<-1
    post<-c()# post sample
    prop<-c() # vec of proposed states 1s and 2s
    prop[1]=init # initial state
    post[1]=prop[1]
    dice<-c()
    dice[1]<-die()
    
    for(i in 2:n){ # starts at 2 because initial value given above
    prop[i]<-coin()
    alpha[i]=min(1,h[prop[i]]/h[post[i-1]])
    
    dice[i]<-die()
    ifelse(alpha[i]==1,dieset[i]<-"E1",dieset[i]<-"E2")
    # is x an element of set y
    if(alpha[i]==1 | (is.element(dice[i],E2) & alpha[i]!=1)){post[i]<-prop[i]}
    else{post[i]<-post[i-1]}
     }  
    res<-matrix(c(prop,round(alpha,2),dieset,dice,post ),nc=5,nr=n,byrow=FALSE,dimnames=list(1:n,c("proposal","alpha", "E","dice","post")))
    sim<-table(post)/n
    postexact<-h/sum(h)
    dev.new(noRStudioGD = TRUE)
    barplot(sim,...)
    return(list(iter=res,sim=sim,postexact=postexact,post=post,xtable=xtable(res,dig=1)) )
  }
  
  coindie(n=20,h=c(0.6,0.4),E2=c(3,4,5,6)) ->ans
  ans$it
  
  coindie(n=20,h=c(1,6),E2=c(3)) ->ans
  ans$it
  
  # Use cdbbox in conjunction with coindie()
  cdbbox(k=3,lik=dbinom(x=6,size=10,prob=c(0.5,0.8)),theta=c(0.5,0.8),h1="s")->ans 
  names(ans)
  ans2=coindie(n=3000,h=ans$h,E2=1:ans$k)
  ans2$it
  
  
  # retrieving posterior theta values
  #trace plot
  
  ans3=coindie(n=1000,h=ans$h,E2=1:ans$k)
  ans4=c(0.5,0.8)[ans3$post]
  plot(ans4,type="l",main="Trace plot",xlab="Iteration",ylab="theta")

  
  
  
  # TASK 3 ======================================================================

  # This function makes discrete simulations from a posterior with any number of h values
  # n=nu of iterations
  # You can embellish this function
  simR<-function(n=10000, h=c(0.03344302,0.06165627),...){
    alpha<-c() # holds transition probs
    alpha[1]<-1
    u<-c() # holds uniform values
    u[1]<-1
    post<-c()# post sample
    prop<-c() # vec of proposed states 1s and 2s
    prop[1]=1 # initial state
    post[1]=prop[1]
    for(i in 2:n){ # starts at 2 because initial value given above
      # proposal state 
      prop[i]=sample(1:length(h),1,replace=TRUE)
      # calculate alpha
      # notice h[prop[i]] gives the actual value of h
      alpha[i]=min(1,h[prop[i]]/h[post[i-1]])
      # to calculate accepting proposal with prob alpha
      # select a random value from  a uniform (0,1)
      u[i]=runif(1)
      if(u[i]<=alpha[i]){post[i]<-prop[i]}
      else{post[i]<-post[i-1]}
    }
    res<-matrix(c(prop,u,alpha,post ),nc=4,nr=n,byrow=FALSE)
    sim<-table(post)/n
    # windows only works with a pc
    # quartz with macs
    # dev.new(noRStudioGD = TRUE) # or quartz() 
    
    barplot(sim,...)
    postexact<-h/sum(h)
    # The returned output is a list 
    # Use obj$ to obtain whatever interests you
    return(list(iter=res,sim=sim,postexact=postexact,post=post) )
  }

  
  # Uniform Binomial Experiment
  getH <- function(numThetaValues, x, n) {
    
    ## Form uniform probability
    theta <- seq(0, 1, length = numThetaValues)
    
    ## Calculate prior assuming uniform distribution
    prior = rep(1/numThetaValues, numThetaValues)
    
    ## Calculate the likelihood
    likelihood  = dbinom(x=x, size=n, prob=theta)
    
    ## Calculate the Prior x the Likelihood
    h <-  prior * likelihood
    
    return(h)
  }
  
  numThetaValues = 40
  x = 4
  n = 10
  
  
  simROut <- simR(n=10000, h=getH(numThetaValues, x, n),
                  
                  # Title
                  main = paste("Histogram of Simulation Output using simR()\n",
                               "Assumes Uniform Prior dist. over ", numThetaValues, 
                               " values of theta. Prob. ", x, 
                               " with ", n, "bernoulli trials"),
                  ylab = "Probability",
                  xlab = 'Number of Theta Values',
                  col = 'lightblue'
                  )

  
  
  # Task 4 =====================================================================
  
  
  # What about different proposal distributions
  # Again for the discrete case
  simRQ<-function(n=1000,init=1, h=c(1,1),pr=c(1,1)/2,...){
    alpha<-c() # holds transition probs
    alpha[1]<-1
    u<-c() # holds uniform values
    u[1]<-1
    post<-c()# post sample
    prop<-c() # vec of proposed states 1s and 2s etc
    prop[1]=init # initial state
    post[1]=prop[1]
    q<-function(x){pr[x]}
    for(i in 2:n){ # starts at 2 because initial value given above
      #make a sample from the proposal
      sample(1:length(h),1,replace=TRUE,prob=pr)->prop[i]
      #Calculate alpha adjusting for the proposal being non uniform
      alpha[i]=min(1,h[prop[i]]*q(post[i-1])/(h[post[i-1]]*q(prop[i])))
      # now choose the proposal with probability alpha
      u[i]=runif(1)
      if(u[i]<=alpha[i]){post[i]<-prop[i]}
      else{post[i]=post[i-1]}
    }
    res<-matrix(c(prop,u,alpha,post ),nc=4,nr=n,byrow=FALSE,dimnames=list(1:n,c("prop","u","alpha","post")))
    sim<-table(post)/n
    postexact<-h/sum(h)
    dev.new(noRStudioGD = TRUE)
    barplot(sim,...)
    tmp<-c()
    ifelse(length(res[,1])>=20,tmp<-res[1:20,],tmp<-res)
    return(list(iter=tmp,sim=sim,post=postexact) )
  }
  
  # Create a funciton to establish a Uniform Binomial Experiment
  getH <- function(numThetaValues, x, n) {
    
    ## Form uniform probability
    theta <- seq(0, 1, length = numThetaValues)
    
    ## Calculate prior assuming uniform distribution
    prior = rep(1/numThetaValues, numThetaValues)
    
    ## Calculate the likelihood
    likelihood  = dbinom(x=x, size=n, prob=theta)
    
    ## Calculate the Prior x the Likelihood
    h <-  prior * likelihood
    
    return(h)
  }
  
  
  # Inputs for getH()
  numThetaValues = 11
  x = 4
  n = 10
  
  # Function to create a peak distribution - only for odd dist
  makePeak <- function(numThetaValues) {
    peak = round(numThetaValues / 2)  # Peak of distribution
    beg  = 1:(peak-1)                 # up until the peak
    end  = (peak-1):1                 # After the peak
    return(c(beg, peak, end))         # Combine it to make mountain
  }
    
  prInput = makePeak(numThetaValues)  # proposal input that is a peak distribution (Q)
  hInput = getH(numThetaValues, x, n) # Uniform prior
  
  simRQ(n=10000,h=hInput,pr=prInput,
        
        # Title
        main = paste("Histogram of Simulation Output using simRQ()\n",
                     "Assumes Uniform Prior dist. over ", numThetaValues, 
                     " values of theta. Prob. ", x, 
                     " with ", n, "bernoulli trials. Uses Peak Proposal Dist."),
        ylab = "Frequency",
        xlab = "Proposed h's",
        col = 'tomato3'
  )
  
  
# TASK 5 =======================================================================
  
  ## Now we shall look at the continuous case
  
  # Task 5
  ### Using a beta proposal
  ### You can change the proposal to whatever you require
  ## a,b are the parameters of the Beta proposal
  ## a=b=1 is a uniform
  simRC<-function(n=10,init=0.5, h=function(theta){dunif(theta)*dbinom(x=4,size=10,prob=theta)},a=3,b=4){
    #dbeta(x, shape1, shape2, ncp = 0, log = FALSE)
    alpha<-c() # holds transition probs
    alpha[1]<-1
    u<-c() # holds uniform values
    u[1]<-1
    post<-c()# post sample
    prop<-c() # vec of proposed states 1s and 2s
    prop[1]=init # initial state
    post[1]=prop[1]
    q=function(x){dbeta(x,a,b)}
    for(i in 2:n){ # starts at 2 because initial value given above
      rbeta(1,a,b)->prop[i]
      
      alpha[i]=min(1, h[prop[i]]*q(post[i-1])/(h[post[i-1]]*q(prop[i])))
      u[i]=runif(1)
      ifelse(u[i]<=alpha[i],post[i]<-prop[i],post[i]<-post[i-1])
    }
    res<-matrix(c(prop,u,alpha,post ),nc=4,nr=n,byrow=FALSE,dimnames=list(1:n,c("prop","u","alpha","post")))
    windows()
    hist(post,freq=FALSE)
    
    return(list(matrix=res,summary=summary(post)) )
  }
  
  # Need to alter to make the graphs
  
  # The above should answer the second part o the q
  
  simRC(n=10000)
  