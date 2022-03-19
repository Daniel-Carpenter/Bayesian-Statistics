# Chapter 9 
# Grid approximation 
# Bivariate parameter vector
graphics.off()

f = function( theta,w, k, Data, Aw = 1, Bw = 1)
{
  a = w*(k-2)+1
  b = (1-w)*(k-2) + 1
  dbinom(x = sum(Data),size = length(Data), prob = theta)*dbeta(theta, a,b)*dbeta(w,Aw,Bw)
}

# Make vectors for possible theta and w parameter values

theta = seq(0,1, length =100)
w =theta

# Explain the grid approximation
windows()
plot(w ~ theta, type = "n")
abline(v = theta, h = w)

# make f(theta, w) values we will call "z" values
z = outer(theta,w, f, k=100,Data=rep(c(1,0),c(6,4) ))
z = z/sum(z)

zr = round(z,2)
# add the z to the grid
n = length(w)
for(j in 1:n)
{
for(i in 1:n)
{
text(theta[i], w[j],zr[i,j] )
}
}
# persp plot
windows()
persp(theta,w,zr)

# cotour plot
windows()
contour(theta,w,zr)


################################### 2 coins -  from the same mint ####

Data = matrix(c(rep(c(1,0), c(4,6)), rep(c(1,0), c(6,4))), nr = 10,nc=2, byrow =FALSE)
colnames(Data) = c("coin1", "coin2")
Data
g = function(theta1,theta2,w, k, Data, Aw = 1, Bw = 1)
{
  a = w*(k-2)+1
  b = (1-w)*(k-2) + 1
  dbinom(x = sum(Data[,1]),size = length(Data[,1]), prob = theta1)*
    dbinom(x = sum(Data[,2]),size = length(Data[,2]), prob = theta2)*
    dbeta(theta1, a,b)*dbeta(theta2,a,b)*dbeta(w,Aw,Bw)
}


