# Exercises chapter 2

# 2.1
# model a

ma = function(x) rep(1/4, length(x))

# model b

mb = function(x) x/10

# model c

mc = function(x) 12/(25*x)

ma(1:4)
mb(1:4)
mc(1:4)
 


x=1:4
ya=ma(x)


plot(ya~x, type = "h", lwd = 10, main = "Model A")

yb = mb(x)

plot(yb~x, type = "h", lwd = 10, main = "Model B")

yc = mc(x)

plot(yc~x, type = "h", lwd = 10, main = "Model C")

sum(ya)
sum(yb)
sum(yc)

# Using GGPLOT
# 

dfa = data.frame(x =x, ya = ya)
dfb = data.frame(x=x,yb = yb)
dfc = data.frame(x=x, yc = yc)

library(ggplot2)
ga = ggplot(dfa, aes(x = x, y = ya, col=I("red"))) + geom_line(lwd = 10)
gb = ggplot(dfb, aes(x = x, y = yb, col = I("blue") )) + geom_line(lwd = 10)
gc = ggplot(dfc, aes(x=x,y=yc, col = I("green"))) + geom_line(lwd = 10)
 
ga
gb
gc

# Ex2.2
# 

# What would be expected under 100 rolls of such a die?
100*ya
100*yb
100*yc
