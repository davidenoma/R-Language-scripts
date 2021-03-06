library(devtools)
library(Biobase)
library(UsingR)

data(galton)
par(mfrow=c(1,2))

hist(galton$child,col="blue",breaks=100)
hist(galton$parent,col="blue",breaks=100)


meanChild <- mean(galton$child)
lines(rep(meanChild,100),seq(0,150,length=100),col="red",lwd=5)

#Plot child versus parent 
par(mfrow = c(1,2))

plot(galton$parent,galton$child,pch=19,col="blue")

plot(galton$parent,galton$child,pch=19,col="blue")
near65 <- galton[abs(galton$parent - 65)<1, ]
points(near65$parent,near65$child,pch=19,col="red")
lines(seq(64,66,length=100),rep(mean(near65$child),100),col="red",lwd=4)

#Fitting a line 
plot(galton$parent,galton$child,pch=19,col="blue")
lm1 <- lm(galton$child ~ galton$parent)

lines(galton$parent,lm1$fitted,col="red",lwd=3)

##Not all points are on the line 
plot(galton$parent,galton$child,pch=19,col="blue")
lines(galton$parent,lm1$fitted,col="red",lwd=3)

#Plotting what is left over 
par(mfrow=c(1,2))
plot(galton$parent,galton$child,pch=19,col="blue")
lines(galton$parent,lm1$fitted,col="red",lwd=3)
plot(galton$parent,lm1$residuals,col="blue",pch=19)
abline(c(0,0),col="red",lwd=3)