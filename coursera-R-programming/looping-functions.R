x <- list(a=1:5,b=rnorm(10))
x
x$a
x#b
lapply(x, mean)
x#b
x[b]
x
x[[b]]
x['b']
x <- list(a=1:4,b= rnorm(10),c = rnorm(20,1), d = rnorm(100,5))
x
runif(1)


lapply(x, runif)
x <- list(a= matrix(1:4,2,2) , b=matrix(1:6,3,2))
x
lapply(x,function(elt) elt[,1])
x['a']
sapply(x,mean)
typeof(sapply(x, mean))
str(apply)
x <- matrix(rnorm(200),20,10)
x
apply(x,2,mean)
apply(x, 1, sum)
#This means that you preserve the rows collapse 
#the columns 

rowSums()
apply(x, 1, sum)
rowMeans()
apply(x, 1, mean)

colSums()
apply(x, 2, sum)
colMeans()
apply(x, 2, mean)

apply(x,1,quantile,probs = c(0.25,0.75))

a <- array(rnorm(2*2*10),c(2,2,10))
typeof(a)apply(a,c(1,2),mean)
rowMeans(a,dims = 2)
#mapply

x <- c(rnorm(10), runif(10),rnorm(10,1))
x
f <- gl()
f <- gl(3,10)
f
tapply(x, f, mean)tapply(x, f, mean,simplify = FALSE)
tapply(x, f, range)
str(split)

split(x,f)
lapply(split(x,f), mean)

library(datasets)
head(airquality)
s <- split(airquality,airquality$Month)
lapply(s, function(x) colMeans(x[,c("Ozone","Solar.R","Wind")]))
lapply(s, function(x) colMeans(x[,c("Ozone","Solar.R","Wind")],na.rm = T))

x <- rnorm(10)
x
f1 <- gl(2,5)
f1

f2 <- gl(5,2)
f1
f2
interaction(f1,f2)
str(split(x,list(f1,f2)))
str(split(x,list(f1,f2),drop=TRUE))
#Debugging tools- Diagnosing the problem
log(-1)
rm(ls()[14])
traceback()


debug(lm)
lm(y~x)

options(error=recover)
read.csv("ultimate")
options(error=recover)
read.csv("ultimate")





