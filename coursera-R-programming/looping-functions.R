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
rowMeans()
colSums()
colMeans()

