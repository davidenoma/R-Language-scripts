x <-  matrix(1:12,4,3)
y <- c(1.7, 'a')
y
y = c(TRUE, 2)
y
y = c( 'a', true)
y = c( 'a', TRUE)
y
m <- 1:10
dim(m)
dim(m) <- c(1,10)
m
dim(m) <- c(2,5)
m
1:3
10:12
cbind(1:3, 10:13)
cbind(1:3, 10:12)
rbind(1:3, 10:12)
matr = rbind(1:3, 10:12)
typeof(matr)
x <- factor(c("yes","no","no","yes"))
x
con <- url("https://www.smalljobsnaija.com",'r')
x <- readLines(con)
head(x)
#Logical and Numeric indices for subsetting 


x <- c('a','b','c','d')
x
x[1]
x[0]
x[4]
x[1:4]
u <- x > 'a'
u
x[u]
x <- list(foo = 1:4, bar = 0.6)
x
x[1]
x[2]
x[[1]]
typeof(x[[1]])
typeof(x[1])
x$bar
x['bar']
x[['bar']]
x$bar
typeof(x$bar)
typeof(x$foo)
x <- matrix(1:6, 2, 3)
x





x[1,2]
x[1]
x[1,]
x[,3]
x <- list(aardvark = 1:5)
x
x$a
x[['a']]
x[['a', exact = F]]
x <- c(1,2, NA, 4, NA, 45)
X
x

#REmoving NA values
bad <- is.na(x)
bad
x[bad]
x[!bad]
y <- c('a','b',NA,'d',NA,'f')
good <- complete.cases(x,y)
good
x[good]
y[good]

#Vectorized operations
x <- c(1,2,3,5)
y <- c(10,2,4,5)
x + y
x == 2

#Vectorized Matrix operations

##CONTROL STRUCTURES 


x <- c('a','b','c','d')
x
for(i in seq_along(x)){
  print(x[i])
}

x <- matrix(1:6, 2 , 3)
for (i in seq_len(nrow(x))){
  for(j in seq_len(ncol(x))){
    print(x[i,j])
  }
}

coin_toss <- function (z){
  
  
  while (z >= 3 && z <= 10 ){
    print(z)
    coin <- rbinom(1,1,0.5)
    
    if (coin == 1 ){
      z <- z + 1 
    } else {
      z <- z - 1
    }
  }
}
above10 <- function(x){
  use <- x > 10 
  x[use]
}
above <- function(x,n){
  use <- x > n
  x[use]
}

columnmean <- function (y, removeNA=T) {
  nc <- ncol(y)
  means <- numeric(nc)
  for(i in 1:nc){
    means[i] <- mean(y[,i],na.rm = removeNA)
  }
  means
}


f <- function(a,b){
  a^2
}

 y <- 10 
 f <- function(x){
   y <- 2 
   y^2 + g(x)
 }
 
 g <- function (x){
   x*y
 }
 ##Date and Time functions
 as.Date('2020-02-01')
x <- as.Date('2020-02-01')
x
x + 1
x + 100
x <- Sys.time()
p <- as.POSIXlt(x)
p
unclass(p)
p$sec
p$zone
p$min
p$hour

#Simulation and Profiling 
g <- gl(40,10)
g

#str function 
str(f)
summary(f)
g
f

str(g)
summary(g)
library(datasets)
str(airquality)
summary(airquality)
m <- matrix(rnorm(100),10,10)
str(m)
summary(m)
m[,1]
s <- split(airquality,airquality$Month)
s
s <- split(airquality,airquality$Month)
str(s)

#simulation 
#probability distribution functions 
#dnorm, pnorm, qnorm, rnorm
#default is mean 0 and sd 1 
x <- rnorm(10)
x <- rnorm(10, 20, 1)
x
#The seed is very important
set.seed(1)
rnorm(5)
rnorm(5)
set.seed(1)
rnorm(5)
plot(x)
hist(x)
rnorm(100)
hist(x)
x
x <- rnorm(100)
hist(x)
#Poisson distribution
rpois(10)
rpois(10,1)
rpois(10,20)
##Cummulative distribution
ppois(10,1)
ppois(4,2)

#Probability that x <= 4

set.seed(20)
x <- rnorm(100)
e <- rnorm(100,0,2)
y <- 0.5 + 2 * x + e
y
summary(y)

plot(x,y)

#if x is a binary random variable 
set.seed(10)
x <- rbinom(100,1,0.5)
x
e <- rnorm(100,0,2)
y <- 0.5 + 2 * x + e 
summary(y)
plot(x,y)

#Generalized linear model with a poisson distribution 

set.seed(1)
x <- rnorm(100)
log.mu <- 0.5 + 0.3 * x 
y <- rpois(100,exp(log.mu))
summary(y)
plot(x,y)
set.seed(1)
sample(1:10,4)
sample(letters,5)
sample(1:10)
sample(1:10,replace = T)

#Profiling R code 
system.time()
system.time(mean(rnorm(999999999999)))

#Elapsed time > user time 
system.time(readLines('https://goal.com'))



#Elapsed time < user time 
hil <- function (n){
  i<- 1:n
  1 /outer(i - 1, i , "+")
}
x <- hil(1000)
system.time(svd(x))

#Longer expressions
system.time({
  n <- 1000 
  r <- numeric(n)
  for (i in 1:n){
    x <- rnorm(n)
    r[i] <- mean(x)
  }
  
})
Rprof()
summaryRprof()

sample.interval = 1000
