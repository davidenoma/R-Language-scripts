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







