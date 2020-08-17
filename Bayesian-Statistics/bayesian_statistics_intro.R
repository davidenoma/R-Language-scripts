z = seq(from=.1, to=.9, by=0.01)
plot(z,col='red')
trees
trees[,1]
sum(trees[,1])
names(trees)
attach(trees)
hist(Girth)
hist(trees[,1])
boxplot(Girth)
plot(Girth,Height)
#Useful Visualization tool
pairs(trees)
#Summary Statistics 
summary(trees)

#Plotting Likelihood in R 
#Binomial or Bernoulli 
likelihood = function(n,y,theta){
  return(theta^y*(1-theta)^(n-y))
}
theta = seq(from=0.01, to=0.99, by=0.01)
plot(theta, likelihood(400,72,theta))
72/420
abline(v=.18)
loglikelihood = function(n,y,theta){
  return (y*log(theta)+(n-y)*log(1-theta))
}

plot(theta,loglikelihood(400,72,theta),type='l')

