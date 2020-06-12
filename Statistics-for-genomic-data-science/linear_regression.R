library(devtools)
library(Biobase)
library(broom)

con =url("http://bowtie-bio.sourceforge.net/recount/ExpressionSets/bodymap_eset.RData")
load(file=con)
close(con)
bm = bodymap.eset
pdata=pData(bm)
edata=as.data.frame(exprs(bm))
fdata = fData(bm)

edata = as.matrix(edata)

#Fit the first gene to the age to yield a linear model
lm1 = lm(edata[1,] ~ pdata$age)
tidy(lm1)
plot(pdata$age, edata[1,], col = 2 )
#adding the linear model fit to this dataset 
#lwd = line width. 
abline(lm1, col =4 , lwd = 4)

table(pdata$gender)
plot(pdata$gender, pdata$age)

boxplot(edata[1,] ~ pdata$gender)
#Relating the gender to the outcome of gene expression 
points(edata[1,] ~ jitter(as.numeric(pdata$gender)), col = as.numeric(pdata$gender))

dummy_m = pdata$gender == "M"
dummy_f = pdata$gender == "F"

#A linear modeling relating the expression of gene 1 to the gender 
lm2 = lm(edata[1,] ~ pdata$gender)
tidy(lm2)
#We can also do the same for tissue type 
table(pdata$tissue.type)
#we may create dummy variables for the tissue types 
lm3 = lm(edata[1,] ~ pdata$tissue.type)
tidy(lm3)

#adjusting for variables 
#Adding for variabled for the model. 
lm4 = lm(edata[1,] ~ pdata$age + pdata$gender)
lm5 = lm(edata[1,] ~ pdata$age*pdata$gender)
tidy(lm5)

#we can overlay onto a graph 
lm6 = lm(edata[6,] ~ pdata$age)
plot(pdata$age, edata[6,], col= 9 )
abline(lm6, col = 7 , lwd = 3)

index = 1:19
lm7 = lm(edata[6,] ~ index)
plot(index, edata[6,], col = 2 )
abline(lm7,col = 3, lwd = 7)

lm8 = lm(edata[6,-19] ~index[-19])
abline(lm8, col = 7, lwd = 3)
#above shows the effect of the outlier on the result 
legend(5,1000, c('With outlier','without outlier'), col = c(3,7),lwd = 4)




##--
par(mfrom = c(1,2))
