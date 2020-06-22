con =url("http://bowtie-bio.sourceforge.net/recount/ExpressionSets/montpick_eset.RData")
load(file=con)
close(con)
mp = montpick.eset
pdata=pData(mp)
edata=as.data.frame(exprs(mp))
fdata = fData(mp)

library(devtools)
library(Biobase)
svd1 = svd(edata)
#Pecentage variation explained by the first principal component
plot(svd1$d,ylab="Singular value",col=2)
plot(svd1$d^2/sum(svd1$d^2),ylab="Percent Variance Explained",col=2)

#Plottingt the top two principal components 
par(mfrow=c(1,2))
plot(svd1$v[,1],col=2,ylab="1st PC")
plot(svd1$v[,2],col=2,ylab="2nd PC")

#Percentage explained after log2 transform
edata = log2(edata + 1)
svd2 = svd(edata)
plot(svd2$d,ylab="Singular value",col=2)
plot(svd2$d^2/sum(svd2$d^2),ylab="Percent Variance Explained",col=2)
#Percentage variation after subtracting the row means and log2 transformation
edata_centered = edata - rowMeans(edata)
svd3 = svd(edata_centered)
plot(svd3$d,ylab="Singular value",col=2)
plot(svd3$d^2/sum(svd3$d^2),ylab="Percent Variance Explained",col=2)

#Doing K means clustering 
edata = edata[rowMeans(edata) > 100, ]
edata = log2(edata + 1)
edata_centered = edata - rowMeans(edata)

kclust = kmeans(t(edata_centered),centers = 2)
set.seed(333)
svd4 = svd(edata_centered)
#Corrrelation betwwen the first singular vector and the sample clustering indicator 
matplot(t(kclust$centers),col=1:3,type="l",lwd=3)
cor(kclust$cluster,svd4$v[,1])


#Linear model 
con =url("http://bowtie-bio.sourceforge.net/recount/ExpressionSets/bodymap_eset.RData")
load(file=con)
close(con)
bm = bodymap.eset
edata = exprs(bm)
pdata_bm=pData(bm)
edata = as.matrix(edata)
lm1 = lm(edata[1,] ~ pdata_bm$num.tech.reps)
tidy(lm1)
plot(pdata_bm$num.tech.reps , edata[1,])

abline(lm1$coeff[1], lm1$coeff[2], col=2, lwd=3)

#Linear Model Two 
con =url("http://bowtie-bio.sourceforge.net/recount/ExpressionSets/bodymap_eset.RData")
load(file=con)
close(con)
bm = bodymap.eset
edata = exprs(bm)
pdata_bm=pData(bm)
lm2 = lm(edata[1,] ~ pdata_bm$age + pdata_bm$gender)
summary(lm2)

#Linear Regression 3 
con =url("http://bowtie-bio.sourceforge.net/recount/ExpressionSets/montpick_eset.RData")
load(file=con)
close(con)
mp = montpick.eset
pdata=pData(mp)
edata=as.data.frame(exprs(mp))
fdata = fData(mp)
edata = log2(edata + 1)


mod = model.matrix(~pdata$population)

lm3 = lm.fit(mod, t(edata))
summary(lm3)
dim(lm3$residuals)
dim(lm3$effects)
dim(lm3$coefficients)


##The Many Regression Model 
library(devtools)
library(Biobase)
library(limma)
library(edge)

con =url("http://bowtie-bio.sourceforge.net/recount/ExpressionSets/bodymap_eset.RData")
load(file=con)
close(con)
bm = bodymap.eset
edata = exprs(bm)
pdata_bm=pData(bm)

# subset the expression data to the samples without mimssing values of age
pdata_bm = na.omit(pdata_bm)
edata = edata[,rownames(pdata_bm), drop=FALSE]

age = model.matrix(~pdata_bm$age)
limma_fit = lmFit(edata, age)
limma_fit$coefficients[1000,]
