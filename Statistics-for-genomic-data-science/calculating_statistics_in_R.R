library(devtools)
library(Biobase)
library(limma)
library(edge)
library(genefilter)

con =url("http://bowtie-bio.sourceforge.net/recount/ExpressionSets/bottomly_eset.RData")
load(file=con)
close(con)
bot = bottomly.eset
pdata=pData(bot)
edata=as.matrix(exprs(bot))
fdata = fData(bot)
ls()


edata = log2(as.matrix(edata) + 1 )
edata = edata[rowMeans(edata) > 10, ]
tstats_obj = rowttests(edata, pdata$strain)
names(tstats_obj)
hist(tstats_obj$statistic, col = 4)
fstats_obj = rowFtests(edata, as.factor(pdata$lane.number))
table(pdata$lane.number)
names(fstats_obj)
hist(fstats_obj$statistic, col = 3)

#We can also fit using the limma model 
mod = model.matrix(~pdata$strain)
fit_limma = lmFit(edata,mod)
#This is to shrink the statistic 

ebayes_limma = eBayes(fit_limma)
head(ebayes_limma)
plot(ebayes_limma$t[,2], -tstats_obj$statistic, col = 4 , xlab = "Moderated T- stat", ylab = " T-stat")
#The plot shows that there is a difference between the modearated the actual T-statistic. 

abline(c(0,1), col = "red", lwd = 3)

#In this we are creating and adjusted model matrix
mod_adj = model.matrix(~ pdata$strain + as.factor(pdata$lane.number))
fit_limma_adj = lmFit(edata,mod_adj)
#Shrinking the T Statistic 
ebayes_limma_adj  = eBayes(fit_limma_adj)
head(ebayes_limma_adj$t)


plot(ebayes_limma_adj$t[,2], -tstats_obj$statistic, col=3, xlab = "Moderated T-stat", ylab = " T-stat" )
abline(c(0,1),lwd=3,col='red')


mod_lane = model.matrix((~ as.factor(pdata$lane.number)))
#This in when we want to fit the sam[le date with the factor as a model 
fit_limma_lane = lmFit(edata,mod_lane)
#E bayes is the empirical estimation of the bayes statistics
ebayes_limma_lane = eBayes(fit_limma_lane)
head(ebayes_limma_lane$t)


#To find the top associated genes with the lane 
top_lane = topTable(ebayes_limma_lane, coef=2:7, number = dim(edata)[1],sort.by = "none")
head(top_lane)
plot(top_lane$F, fstats_obj$statistic, xlab = "Moderated F-Statistic", ylab = "F-Statistic", col = 3)

edge_study = build_study(edata, grp = as.factor(pdata$lane.number))
de_obj = lrt(edge_study)
qvalue = qvalueObj(de_obj)
names(qvalue)
plot(qvalue$stat, fstats_obj$statistic , col = 4 , xlab = "F-stat from edge", ylab = "F-stat from gene Filter")

#When we adjust for the strain. 
edge_study2 = build_study(edata, grp = as.factor(pdata$lane.number),adj.var = pdata$strain)
de_obj2 = lrt(edge_study2)
qval2 = qvalueObj(de_obj2)
plot(qval2$stat, fstats_obj$statistic, col=4, xlab = "F-stats from Edge", ylab = "Fstat from gene filter")
