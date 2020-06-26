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

edata = log2(as.matrix(edata) + 1 )
edata = edata[rowMeans(edata) > 10, ]

tstats_obj = rowttests(edata, pdata$strain)
length(tstats_obj$statistic)
dim(edata)
length(pdata$strain)
hist(tstats_obj$statistic, col = 2 , xlim = c(-5,2))
set.seed(135)e
# We seet seed when performing operations that require randomness to increase replicability
strain = pdata$strain
#To permute we use the sample command to randomize 
strain0 = sample(strain)
 #We are trying to break relationship between the strain and the expression data 
tstats_obj0 = rowttests(edata,strain0)
hist(tstats_obj0$statistic, col = 2, xlim = c(-5,2))


quantile(tstats_obj$statistic)
quantile(tstats_obj0$statistic)

