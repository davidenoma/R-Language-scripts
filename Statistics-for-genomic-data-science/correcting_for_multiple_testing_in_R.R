tropical = c('darkorange','dodgerblue','hotpink','limegreen','yellow')
palette(tropical)
par(pch=19)
library(devtools)
library(Biobase)
library(limma)
library(edge)
library(genefilter)
library(qvalue)

con =url("http://bowtie-bio.sourceforge.net/recount/ExpressionSets/bottomly_eset.RData")
load(file=con)
close(con)
bot = bottomly.eset
pdata=pData(bot)
edata=as.matrix(exprs(bot))
fdata = fData(bot)

edata = log2(as.matrix(edata) + 1)
edata = edata[rowMeans(edata) > 10, ]

fstats_obj = rowFtests(edata, as.factor(pdata$strain))
hist(fstats)