library(Biobase)
library(GenomicRanges)
data(sample.ExpressionSet, package = "Biobase")

se = makeSummarizedExperimentFromExpressionSet(sample.ExpressionSet)
#install SummarizeExperiment with BiocManager


colData(se)
rowData(se)
rowRanges(se)
assay(se)

con =url("http://bowtie-bio.sourceforge.net/recount/ExpressionSets/bottomly_eset.RData")
load(file=con)
close(con)
bot = bottomly.eset
pdata_bot=pData(bot)

con =url("http://bowtie-bio.sourceforge.net/recount/ExpressionSets/bodymap_eset.RData")
load(file=con)
close(con)
bm = bodymap.eset

pdata_bm=pData(bm)
edata = exprs(bm)
#Rank The 500 most likely highly expressed genes. 
row_sums = rowSums(edata)
index = which(rank(-row_sums) < 500 )
heatmap(edata[index,],Rowv=NA)

row_sums = rowSums(edata)
index = which(rank(-row_sums) < 500 )
heatmap(edata[index,],Rowv=NA,Colv=NA)

con =url("http://bowtie-bio.sourceforge.net/recount/ExpressionSets/montpick_eset.RData")
load(file=con)
close(con)
mp = montpick.eset
pdata=pData(mp)
edata=as.data.frame(exprs(mp))
fdata = fData(mp)

dist1 = dist(t(edata))
hclust1 = hclust(dist1)
plot(hclust1,col= 3, hang =-1 )

edata = edata[rowMeans(edata) < 100,]
dist2 = dist(t(edata))

colramp = colorRampPalette(c(3,"white",2))(9)
heatmap(as.matrix(dist2),col=colramp, Colv = NA, Rowv = NA)

edata=as.data.frame(exprs(mp))
edata = log2(edata + 1)
dist3 = dist(t(edata))

colramp = colorRampPalette(c(3,"white",2))(9)
heatmap(as.matrix(dist3),col=colramp, Colv = NA, Rowv = NA)
hclust3 = hclust(dist3)
library('rafalib')

myplclust(hclust = hclust1, lab.col = rep(1, length(hclust1$labels)))
myplclust(hclust = hclust2, lab.col = rep(1, length(hclust2$labels)))
myplclust(hclust = hclust3, lab.col = rep(1, length(hclust3$labels)))

#applying k-means clustering and log2transform 
edata=as.data.frame(exprs(mp))
edata = log2(edata + 1)
kmeans1 = kmeans(edata, centers = 2)
set.seed(1235)

matplot(t(kmeans1$centers),col=1:3, type = "l", lmd=3)
table(kmeans1$cluster)

