library(Biobase)
library(limma)
con =url("http://bowtie-bio.sourceforge.net/recount/ExpressionSets/bottomly_eset.RData")
load(file=con)
close(con)
bot = bottomly.eset
pdata_bot=pData(bot)
fdata_bot = featureData(bot)
edata = exprs(bot)
fdata_bot = fdata_bot[rowMeans(edata) > 5]
edata = edata[rowMeans(edata) > 5, ]
edata = log2(edata+1)

#Finding Differentially Expressed genes. 
mod = model.matrix(~pdata_bot$strain)
fit_limma = lmFit(edata,mod)
ebayes_limma = eBayes(fit_limma)
limma_pvals = topTable(ebayes_limma,number=dim(edata)[1], adjust.method ="BH", p.value=0.05, sort.by='none')
limma_pvals[1,]
dim(limma_pvals)

library(devtools)
library(Biobase)
library(goseq)
library(DESeq2)
#Now to get info on the names of the genes 
limma_table = topTable(ebayes_limma,number=dim(edata)[1], adjust.method ="BH", sort.by='none')
genes = as.integer(limma_table$adj.P.Val < 0.05)
names(genes) = rownames(edata)
not_na = !is.na(genes)
genes = genes[not_na]


pwf = nullp(genes, "mm9", "ensGene")
GO.wall = goseq(pwf, "mm9", "ensGene")
head(pwf)
GO.top10 = GO.wall[1:10,1]
GO.top10[1]
GO.wall$term[1]
limma_pvals = topTable(ebayes_limma,number=dim(edata)[1])
limma_pvals
hist(limma_pvals,col=4)

top_bh = p.adjust(limma_pvals$P.Val, method = "BH")

binded = cbind(top_bh, rownames(limma_pvals))
binded[,1] = as.double(binded[,1])
filter_bind = binded [ as.double (binded[,1]) < 0.05]

diff_genes = filter_bind[224:length(filter_bind)]
sum(top_bh < 0.05)
# 
# library(goseq)
# library(DESeq2)

pwf = nullp(diff_genes, "mm9", "ensGene")
head(pwf)
GO.wall = goseq(pwf, 'mm9','ensGene')
head(GO.wall)
supportedGenomes()
