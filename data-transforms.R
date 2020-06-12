tropical = c('orange','dodgerblue','hotpink','limegreen','yellow')
palette(tropical)
par(pch=19)

#look through the Markdown file for required libraries for Data transforms
con=url("http://bowtie-bio.sourceforge.net/recount/ExpressionSets/bodymap_eset.RData")
load(file=con)
ls()
close(con)
bm = bodymap.eset
pdata = pData(bm)
edata = exprs(bm)
fdata = fData(bm)

#plot of simulated random data that follows normal distribution
hist(rnorm(1000),col=2)

hist(edata[,1],col=5)
hist(edata[,1],col=5,breaks = 1000)
#The visual shows that most of the data are close to zero and so we 
#do a log transform 
hist(log(edata[,1]),col=9)
hist(log(edata[,1]),col=9,breaks = 100)
min(log(edata))

quantile(log(edata[,1]))
# so we add 1s and do a log transform again 
hist(log(edata[,1] + 1 ))

hist(log2(edata[,1] + 1 ))

#Zooming in to the histogram 
hist(log2(edata[,1] + 1 ), breaks = 100, col=6, xlim = c(1,15),ylim = c(0,400))

#count the number of genes whose expression is equal to zero 
hist(rowSums(edata == 0),col = 3)

#Remove the low value genes 
low_genes = rowMeans(edata) < 5
table(low_genes)
filt_edata = filter(edata, !low_genes)
dim(filt_edata)

low_genes2 = rowMedians(as.matrix(edata)) < 5
table(low_genes,low_genes2)
