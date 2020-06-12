tropical(c('darkorange','dodgerblue','hotpink','limegreen','yellow'))
palette(tropical)


con =url("http://bowtie-bio.sourceforge.net/recount/ExpressionSets/montpick_eset.RData")
load(file=con)
close(con)
mp = montpick.eset
pdata=pData(mp)
edata=as.data.frame(exprs(mp))
fdata = fData(mp)

edata = log2(edata + 1 )
edata = edata[rowMeans(edata) > 3, ]
colramp = colorRampPalette(c(3, 'white', 2 ))(20)

plot(density(edata[,1], col = colramp[1], lwd =3 , ylim = c(0,.20)))

for(i in 2:20){lines(density(edata[,i]), lwd=3, col=colramp[i])}

#Quantile Normalization 
norm_edata = normalize.quantiles(as.matrix(edata))
plot(density(norm_edata[,1], col = colramp[1], lwd =3 , ylim = c(0,.20)))
for(i in 2:20){lines(density(norm_edata[,i]), lwd=3, col=colramp[i])}
##We can see from the plot that there is a difference between the normalized on and the previous. 

#Remove gene by gene variability. 
plot(norm_edata[1,],col = as.numeric(pdata$study))


#Decomposition 
svd1 = svd(norm_edata - rowMeans(norm_edata))
plot(svd1$v[,1],svd1$v[,2],xlab = 'PC1', ylab = 'PC2',
          col = as.numeric(pdata$study))
