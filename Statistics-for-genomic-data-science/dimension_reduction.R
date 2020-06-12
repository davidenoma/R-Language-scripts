tropical = c('orange','dodgerblue','hotpink','limegreen','yellow')
palette(tropical)
par(pch=19)

#look through the Markdown file for required libraries for Data transforms

con =url("http://bowtie-bio.sourceforge.net/recount/ExpressionSets/montpick_eset.RData")
load(file=con)
close(con)
mp = montpick.eset
pdata=pData(mp)

#convert to data frame.
edata=as.data.frame(exprs(mp))
fdata = fData(mp)


edata = edata [rowMeans(edata) > 100, ]
edata = log2(edata + 1 ) 
#to remove the mean row mean from the dataset 

edata_centered = edata - rowMeans(edata)

sv1 = svd(edata_centered)
names(sv1)
dim(sv1$v)



plot(sv1$d, ylab = 'Singular Value', col = 2)

plot(sv1$d^2/sum(sv1$d^2), ylab = ' Percent Variance Explained ', col = 2)
par(mfrom = c (1,2))
plot(sv1$v[,1], col = 2, ylab = '1st PC ')
plot(sv1$v[,2], col = 2, ylab ='2nd PC' )

plot(sv1$v[,1], sv1$v[,2], col = 5 , ylab = '2nd PC', xlab = '1st PC')
par(mfrom = c (1,1))
plot(sv1$v[,1], sv1$v[,2], ylab = '2nd PC', xlab = '1st PC', col=as.numeric(pdata$study) )
boxplot(sv1$v[,1]~ pdata$study, border = c(1,2))
points(sv1$v[,1] ~ jitter(as.numeric(pdata$study))  , col = as.numeric(pdata$study))

pc1 = prcomp(edata)
plot(pc1$rotation[,1],sv1$v[,1],col = 4)
edata_centered2 = t(t(edata) - colMeans(edata))
sv2 = svd(edata_centered2)
plot(pc1$rotation[,1],sv2$v[,1], col = 4)

edata_outlier = edata_centered
edata_outlier[6,] = edata_centered[6,] * 10000
sv3 = svd(edata_outlier)
plot(sv1$v[,1],sv3$v[,1],xlab = 'Without Outlier', ylab = 'With outlier')

