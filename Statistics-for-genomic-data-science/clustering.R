tropical = c('orange','dodgerblue','hotpink','limegreen','yellow')
palette(tropical)
par(pch=19)

#look through the Markdown file for required libraries for Clustering

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

#This gives us all the columns that have means greater than 5000
edata = edata[rowMeans(edata) > 5000,]
#Then we transform
edata = log2(edata + 1)

#Calculate the distance between the rows 
dist1 = dist(t(edata))
colramp = colorRampPalette(c(3,"white",2))(9)
heatmap(as.matrix(dist1),col=colramp, Colv = NA, Rowv = NA)

hclust1 = hclust(dist1)
plot(hclust1,col= 3, hang =-1 )

dend = as.dendrogram(hcust1)
dend = dendextend::color_labels(hclust1,4,1:14)
plot(dend)

dendextend::labels_colors(dend) = c (rep(1,10),rep(2,9))


##Kmeans clustering 
kmeans1 = kmeans(edata, centers = 3)
names(kmeans1)
matplot(t(kmeans1$centers),col=1:3, type = "l", lmd=3)
table(kmeans1$cluster)

newdata = as.matrix(edata)[order(kmeans1$cluster),]
heatmap(newdata, col = colramp, Colv = NA, Rowv = NA)


