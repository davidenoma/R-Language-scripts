tropical = c ('darkorange','dodgerblue','hotpink','limegreen','yellow')
palette(tropical)
library(knitr)

knit_hooks$set(setPch = function(before,options,envir){
  if(before) par(pch = 19)
})
opts_chunk$set(setPch = TRUE)

knitr::opts_chunk$set(fig.width = 5,fig.height = 5, size = "footnotesize", warning = FALSE, message = FALSE)

#look through the Markdown file for required libraries for Exploratory Data analysis

con=url("http://bowtie-bio.sourceforge.net/recount/ExpressionSets/bodymap_eset.RData")
load(file=con)
ls()
close(con)
bm = bodymap.eset
pdata = pData(bm)
edata = exprs(bm)
fdata = fData(bm)
#Basic operation

table(pdata$gender)

table(pdata$gender, pdata$race)

summary(edata)

sum(pdata$age == ' ', na.rm = TRUE)

dim(edata)
is.na(edata)[1,]
#This sums all N/As and to check for NAs
sum(is.na(edata))

gene_na = rowSums(is.na(edata))
table(gene_na)
sampe_na = colSums(is.na(edata))
table(sample_na)
#Always check for missing values 
c(dim(pdata), dim(edata), dim(fdata))

boxplot(edata[,1])

boxplot(log2(edata[,1]+1))

boxplot(log2(edata + 1),col=4,range = 0)

par(mfrom = c(1,2))
hist(log2(edata[,1]+1),col = 4)
hist(log2(edata[,2]+1),col = 4)
par(mfrom = c(1,1))
plot(density(log2(edata[,1]+1)),col = 3)
plot(density(log2(edata[,1]+1)),col = 4)
#The lines command is to allow overlay of the plots
lines(density(log2(edata[,2]+1)),col = 4)

qqplot(log2(edata[,1]+1),log2(edata[,2]+1))
abline(c(0,1))

##Ma plot
mm = log2(edata[,1] + 1) - log2(edata[,2] + 1)
aa = log2(edata[,1] + 1) + log2(edata[,2] + 1)
plot(aa, mm , col= 9)

edata = as.data.frame(edata)

#Removing row means less than 1 to eliminate the zero values
filt_edata  = filter(edata,rowMeans(edata) > 1)
dim(filt_edata)

boxplot(as.matrix(log2(filt_edata + 1)), col = 3)

#Compare datasets to annotation to ensure correctness
aeid = as.character(fdata[,1])
#to extract chromosome information
chr = AnnotationDbi::select(org.Hs.eg.db,keys = aeid, keytype = "ENSEMBL",columns = "CHR")
#Take non duplicated chromosomes 
chr = chr[!duplicated(chr[,1]),]
dim(chr)
##Confirm that the annotation is still in the right order 
all(chr[,1]== rownames(edata))

#Select the chromosome Y genes 
edataY = dplyr::filter(edata,chr$CHR=="Y")

# We plot the sums of columns of samples bearing the Y chromosome against gender 
boxplot(colSums(edataY) ~ pdata$gender)
points(colSums(edataY) ~ jitter(as.numeric(pdata$gender)),
       col = as.numeric(pdata$gender),pch=19)
##What we were able to achieve was validate the genders in the datasets. 

#MUltivariate plots

ematrix = as.matrix(edata) [rowMeans(edata) > 10000,]
heatmap(ematrix)
colramp = colorRampPalette(c(3,"white",2))(9)
heatmap(ematrix,col = colramp)
#To remove the automatic clustering
heatmap(ematrix,col = colramp, Rowv = NA, Colv = NA)

heatmap.2(ematrix,col = colramp, Rowv = NA, Colv = NA, dendogram = "none", scale = "row", trace = "none")

