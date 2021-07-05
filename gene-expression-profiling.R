#R scripts for GEO Analysis.
# #get BiocManager::install()
source("http://bioconductor.org/BiocManager::install.R")
# #install base Bioconductor

# #install packages for this analysis
BiocManager::install("GEOquery")
BiocManager::install("affy")
BiocManager::install("limma")
BiocManager::install("hgu133plus2.db")
BiocManager::install("Homo.sapiens")
BiocManager::install("pheatmap")

#install package from CRAN
install.packages("hexbin")
install.packages("RColorBrewer")
install.packages("ggrepel")

library(GEOquery)
my.gse<- "GSE27447"
if(!file.exists("geo_downloads")) dir.create("geo_downloads")
if(!file.exists("results"))  dir.create("results", recursive=TRUE)
##get data from GEO
my.geo.gse <- getGEO(GEO=my.gse, filename=NULL, destdir="./geo_downloads", GSElimits=NULL, GSEMatrix=TRUE, AnnotGPL=FALSE, getGPL=FALSE)
class(my.geo.gse)
length(my.geo.gse)
names(my.geo.gse)

##get rid of list structure
my.geo.gse <- my.geo.gse[[1]]

##object is now an ExpressionSet
class(my.geo.gse)
str(my.geo.gse)
#Phenotype Data
colnames(pData(my.geo.gse))


pData(my.geo.gse)[1]
pData(my.geo.gse)$data_processing[1]
exprs(my.geo.gse)
head(exprs(my.geo.gse))
summary(exprs(my.geo.gse))



#Trying log2 normalization
gse <- my.geo.gse

#Some exploratory data analysis 
library(dplyr)
sampleInfo <- pData(gse)
sampleInfo <- dplyr::select(sampleInfo, "description","disease state:ch1")
sampleInfo <- dplyr::rename(sampleInfo,sample = "description", "state"="disease state:ch1")
library(pheatmap)
corMatrix <- cor(exprs(gse),use="c")
pheatmap(corMatrix)  
rownames(sampleInfo)
# rownames(sampleInfo) <- colnames(corMatrix)

#Correlation Matrix
pheatmap(corMatrix,annotation_col=sampleInfo)  
exprs(gse) <- log2(exprs(gse))
boxplot(exprs(gse),outline=FALSE)
library(ggplot2)
library(ggrepel)
pca <- prcomp(t(exprs(gse)))
cbind(sampleInfo, pca$x) %>% 
ggplot(aes(x = PC1, y=PC2, col=sample,label=paste("description", description))) + geom_point() + geom_text_repel()

#The function getGEOSuppFiles() downloads the raw data files to your computer. 
if(!file.exists(paste0("./geo_downloads/",my.gse))){
  getGEOSuppFiles(my.gse, makeDirectory=T, baseDir="geo_downloads")
}
  
list.files("geo_downloads")
list.files(paste0("geo_downloads/",my.gse))

file.list <- read.delim(paste0("geo_downloads/",my.gse,"/filelist.txt"), as.is=T)
file.list
##function creates a directory and downloads the "tarball" that contains
##gzipped CEL files. We need to untar the tarball but we do not need to unzip
##the CEL files. R can read them in compressed format.

untar(paste0("geo_downloads/",my.gse,"/",my.gse,"_RAW.tar"), exdir=paste0("geo_downloads/",my.gse,"/CEL"))
list.files(paste0("geo_downloads/",my.gse,"/CEL"))
my.cels <- list.files(paste0("geo_downloads/",my.gse,"/CEL"), pattern=".CEL")
my.cels <- sort(my.cels)
my.cels

#PROCESSING THE MICROARRAY DATA

##make data frame of phenoData
my.pdata <- as.data.frame(pData(my.geo.gse), stringsAsFactors=F)
head(my.pdata)  
dim(my.pdata)
colnames(my.pdata)

head(my.pdata[, c("title", "geo_accession", "description")], 10)

my.pdata <- my.pdata[, c("description", "disease state:ch1" )]
my.pdata <- my.pdata[order(rownames(my.pdata)), ]
head(my.pdata, 10)

##the rownames of the data frame must be the same as the CEL file names.
##For this data, we need to do a bit of tinkering.
rownames(my.pdata) == my.cels
table(rownames(my.pdata) == my.cels)

head(rownames(my.pdata))
temp.rownames <- paste(rownames(my.pdata), ".CEL.gz", sep="")
rownames(my.pdata) <- temp.rownames


#This is to ensure that the rownames of the phenotype are the same with the dataset naming. 

rownames(my.pdata) <- my.cels
table(rownames(my.pdata) == my.cels)
write.table(my.pdata, file=paste0("geo_downloads/",my.gse,"/CEL/",my.gse,"_SelectPhenoData.txt"), sep="\t", quote=F)
my.pdata

# Reading the CEL Files
##Perform affy normalization
library(affy)
cel.path <- paste0("geo_downloads/",my.gse,"/CEL")
my.affy <- ReadAffy(celfile.path=cel.path, phenoData=paste(cel.path, paste0(my.gse,"_SelectPhenoData.txt"), sep="/"))
show(my.affy)
head(exprs(my.affy))

#NOrmalization of the expression object
my.rma <- rma(my.affy)
head(exprs(my.affy))
colnames(my.affy)

dim(exprs(my.affy))
dim(exprs(my.rma))

#Raw Expression Data
plotDensity(exprs(my.affy))
#Normalized expression levels. 
plotDensity(exprs(my.rma))


my.mas5 <- mas5(my.affy)

#This is not running
plotDensity(exprs(my.mas5))
#This is also not running

my.calls <-mas5calls.AffyBatch(my.rma)
head(exprs(my.calls))



# my.rma<-my.rma[,c(1:5,6,10,12,15,19)]
my.rma

library('limma')
#Manual creation of levels, could be faster with another approach. 
my.affy$sample.levels <- c(rep("non",5),"tnbc",rep("non",3),"tnbc","non","tnbc",rep("non",2),"tnbc",rep("non",3),"tnbc")

pData(my.rma)$sample.levels <- c(rep("non",5),"tnbc",rep("non",3),"tnbc","non","tnbc",rep("non",2),"tnbc",rep("non",3),"tnbc")

pData(my.rma)
table(pData(my.affy)$description, pData(my.affy)$sample.levels)

plotMDS(exprs(my.rma), labels=pData(my.rma)$sample.levels, top=500, gene.selection="common", main="MDS Plot to Compare Replicates")

##make sample.levels a factor
pData(my.rma)$sample.levels <- as.factor(pData(my.rma)$sample.levels)
plotDensity(exprs(my.rma))
##Load limma to get plotDensities function. Load RColorBrewer to provide good
##color palettes.
library(limma)
library(RColorBrewer)
library(pheatmap)

display.brewer.all()
level.pal <- brewer.pal(6, "Dark2")
level.cols <- level.pal[unname(pData(my.rma)$sample.levels)]
level.cols

plotDensities(exprs(my.affy), legend=F, col=level.cols, main="Arrays Not Normalized")
plotDensities(exprs(my.rma), legend=F, col=level.cols, main="Arrays Normalized")
legend("topright", legend=levels(pData(my.rma)$sample.levels), fill=level.pal)

boxplot(exprs(my.affy), las=2, names=pData(my.affy)$sample.labels, outline=F, col=level.cols, main="Arrays Not Normalized")
boxplot(exprs(my.rma), las=2, names=pData(my.rma)$sample.labels, outline=F, col=level.cols, main="Arrays  Normalized")

#Problem. 
my.calls <- mas5calls(my.affy)
plotMDS(exprs(my.rma), labels=pData(my.rma)$sample.levels, top=500, gene.selection="common", main="MDS Plot to Compare Replicates")



#The next step in the analysis is to describe the experiment 
#and analysis for limma in the form of a matrix. T
my.design <- model.matrix(~0 + pData(my.rma)$sample.levels, pData(my.rma))
my.design

#rownames(my.design) <- pData(my.rma)$sample.labels
colnames(my.design) <- levels(pData(my.rma)$sample.levels)
my.design

##determine the average effect (coefficient) for each treatment
my.fit <- lmFit(my.rma, my.design)
write.table(my.fit$coefficients, file=paste0("results/",my.gse,"_Limma_Coeff.txt"), sep="\t", quote=F)
my.fit

##specify the contrast of interest using the levels from the design matrix
my.contrasts <- makeContrasts(non - tnbc, tnbc - non, levels=my.design)
#contrast.fits <- sapply(colnames(my.contrasts), function(x)(contrasts.fit(my.fit, contrasts=my.contrasts[, x])))
length(contrast.fits)


fit2 <- contrasts.fit(my.fit, my.contrasts)
fit2 <- eBayes(fit2)
topGenes <-topTable(fit2,adjust="BH", number=length(fit2$coefficients), sort.by="none")
my.tests <- decideTests(fit2,method="separate", adjust.method="BH", p.value=0.05, lfc=0)
table(my.tests)
summary(my.tests)
anno <- fData(my.rma)
anno
#The top genes with a p-value of less than 0.05
topGenes[topGenes$adj.P.Val<0.05,]
library (hgu133plus2.db)
columns(hgu133plus2.db)
keytypes(hgu133plus2.db)
gene.data <- select(hgu133plus2.db, keys=rownames(topGenes, keytype="PROBEID", columns=c("ENTREZID", "GENENAME", "SYMBOL")))
library(hugene10stv1 )
                    