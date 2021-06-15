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

#install package from CRAN
install.packages("hexbin")
install.packages("RColorBrewer")

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
summary(exprs(my.geo.gse))

#The function getGEOSuppFiles() downloads the raw data files to your computer. 
if(!file.exists(paste0("./geo_downloads/",my.gse)))
  getGEOSuppFiles(my.gse, makeDirectory=T, baseDir="geo_downloads")
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
plotDensity(exprs(my.mas5))

my.calls <-mas5calls(my.affy)
head(exprs(my.calls))



BiocManager::install('xps')

library('limma')
my.affy$sample.levels <- c(rep("non",5),"tnbc",rep("non",3),"tnbc","non","tnbc",rep("non",2),"tnbc",rep("non",3),"tnbc")
pData(my.rma)$sample.levels <- c(rep("non",5),"tnbc",rep("non",3),"tnbc","non","tnbc",rep("non",2),"tnbc",rep("non",3),"tnbc")

table(pData(my.affy)$description, pData(my.affy)$sample.levels)

plotMDS(exprs(my.rma), labels=pData(my.rma)$sample.levels, top=500, gene.selection="common", main="MDS Plot to Compare Replicates")

##make sample.levels a factor
pData(my.rma)$sample.levels <- as.factor(pData(my.rma)$sample.levels)
levels(pData(my.rma)$sample.levels)
  plotDensity(exprs(my.rma))
##Load limma to get plotDensities function. Load RColorBrewer to provide good
##color palettes.
library(limma)
library(RColorBrewer)
display.brewer.all()
level.pal <- brewer.pal(6, "Dark2")
level.cols <- level.pal[unname(pData(my.rma)$sample.levels)]
level.cols

plotDensities(exprs(my.rma), legend=F, col=level.cols, main="Arrays Not Normalized")
legend("topright", legend=levels(pData(my.rma)$sample.levels), fill=level.pal)

boxplot(exprs(my.rma), las=2, names=pData(my.rma)$sample.labels, outline=F, col=level.cols, main="Arrays Not Normalized")

#Problem. 
my.calls <- mas5calls(my.affy)
plotMDS(exprs(my.rma), labels=pData(my.rma)$sample.levels, top=500, gene.selection="common", main="MDS Plot to Compare Replicates")


my.design <- model.matrix(~0 + sample.levels, pData(my.rma))
my.design
rownames(my.design) <- pData(my.rma)$sample.labels
my.design
colnames(my.design) <- levels(pData(my.rma)$sample.levels)
my.design
##determine the average effect (coefficient) for each treatment
my.fit <- lmFit(my.rma, my.design)
write.table(my.fit$coefficients, file=paste0("results/",my.gse,"_Limma_Coeff.txt"), sep="\t", quote=F)

rownames(my.design) <- pData(my.rma)$sample.labels
my.design
colnames(my.design) <- levels(pData(my.rma)$sample.levels)
my.design
makeContrasts()
##determine the average effect (coefficient) for each treatment
my.fit <- lmFit(my.rma, my.design)





ph = my.affy@phenoData
colnames(ph@data)[2]="source"

groups = ph@data$source
f = factor(groups,levels=c("non","tnbc"))
design = model.matrix(~ 0 + f)
f
design
colnames(design) = c("non","tnbc")
data.matrix = exprs(my.rma)
data.fit = lmFit(data.matrix,design)


ourData <- my.rma[,my.rma$disease.state.ch1 %in% c("non-triple negative breat cancer tumor","triple negative breat cancer tumor" )]
ourData$cancerType <-  factor (ourData$disease.state.ch1)
#Linear Model
design <- model.matrix(~ ourData$cancerType)
fit <- lmFit(ourData, design)
fit <- eBayes(fit)
topTable(fit)



library (hgu133plus2.db)
columns(hgu133plus2.db)
hgu133plus2.db
["ENTREZID", "GENENAME", "SYMBOL"]
