tropical  = c('darkorange','dodgerbue','hotpink','limgreen','black');
palette(tropical)
par(pch=17)

library(devtools)
library(Biobase)
library(MatrixEQTL)
library(broom)
#To load  data from the package 
base.dir = find.package("MatrixEQTL")

#Now we load the SNP data, expression data and covariates 
SNP_file_name = paste(base.dir,"/data/SNP.txt",sep="")
expr_file_name = paste(base.dir, "/data/GE.txt",sep="")
covariates_file_name = paste(base.dir, "/data/Covariates.txt",sep ="")
output_file_name = tempfile()

#We should make sure to read the file to have an idea of the headers 
expr = read.table(expr_file_name, sep="\t",header = T, row.names = 1
                  )
expr
snps = read.table(SNP_file_name,sep="\t",
                  header = T, row.names = 1)
snps
cvrt = read.table(covariates_file_name,sep = "\t", header = T, row.names = 1)

e1 = as.numeric(expr[1,])
s1 = as.numeric(snps[1,])
#To relate the fiest set of genes to the first set of snps 
lm1 = lm( e1 ~ s1)
tidy(lm1)
#Now we plot the expression levels versus the genotype 
plot(e1 ~ jitter(s1), col=(s1+1) , xaxt ="n", xlab = "Genotype", "ylab" = "Expression")
#Addition of axix label 
#Assuming the additive model 
axis(1, at=c(0:2),labels = c("AA","Aa", "aa"))
lines(lm1$fitted.values ~s1, type="b", pch=14, col="grey")

#Bilion of computations require a better models
pvOutputThreshold = 1e-2
errorCovariance = numeric()
useModel = modelLINEAR

snps = SlicedData$new()
snps$fileDelimiter = "\t"
snps$fileOmitCharacters = "NA"
snps$fileSkipRows = 1
snps$fileSkipColumns = 1
snps$fileSliceSize = 2000
snps$LoadFile(SNP_file_name)

gene = SlicedData$new()
gene$fileDelimiter = "\t"
gene$fileOmitCharacters = "NA"
gene$fileSkipRows = 1
gene$fileSkipColumns = 1
gene$fileSliceSize = 2000
gene$LoadFile(expr_file_name)

cvrt = SlicedData$new()

  me = Matrix_eQTL_engine(
    
        snps = snps,
        gene = gene,
        cvrt = cvrt, 
        output_file_name = NULL,
        pvOutputThreshold = pvOutputThreshold,
        useModel = useModel,
        errorCovariance = errorCovariance,
        verbose = TRUE,
        pvalue.hist = TRUE, 
        min.pv.by.genesnp = FALSE,
        noFDRsaveMemory = FALSE)
plot(me)

#Number of Tests
me$all$ntests
me$all$neqtls

