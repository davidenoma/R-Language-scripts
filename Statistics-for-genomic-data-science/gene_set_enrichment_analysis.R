tropical = c('darkorange','dodgerblue','hotpink','limegreen','yellow')
palette(tropical)
par(pch = 19)
library(devtools)
library(Biobase)
library(goseq)
library(DESeq2)

head(supportedGenomes())
head(supportedGeneIDs())

temp_data = read.table(system.file("extdata","li_sum.txt",package = "goseq"),sep="\t",
                       header = TRUE, stringsAsFactors = FALSE)

#Removing the first column
expr = temp_data[,-1]
head(expr)
rownames(expr) = temp_data[,1]
rownames(expr)
head(expr)

#Removing lowly expressed genes 
expr = expr[rowMeans(expr) > 5, ]
grp = factor(rep(c("Control","Treated"), times = c(4,3)))
grp
pdata = data.frame(grp)

#Now we pass the factors to DESeq
#Genotype, Phenotype and the Model to fit which is the group
de = DESeqDataSetFromMatrix(expr, pdata, ~grp)
#Identifying the differentially expressed genes with dese1
de_fit = DESeq(de)
de_results = results(de_fit)
head(de_results)

genes = as.integer(de_results$padj < 0.05)
not_na = !is.na(genes)
#to make the row names match because it is a program requirement.
names(genes) = rownames(expr)
genes = genes[not_na]

#Probability weighted function
pwf = nullp(genes, "hg19", "ensGene")
head(pwf)
GO.db::GO_dbInfo()
GO.wall = goseq(pwf, "hg19","ensGene")
#Finds the GO annotations from the web
head(GO.wall)
#To find the GO for a particular category
#This is specific for the Molecular function subset of the Gene Ontology. 

GO.MF = goseq(pwf,"hg19","ensGene",test.cats = c("GO:MF"))


