con=url("http://bowtie-bio.sourceforge.net/recount/ExpressionSets/bodymap_eset.RData")
load(file=con)

close(con)

bm = bodymap.eset
#View(bm)
dim(bm)
#This gives the genes expressed in the ESET
exp_data = exprs(bm)
head(exp_data)
#This gives the phenotype data of the ESET
pheno_data = pData(bm)
head(pheno_data)

#This gives the feature data 
feature_data = featureData(bm)
dim(feature_data)
