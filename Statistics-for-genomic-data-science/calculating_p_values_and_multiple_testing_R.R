tropical = c('darkorange','dodgerblue','hotpink','limegreen','yellow')
palette(tropical)
par(pch=19)
library(devtools)
library(Biobase)
library(limma)
library(edge)
library(genefilter)
library(qvalue)

con =url("http://bowtie-bio.sourceforge.net/recount/ExpressionSets/bottomly_eset.RData")
load(file=con)
close(con)
bot = bottomly.eset
pdata=pData(bot)
edata=as.matrix(exprs(bot))
fdata = fData(bot)

edata = log2(as.matrix(edata) + 1)
edata = edata[rowMeans(edata) > 10 ,]

#We can calculate pvalue and show the distribution of the p-values
#This means that we are trying to calculate the ftests statistic between the groupd 
#of the strains. 
fstats_obj = rowFtests(edata, as.factor(pdata$strain))
hist(fstats_obj$p.value)

#We can also use the edge for the p-value 
edge_study = build_study(edata, grp=pdata$strain, adj.var = as.factor(pdata$lane.number))
#Now we do the differenrial expression analysis 
de_obj = lrt(edge_study)
qval = qvalueObj(de_obj)
hist(qval$pvalues,col=3)

#Ideally we should have more pvalues close to zero and if this is not the case then there could 
#possibly be confounding effects 

#moderated p-values 
mod = model.matrix(~pdata$strain + pdata$lane.number)
fit_limma = lmFit(edata,mod)
ebayes_limma = eBayes(fit_limma)
limma_pvals = topTable(ebayes_limma,number= dim(edata)[1])$P.Value
hist(limma_pvals,col=4)

#Then we try more models because subsequent models are not expressing the ideal distribution of the pvalues 
set.seed(3333)
#Now we do 1000 random simulation 
B = 1000 
tstats_obj = rowttests(edata, pdata$strain)
tstat0 = matrix(NA,nrow=dim(edata)[1],ncol=B)
tstat = tstats_obj$statistic
strain = pdata$strain 
for (i in 1:B){
  strain0 = sample(strain)
  tstat0[,i] = rowttests(edata,strain0)$statistic
}

emp_pvals = empPvals(tstat,tstat0)
hist(emp_pvals,col=2)
 
#After calculating the p-value we have to adjust for multiple tesing 
fp_bonf = p.adjust(fstats_obj$p.value,method = "bonferroni")
hist(fp_bonf, col=3)
quantile(fp_bonf)

fp_bonf < 0.05
sum(fp_bonf < 0.05)

fp_bh = p.adjust(fstats_obj$p.value, method = "BH")
hist(fp_bh, col=3)
quantile(fp_bh)
sum(fp_bh < 0.05)


limma_pvals_adj = topTable(ebayes_limma, number=d==im(edata)[1])
limma_pvals_adj  =  limma_pvals_adj$adj.P.Val
quantile(limma_pvals_adj)
hist(limma_pvals_adj,col=2)
sum(limma_pvals_adj < 0.05)
#After adjustment we get 2 significant p values 
qvalue_limma = qvalue(limma_pvals)
summary(qvalue_limma)
qval$pi0

#We can also apply the edge object to this 
qval = qvalueObj(de-)