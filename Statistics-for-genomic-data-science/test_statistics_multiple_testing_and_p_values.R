library(snpStats)
library(broom)
data(for.exercise)
use <- seq(1, ncol(snps.10), 10)
sub.10 <- snps.10[,use]
snpdata = sub.10@.Data
status = subject.support$cc

snp1 = as.numeric(snpdata[,3])
snp1[snp1==0] = NA

lm = lm(status ~ snp1)
tidy(lm)

#logistic regression
glm1 = glm(status ~ snp1,family="binomial")
tidy(glm1)

#Fitting for reccessive model 
data(for.exercise)
use <- seq(1, ncol(snps.10), 10)
sub.10 <- snps.10[,use]
snpdata = sub.10@.Data
status = subject.support$cc

snp10 = as.numeric(snpdata[,10])
snp10[snp10 == 0] = NA
snp10_rec = (snp10 == 2)


glm_rec = glm(status ~ snp10_rec,family="binomial")
glm_add = glm(status ~ snp10, family = "binomial")
tidy(glm_rec)
tidy(glm_add)

#Fit an additive regression model to all the snps 
results = rep(NA, dim(snpdata)[2])
for (i in 1:ncol(snpdata)){
  snpdata_i = as.numeric(snpdata[,i])
  snpdata_i[snpdata_i == 0] = NA
  glm_i = glm(status ~ snpdata_i, family = "binomial")
  results[i] = tidy(glm_i)$statistic[2]
}
mean(results)
max(results)
min(results)
#Square coefficients and correlate with the chi.squared
coefficients = results ^ 2
glm_all = snp.rhs.tests(status ~ 1,snp.data=sub.10)

qq.chisq(chi.squared(glm_all),df=1)
cor(coefficients, chi.squared(glm_all))


#CAlculate the F statistice an do log2 data transform 
con =url("http://bowtie-bio.sourceforge.net/recount/ExpressionSets/montpick_eset.RData")
load(file=con)
close(con)
mp = montpick.eset
pdata=pData(mp)
edata=as.data.frame(exprs(mp))
fdata = fData(mp)

edata = log2(as.matrix(edata) + 1)


library(devtools)
library(Biobase)
library(limma)
library(edge)
library(genefilter)
#CAlculation of the F-statistic for difference between studies 
fstats_obj = rowFtests(edata,as.factor(pdata$population))
names(fstats_obj)
hist(fstats_obj$p.value,col=2)

tstats_obj = rowttests(edata,pdata$population)
names(tstats_obj)
hist(tstats_obj$p.value)
tidy(tstats_obj)
hist(tstats_obj$statistic)
hist(fstats_obj$statistic)

con =url("http://bowtie-bio.sourceforge.net/recount/ExpressionSets/montpick_eset.RData")
load(file=con)
close(con)
mp = montpick.eset
pdata=pData(mp)
edata=as.data.frame(exprs(mp))
edata = edata[rowMeans(edata) > 100,]
fdata = fData(mp)
#USing the Limma to fit statistics 
de = DESeqDataSetFromMatrix(edata, pdata, ~study)
glm_all_nb = DESeq(de)
result_nb = results(glm_all_nb)
hist(result_nb$stat)
 
edata = log2(edata + 1)
mod = model.matrix(~ pdata$study)
fit_limma = lmFit(edata,mod)
ebayes_limma = eBayes(fit_limma)
head(ebayes_limma$t)

top = topTable(ebayes_limma, number=dim(edata)[1],sort.by="none")
cor(result_nb$stat, top$t)
y = cbind(result_nb$stat, top$t)
limma::plotMA(y)

result_nb$pvalue
top$P.Value
fp_bh = p.adjust(result_nb$pvalue,method="BH")
hist(fp_bh,col=3)
quantile(fp_bh)
top_bh = p.adjust(top$P.Value, method = "BH")


sum(fp_bh < 0.05)
sum(top_bh < 0.05)
