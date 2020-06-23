library(devtools)
library(Biobase)
library(snpStats)
library(broom)
library(MASS)
library(DESeq2)
data(for.exercise)

use = seq(1, ncol(snps.10),10)
sub.10 = snps.10[,use]


#xxt is to normalize and standardize according to the hardy weinberg equilibrium 

xxmat = xxt(sub.10,correct.for.missing = FALSE)
evv = eigen(xxmat, symmetric = TRUE)
pcs = evv$vectors[,1:5]
#The code below is to load the SNPs
snpdata = sub.10@.Data
status = subject.support$cc
snp1 = as.numeric(snpdata[,1])
snp1[snp1 == 0] = NA

#Next we fit a GLM model 
#This is for the additive model
glm1 = glm(status ~ snp1, family = "binomial")
tidy(glm1)
snp1_dom = (snp1 == 1)
glm1_dom = glm(status ~ snp1_dom, family = "binomial" )
tidy(glm1_dom)

#Adjusting for the population structure with the first five principal components because of possible 
#confounding effect 

glm2 = glm(status ~ snp1 + pcs[,1:5], family = "binomial")
 tidy(glm2)
glm_all = snp.rhs.tests(status ~ 1 , snp.data = sub.10)
slotNames(glm_all) 
par(mfrow = c(1,1))
qq.chisq(chi.squared(glm_all), df = 1 )

#Adjusting for the principal components 
glm_all_adj = snp.rhs.tests(status ~ pcs , snp.data = sub.10)
qq.chisq(chi.squared(glm_all_adj), df = 1 )


#For the poisson regression models this is also called the negative binomial regression. 
con =url("http://bowtie-bio.sourceforge.net/recount/ExpressionSets/bottomly_eset.RData")
load(file=con)
close(con)
bot = bottomly.eset
pdata=pData(bot)
edata=as.matrix(exprs(bot))
fdata = fData(bot)
ls()
edata = edata[rowMeans(edata) > 10,]
glm3 = glm(edata[1,] ~ pdata$strain, family = "poisson")
tidy(glm3)
#To fir a negative binomial 
glm.nb1 = glm.nb(edata[1,] ~ pdata$strain)
tidy(glm.nb1)


#we can also do many negative binomical regressions at once 
de = DESeqDataSetFromMatrix(edata, pdata, ~strain)
#Below we can fit all the negative binomials at once 
glm_all_nb = DESeq(de)
results_nb = results(glm_all_nb)
hist(results_nb$stat)
