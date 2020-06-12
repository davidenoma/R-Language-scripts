tropical = c('darkorange','dodgerblue','hotpink','limegreen','yellow')
palette(tropical)
par(pch= 10)
library(devtools)
library(Biobase)
library(sva)
library(bladderbatch)
library(snpStats)

data("bladderdata")

pheno = pData(bladderEset)
edata = exprs(bladderEset)

mod = model.matrix(~ as.factor(cancer) + as.factor(batch),data = pheno)

fit = lm.fit(mod,t(edata))
hist(fit$coefficients[2,], col = 2 , breaks = 100)

table(pheno$cancer,pheno$batch)
  batch = pheno$batch
  modcombat = model.matrix(~1, data=pheno)
  modcancer = model.matrix(~cancer, data=pheno)
  
  combat_edata = ComBat(dat = edata,batch = batch,mod = modcombat,
                        par.prior = TRUE, prior.plots = TRUE)
  

combat_fit = lm.fit(modcancer, t(combat_edata))
hist(combat_fit$coefficients[2,],col = 2,breaks = 100)

plot(fit$coefficients[2,],combat_fit$coefficients[2,],col =2 ,
     xlab = 'Linear Model', ylab = 'Combat', xlim = c(-5,5), ylim=c(-5,5))
par(mfrow = c(1,1))
#Then making it a one by one plot 
plot(fit$coefficients[2,],combat_fit$coefficients[2,],col =2 ,
     xlab = 'Linear Model', ylab = 'Combat', xlim = c(-5,5), ylim=c(-5,5))
abline(c(0,1),col =1 ,lwd =3 )

#The previous program was when we know the batch variable 
#Given that we don't know the batch variable 
mod  = model.matrix(~cancer, data=pheno)
mod0 = model.matrix(~1, data = pheno)
#Surrogate batch effects estimate 
sva1 = sva(edata,mod,mod0,n.sv = 2)

names(sva1)
#Now we compare our batch effects with the estimated variables 
summary(lm(sva1$sv ~ pheno$batch))
boxplot(sva1$sv[,2] ~ pheno$batch)
points(sva1$sv[,2] ~ jitter(as.numeric(pheno$batch)),col = as.numeric(pheno$batch))

#In order words we defined our covariates that we now need to add 
#in our model. 


