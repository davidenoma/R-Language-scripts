tropical = c('darkorange','dodgerblue','hotpink','limegreen','yellow')
palette(tropical)
par(pch=17)

library(devtools)
library(Biobase)
library(limma)
library(edge)

con =url("http://bowtie-bio.sourceforge.net/recount/ExpressionSets/bottomly_eset.RData")
load(file=con)
close(con)
bot = bottomly.eset
pdata=pData(bot)
edata=as.matrix(exprs(bot))
fdata = fData(bot)
edata = log2(as.matrix(edata) + 1 )
edata = edata [rowMeans(edata) > 10, ]


#NOw we want to fit a thousand regression models. 
mod = model.matrix(~ pdata$strain)
fit = lm.fit(mod, t(edata))
names(fit)
length(fit)
#For a single model would have been 
lm(as.numeric(edata[1,]) ~ pdata$strain)
par(mfrow = c(1,2))

hist(fit$coefficients[1,], breaks = 100 , col=2 , xlab = 'Intercept')
hist(fit$coefficients[2,], breaks = 100 , col=3 , xlab = 'strain')

plot(fit$residuals[,1])

#we can fit adjusted modes as well 
mod_adj = model.matrix(~ pdata$strain + as.factor(pdata$lane.number))
fit_adj = lm.fit(mod_adj,t(edata))
fit_adj$coefficients[,1]

fit_limma = lmFit(edata,mod_adj)
names(fit_limma)
fit_limma$coefficients[1,]
fit_adj$coefficients[,1]

#The edge model fitting encapsulates the matrix and algebra complexity
edge_study = build_study(data = edata, grp = pdata$strain, adj.var = as.factor(pdata$lane.number))
#This returns an edge object 
fit_edge = fit_models(edge_study)
summary(fit_edge)

fit_edge@beta.coef[1,]
fit_limma$coefficients[1,]
