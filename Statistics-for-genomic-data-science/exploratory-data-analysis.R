tropical = c ('darkorange','dodgerblue','hotpink','limegreen','yellow')
palette(tropical)
library(knitr)

knit_hooks$set(setPch = function(before,options,envir){
  if(before) par(pch = 19)
})
opts_chunk$set(setPch = TRUE)

knitr::opts_chunk$set(fig.width = 5,fig.height = 5, size = "footnotesize", warning = FALSE, message = FALSE)

#look through the Markdown file for required libraries. 
con=url("http://bowtie-bio.sourceforge.net/recount/ExpressionSets/bodymap_eset.RData")
load(file=con)
ls()
close(con)
bm = bodymap.eset
pdata = pData(bm)
edata = exprs(bm)
fdata = fData(bm)
#Basic operation

table(pdata$gender)

table(pdata$gender, pdata$race)

summary(edata)

sum(pdata$age == ' ', na.rm = TRUE)

dim(edata)
is.na(edata)[1,]
#This sums all N/As and to check for NAs
sum(is.na(edata))

gene_na = rowSums(is.na(edata))
table(gene_na)
sampe_na = colSums(is.na(edata))
table(sample_na)
