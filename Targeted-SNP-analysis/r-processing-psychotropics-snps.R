#Loading the control files.
snps <- read.csv('psychotropic-control.csv')
snps_expt <- read.csv('psycho-tropic-experimental.csv')


#Group the control SNPS
igbo_control <- snps[3:6]
hausa_control<-snps[names(snps)[10:14]]
yoruba_control<-snps[names(snps)[18:21]]

#Group the experimental group SNPs
igbo_expt <- snps_expt[names(snps_expt)[3:6]]
hausa_expt <- snps_expt[names(snps_expt)[10:14]]
yoruba_expt <- snps_expt[names(snps_expt)[18:21]]

#Further filter the SNP alleles
hausa_expt <-hausa_expt[4:13,1:4]
igbo_expt <-igbo_expt[4:13,1:4]
yoruba_expt <-yoruba_expt[4:13,1:4]

hau_ctrl<-hausa_control[1:10,1:4]
igbo_ctrl <- igbo_control[1:10,1:4]
yoruba_ctrl <- yoruba_control[1:10,1:4]

#Rename column names
colnames(yoruba_expt) <- c("rs2023239", "rs806378",  "rs806379",  "rs806381" )
colnames(igbo_expt) <- c("rs2023239", "rs806378",  "rs806379",  "rs806381" )
colnames(hausa_expt) <- c("rs2023239", "rs806378",  "rs806379",  "rs806381" )
colnames(yoruba_ctrl) <- c("rs2023239", "rs806378",  "rs806379",  "rs806381" )
colnames(igbo_ctrl) <- c("rs2023239", "rs806378",  "rs806379",  "rs806381" )
colnames(hau_ctrl) <- c("rs2023239", "rs806378",  "rs806379",  "rs806381" )

#Combining the controls and experimental groups
yor_entire<-rbind(yoruba_ctrl,yoruba_expt)
hau_entire<-rbind(hau_ctrl,hausa_expt)
igb_entire<-rbind(igbo_ctrl, igbo_expt)

#Combining all the snps independent of ethnicity 
all_snps<- rbind(rbind(yoruba_ctrl,yoruba_expt),rbind(hau_ctrl,hausa_expt),rbind(igbo_ctrl, igbo_expt))
rbind(yor_entire,hau_entire,igb_entire)
typeof(all_snps)
all_snps

nrow(all_snps)
# barplot(as.matrix(all_snps),
#         legend.text = row.names(all_snps),
#         args.legend = list(x = "right"),
#         col = c("blue","green","red"))
plot(all_snps,type="h")
table(all_snps)
unique(all_snps)

#installing packages to visualize tables. 
install.packages("data.table")
install.packages("dplyr")
install.packages("formattable")
install.packages("tidyr")
install.packages('xfun')
#Loading packages to visualize the tables
library(data.table)
library(dplyr)
library(formattable)
library(tidyr)

#View all the snps. 
formattable(all_snps)
data.table(all_snps)

install.packages('htmltools')
install.packages('webshot')
library(htmltools)
library(webshot)
webshot::install_phantomjs()
#Save the visualiztions of the SNPS
export_formattable <- function(f, file, width = "100%", height = NULL, 
                               background = "white", delay = 0.2)
{
  w <- as.htmlwidget(f, width = width, height = height)
  path <- html_print(w, background = background, viewer = NULL)
  url <- paste0("file:///", gsub("\\\\", "/", normalizePath(path)))
  webshot(url,
          file = file,
          selector = ".formattable_widget",
          delay = delay)
}

export_formattable(formattable(all_snps), "mysnps.png")

save(all_snps)

all_snps

#VIsualize frequency of all snps in cases and controls
require(c('ggplot','lattice'))
histogram(~as.factor(all_snps$rs2023239),xlab ="rs2023239")
qplot(all_snps$rs2023239,xlab ="rs2023239",ylab = 'count')
histogram(~as.factor(all_snps$rs806378),xlab ="rs806378")
qplot(all_snps$rs806378,xlab ="rs806378",ylab = 'count')

histogram(~as.factor(all_snps$rs806379),xlab ="rs806379")
qplot(all_snps$rs806379,xlab ="rs806379",ylab = 'count')

histogram(~as.factor(all_snps$rs806381),xlab ="rs806381")
qplot(all_snps$rs806381,xlab ="rs806381",ylab = 'count')

#Visualize snp frequency cases vs controls
#rs2023239
qplot(hau_ctrl$rs2023239,xlab = "Hausa Control for rs2023239")
summary(as.factor(hau_ctrl$rs806378))
qplot(hausa_expt$rs2023239,xlab = "Hausa Experimental for rs2023239")
summary(as.factor(hausa_expt$rs806378))
qplot(igbo_ctrl$rs2023239,xlab = "Igbo Control for rs2023239")
qplot(igbo_expt$rs2023239,xlab = "Igbo Experimental for rs2023239")

qplot(yoruba_ctrl$rs2023239,xlab = "Yoruba Control for rs2023239")
qplot(yoruba_expt$rs2023239,xlab = "Yoruba Experimental for rs2023239")

#rs806378
qplot(hau_ctrl$rs806378,xlab = "Hausa Control for rs806378")
qplot(hausa_expt$rs806378,xlab = "Hausa Experimental for rs806378")
qplot(igbo_ctrl$rs806378,xlab = "Igbo Control for rs806378")
qplot(igbo_expt$rs806378,xlab = "Igbo Experimental for rs806378")
qplot(yoruba_ctrl$rs806378,xlab = "Yoruba Control for rs806378")
qplot(yoruba_expt$rs806378,xlab = "Yoruba Experimental for rs806378")

#rs806379
qplot(hau_ctrl$rs806379,xlab = "Hausa Control for rs806379")
qplot(hausa_expt$rs806379,xlab = "Hausa Experimental for rs806379")
qplot(yoruba_ctrl$rs806379,xlab = "Yoruba Control for rs806379")
qplot(yoruba_expt$rs806379,xlab = "Yoruba Experimental for rs806379")
qplot(igbo_ctrl$rs806379,xlab = "Igbo Control for rs806379")
qplot(igbo_expt$rs806379,xlab = "Igbo Experimental for rs806379")

#rs806381
qplot(igbo_ctrl$rs806381,xlab = "Igbo Control for rs806381")





