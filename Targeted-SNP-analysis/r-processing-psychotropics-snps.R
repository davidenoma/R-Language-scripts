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

# 
# yoruba_control[4:13,]
# igbo_control[4:13,]
# hausa_control[4:13,]

cbind(hausa_expt[4:13,],igbo_expt[4:13,],yoruba_expt[4:13,])


hau_ctrl<-hausa_control[1:10,1:4]
hau_ctrl
igbo_ctrl <- igbo_control[1:10,1:4]
igbo_ctrl
yoruba_ctrl <- yoruba_control[1:10,1:4]
yoruba_ctrl

#Rename column names
colnames(yoruba_expt) <- c("rs2023239", "rs806378",  "rs806379",  "rs806381" )
colnames(igbo_expt) <- c("rs2023239", "rs806378",  "rs806379",  "rs806381" )
colnames(hausa_expt) <- c("rs2023239", "rs806378",  "rs806379",  "rs806381" )
colnames(yoruba_ctrl) <- c("rs2023239", "rs806378",  "rs806379",  "rs806381" )
colnames(igbo_ctrl) <- c("rs2023239", "rs806378",  "rs806379",  "rs806381" )
colnames(hau_ctrl) <- c("rs2023239", "rs806378",  "rs806379",  "rs806381" )


na.omit(rbind(igbo_control,igbo_expt))
# rbind(igbo_control,igbo_expt))[,26]
# rbind(igbo_control,igbo_expt)[,26]
# rbind(igbo_control,igbo_expt)[26,]
# rbind(igbo_control,igbo_expt)[26,1]
typeof(rbind(igbo_ctrl,igbo_expt)[26,1])

#Combining the controls and experimental groups
rbind(yoruba_ctrl,yoruba_expt)
rbind(hau_ctrl,hausa_expt)
rbind(igbo_ctrl, igbo_expt )






