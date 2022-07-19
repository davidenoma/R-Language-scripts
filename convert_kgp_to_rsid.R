library(biomaRt)
library(stringr)
## Use the default ENSEMBL Variation Mart & Human dataset
snpMart = useEnsembl(biomart = "snps",dataset = "hsapiens_snp")
## Create an example set of coordinates as a dataframe
SNP_M <- data.frame(CHR = c(1,1), START = c(10020, 10039), END = c(10020, 10039))
gen_cord = read.csv("C:/Users/HP/OneDrive/Desktop/genomicCoordinates.txt",sep=" ",header = FALSE)
unique_chroms = unique(gen_cord$V1)

  for (variable in unique_chroms) {
    if (nchar (variable) == 4) {
       gen_cord$V1[gen_cord$V1 == variable] <- str_sub(variable,-1,-1)
      
    }else{
      gen_cord$V1[gen_cord$V1 == variable] <- str_sub(variable,-2,-1)
    }
    
  }
coords <- apply(gen_cord, 1, paste, collapse = ":")

final_cord <- getBM(attributes = c('refsnp_id', 'chr_name', 'chrom_start', 'chrom_end', 'allele'),
      filters = c('chromosomal_region'), 
      values = coords,
      verbose = TRUE,
      mart = snpMart) 
