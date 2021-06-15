#We need to inspect the file (you can use ls -S GDS3309.clean) to see what are the NULL characters and in general if the file "looks good"

raw.data <- read.table("GDS4069.clean", sep="\t", header=TRUE)
dim(raw.data)
#The first two columns contain the probe ID and the gene name for each row. 
#Thus, we need to create a file that does not contain these two columns.
data <- raw.data[,-c(1,2)]  
head(data)
dim(data)
boxplot(data)

data2 <- log2(data) ## again remember that most of the operations in R are performed in an element-wise manner. 

##now let's make a boxplot
boxplot(data2, main=expression(paste("Boxplot of the ", log2, " data")))
