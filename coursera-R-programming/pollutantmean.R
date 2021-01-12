#Dataset at https://d396qusza40orc.cloudfront.net/rprog%2Fdata%2Fspecdata.zip

pollution_data = read.csv("C:/Users/kora/Downloads/programs/specdata")

pollutant_mean <- function(directory, pollutant, id = 1:332){
  combine_pol_data <- c()
  for (i in id){
    num = as.character(i)
      if (nchar(num) == 1 ){
        num = paste("00",num,sep="")
    } else if (nchar(num) == 2){
      num = paste("0",num,sep="")
    }
    
    pol_data = read.csv(paste(directory,"/",num,".csv",sep = ""))
    important_data = pol_data[pollutant]
    # print(pol_data)
    # combine_pol_data = rbind(pol_data)
    combine_pol_data <- c(combine_pol_data,important_data[!is.na(important_data)])
  }
  means = mean(combine_pol_data, na.rm = TRUE)
  means
 
}

