#Dataset at https://d396qusza40orc.cloudfront.net/rprog%2Fdata%2Fspecdata.zip

complete <- function(directory, id = 1:332){
  results = data.frame(id = numeric(0), nobs = numeric(0))
  for (i in id){
    num = as.character(i)
    if (nchar(num) == 1 ){
      num = paste("00",num,sep="")
    } else if (nchar(num) == 2){
      num = paste("0",num,sep="")
    }
    pol_data = read.csv(paste(directory,"/",num,".csv",sep = ""))
    important_data <- pol_data[(!is.na(pol_data$sulfate)), ]
    important_data <- important_data[(!is.na(important_data$nitrate)), ]
    nobs <- nrow(important_data)
    results <- rbind(results, data.frame(id=i, nobs=nobs))
    
  }
  results
  
}