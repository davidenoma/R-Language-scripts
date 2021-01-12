corr <- function(directory, threshold=0){
  all <- complete(directory)
  
  if (sum(which(all$nobs>threshold))==0){
    
    NULL
  }
  
  else {
    thresh_index <- as.numeric(all[which(all$nobs>threshold),]$id)
    
    names <- list.files(directory)[thresh_index]
    read <- lapply(paste(directory,"/",names, sep = ""),read.csv)
    
    
    
    return(unlist(lapply(read, function(x){cor(x[,2],x[,3],use="pairwise.complete.obs")})))
  }
}