

# First example -----------------------------------------------------------
# "data 1.csv", "data 2.csv" represents Biochemical Oxygen Demand by time
# They overlap each other. Create function to do reading/merging

MergeDf = function(name1, name2) {
  
  # Read data
  data1 = read.csv(file = name1, stringsAsFactors = F)
  data2 = read.csv(file = name2, stringsAsFactors = F)
  # atspausdinti
  
  # Subset data2 to time values higher than in first data2
  data2 = subset(data2, Time > max(data1[, "Time"]))
  
  # Combine to final result
  data_full = rbind(data1, data2)
  
  return(data_full)
}

data_all = MergeDf("data 1.csv", "data 2.csv")





# Second example ----------------------------------------------------------

CalculateMean = function(arg) {
  
  if (class(arg) == "numeric") {
    
    average = mean(arg)
    return(average)
    
  } else if (class(arg) == "data.frame") {
    
    average = apply(data_all, 2, mean)
    return(average)
    
  } else {
    
    return(paste(class(arg), "data type is not supported."))
    
  }
  
}

CalculateMean(c(1,5, 2, 3.5))
CalculateMean(data_all)
CalculateMean("sdfasf")
