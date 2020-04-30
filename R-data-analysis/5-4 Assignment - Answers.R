
# Assignment ------------------------------------------------------------
# data folder have 3 csv files. Write for loop to read them one by one
# Then calculate means of each column in each file
# Finally combine means of each column in each file into one data.frame

# Save file names into vector
files = c("data_part1.csv", "data_part2.csv",
          "data_part3.csv")
# Use paste() to add "data/" to each element from files
files = paste("data/", files, sep = "")

# Create dummy data.frame
data_all = data.frame(data_frame = "dummy.csv", weight = 1, 
                      Time = 1, Chick = 1, Diet = 1)


# Run for loop through files
for (f in files) {
  # Now calculate mean of each column(output is just a vector)
  data_part = read.csv(f, stringsAsFactors = F)
  
  # Now calculate mean of each column
  data_mean = apply(data_part, 2, mean) # will learn apply() soon
  
  # Create data frame from data_mean values
  data_mean_df = data.frame(data_frame = f, weight = data_mean[1],
                               Time = data_mean[2], Chick = data_mean[3], 
                               Diet = data_mean[4])

  # Combine data_all with data_mean_df
  data_all = rbind(data_all, data_mean_df)
}


# Finally remove first line from data_all
data_all = data_all[-1, ]
data_all