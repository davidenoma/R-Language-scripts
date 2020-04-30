

# Example 1 ---------------------------------------------------------------
# for (i in values) {
#   put some R code here
# }
for (i in 1:10) {
  print(i^2)
}




# Example 2 ---------------------------------------------------------------
# Directly by values
vec = c("first", "second", "third")
for (a in vec) {
  print(a)
}
# Using indexes instead
vec = c("first", "second", "third")
for (a in 1:length(vec)) {
  print(vec[a])
}




# Example 3 ---------------------------------------------------------------
# Reminder of lists in combination with for loops
John = list(Name = "John", Surname = "Smith", Age = 35)
# First - directly
for (a in John) {
  print(a)
}
# Second - by indexing
for (a in 1:length(John)) {
  print(John[[a]])
}
# Third - by element names
for (a in names(John)) {
  print(John[[a]])
}





# Example 4 ---------------------------------------------------------------
# Reading multiple .csv files and combining them into one data.frame
files = c("data_part1.csv", "data_part2.csv",
          "data_part3.csv", "data_part4.csv")
files = paste("data/", files, sep = "")

# Create dummy data.frame
data_all = data.frame(weight = 1, Time = 1, Chick = 1, Diet = 1)

# Running for loop to read file by file
for (f in files) {
  data_part = read.csv(f, stringsAsFactors = F)
  if (ncol(data_part) == 4) {
    data_all = rbind(data_all, data_part)
  }
}


