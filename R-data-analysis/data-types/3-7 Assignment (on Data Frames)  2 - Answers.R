

# Data frames -------------------------------------------------------------

# Create data frame. State and Temperature
data1 = data.frame(State = "Texas", Temperature = 67.9)
# Check class of data1
class(data1)


# Now read temperature by state data from csv file
data2 = read.csv("3-7 data.csv")
data2
# Check number of rows and columns. Use nrow() and ncol()
nrow(data2)
ncol(data2)

# Combine data1 and data2 by row - assign result to "data"
# TIP: rbind()
data = rbind(data1, data2)

# Change Colorado temperature to 47. Use row index - 7
data[7, "Temperature"] = 47
# or
data[data[, "State"] == "Colorado", "Temperature"] = 47


# Assign "temp" variable with temperatures from data
temp = data[, "Temperature"]
class(temp)

# Convert "temp" to celsius. Formula C = (F - 32)/1.8
# Assign to temp_celsius
temp_celsius = (temp - 32)/1.8

# Now add new column to "data" data frame. Call it "Temperature_C"
data = cbind(data, Temperature_C = temp_celsius)

# Rename second column from "Temperature" to "Temperature_F"
# TIP: colnames() function
colnames(data)[2] = "Temperature_F"

# Subset "data" for "Temperature_F" being > 65
subset(data, Temperature_F > 65)

# Subset "data" for lowest temperature. Print full line
# TIP: subset() and min() will help
subset(data, Temperature_F == min(data[, "Temperature_F"]))










# Lists -------------------------------------------------------------------

# Create list named "Texas" with Temperature_F and Temperature_C elements
# Use values from "data" variable
Texas = list(Temperature_F = 67.9, Temperature_C = 19.94) 

# Do same for "Alabama" with Alabama temperature data
Alabama = list(Temperature_F = 62.8, Temperature_C = 17.11) 

# Subset "Texas" for first list element
# Result should still be list!
Texas[1]

# Subset "Texas" for first list value
# Now result should be numeric
Texas[[1]]

# Change Texas Temperature_F to 59
Texas[["Temperature_F"]] = 59

# Length of a list - same as for vectors!
length(Texas)

# Combine "Texas" and "Alabama" variables to one list.
# List of two elements "Texas" and "Alabama" with same names
countries = list(Texas = Texas, Alabama = Alabama)

# Extract names of 

# Extract temperature of Alabama in celsius from "countries"
countries[["Alabama"]][["Temperature_C"]]



