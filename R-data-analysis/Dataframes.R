

# Data frames -------------------------------------------------------------

# Lets create data frame. Temperature of Texas
data1 = data.frame(State = "Texas", Temperature = 67.9)
# Check class of data1



# Now read temperature by state data from csv file
data2 = read.csv("3-7 data.csv")
data2
# Check number of rows and columns. Use nrow() and ncol()


# Combine data1 and data2 by row - assign result to "data"
# TIP: rbind()
data = 


# Change Colorado temperature to 47. Use row index - 7


# Assign "temp" variable with temperatures from data
temp = 
class(temp)

# Convert "temp" to celsius. Formula C = (F - 32)/1.8
# Assign to temp_celsius
temp_celsius = 

# Now add new column to "data" data frame. Call it "Temperature_C"
data = 

# Rename second column from "Temperature" to "Temperature_F"
# TIP: colnames() function


# Subset "data" for "Temperature_F" being > 65


# Subset "data" for lowest temperature. Print full line
# TIP: subset() and min() will help











# Lists -------------------------------------------------------------------

# Create list named "Texas" with Temperature_F and Temperature_C elements
# Use values from "data" variable
Texas = 

# Do same for "Alabama" with Alabama temperature data
Alabama = 

# Subset "Texas" for first list element
# Result should still be list!


# Subset "Texas" for first list value
# Now result should be numeric
  
  
# Change Texas Temperature_F to 59


# Length of a list - same as for vectors!
length(Texas)


# Combine "Texas" and "Alabama" variables to one list.
# List of two elements "Texas" and "Alabama" with same names
countries = 

# Extract temperature of Alabama in celsius from "countries"




